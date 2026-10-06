package com.serenity.lsp

import java.nio.file.Files

import scala.concurrent.duration.*

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.command.*
import com.serenity.keystroke.events.*
import com.serenity.lsp.config.LanguageId
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.manager.StateManagerTestFacade.*
import com.serenity.testkit.{RopeText, SharedDictionary}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class LspQueueSpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private def makeStateManager(): StateManager =
    val logger = LoggerFactory[IO].getLogger(using LoggerName("LspQueueSpec"))
    StateManager.apply(logger, dictionaryCache = SharedDictionary.default).unsafeRunSync()

  private def setBufferLanguage(stateManager: StateManager, language: LanguageId): Unit =
    stateManager
      .executeCommand(
        Command.typed(
          s"lang-${language.id}",
          s"Use ${language.displayName} mode for the current buffer.",
          CommandIntent.File(FileIntent.SetBufferLanguage(Some(language))),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()

  "lspEffectStream" should "emit FileOpened when a Scala file is loaded" in {
    val sm       = makeStateManager()
    val tempFile = Files.createTempFile("test-lsp", ".scala")
    Files.writeString(tempFile, "object Foo")
    try
      sm.applyEvent(LoadFile(tempFile)).unsafeRunSync()

      val effects = sm.lspEffectSource.lspEffectStream
        .take(1)
        .timeout(2.seconds)
        .compile
        .toList
        .unsafeRunSync()

      effects should have size 1
      effects.head match
        case LspEffect.FileOpened(uri, lang, text) =>
          uri should include("test-lsp")
          lang shouldBe LanguageId.Scala
          text.collect() shouldBe "object Foo"
        case other => fail(s"Expected FileOpened, got $other")
    finally
      Files.deleteIfExists(tempFile)
      sm.applyEvent(Quit).unsafeRunSync()
  }

  it should "emit FileClosed when the tab holding an LSP document is closed" in {
    val sm       = makeStateManager()
    val tempFile = Files.createTempFile("test-lsp-close-tab", ".scala")
    Files.writeString(tempFile, "object Closed")
    try
      sm.applyEvent(LoadFile(tempFile)).unsafeRunSync()
      sm.lspEffectSource.lspEffectStream.take(1).timeout(2.seconds).compile.drain.unsafeRunSync()

      sm.applyEvent(CloseTab).unsafeRunSync()

      val stillOpen =
        sm.getCurrentState.unsafeRunSync().persisted.buffers.values.exists(_.document.filePath.contains(tempFile))
      stillOpen shouldBe false
      sm.lspEffectSource.lspEffectStream.take(1).timeout(2.seconds).compile.toList.unsafeRunSync() shouldBe List(
        LspEffect.FileClosed(tempFile.toUri.toString, LanguageId.Scala)
      )
    finally
      Files.deleteIfExists(tempFile)
      sm.applyEvent(Quit).unsafeRunSync()
  }

  it should "not emit FileOpened for files without a known language" in {
    val sm       = makeStateManager()
    val tempFile = Files.createTempFile("test-lsp-unknown", ".xyz")
    Files.writeString(tempFile, "some content")
    try
      sm.applyEvent(LoadFile(tempFile)).unsafeRunSync()

      val effects = sm.lspEffectSource.lspEffectStream
        .interruptAfter(300.millis)
        .compile
        .toList
        .unsafeRunSync()

      effects shouldBe empty
    finally
      Files.deleteIfExists(tempFile)
      sm.applyEvent(Quit).unsafeRunSync()
  }

  it should "rebind the LSP stream when the buffer language changes" in {
    val sm       = makeStateManager()
    val tempFile = Files.createTempFile("test-lsp-rebind", ".scala")
    Files.writeString(tempFile, "object Baz")
    try
      sm.applyEvent(LoadFile(tempFile)).unsafeRunSync()

      sm.lspEffectSource.lspEffectStream.take(1).timeout(2.seconds).compile.toList.unsafeRunSync() should have size 1

      setBufferLanguage(sm, LanguageId.Markdown)

      val effects = sm.lspEffectSource.lspEffectStream
        .take(2)
        .timeout(2.seconds)
        .compile
        .toList
        .unsafeRunSync()

      effects should have size 2
      effects.head shouldBe LspEffect.FileClosed(tempFile.toUri.toString, LanguageId.Scala)
      effects(1) shouldBe LspEffect.FileOpened(tempFile.toUri.toString, LanguageId.Markdown, RopeText("object Baz"))
    finally
      Files.deleteIfExists(tempFile)
      sm.applyEvent(Quit).unsafeRunSync()
  }

  it should "emit the latest full-text change with the next document version" in {
    val sm       = makeStateManager()
    val tempFile = Files.createTempFile("test-lsp-change", ".scala")
    Files.writeString(tempFile, "object Change")
    try
      sm.applyEvent(LoadFile(tempFile)).unsafeRunSync()
      val opened = sm.lspEffectSource.lspEffectStream.take(1).timeout(2.seconds).compile.toList.unsafeRunSync()

      sm.applyEvent(InsertChar('a')).unsafeRunSync()
      sm.applyEvent(InsertChar('b')).unsafeRunSync()
      sm.applyEvent(InsertChar('c')).unsafeRunSync()

      val effects =
        opened ++ sm.lspEffectSource.lspEffectStream.take(1).timeout(2.seconds).compile.toList.unsafeRunSync()
      val currentText =
        sm.getCurrentState
          .unsafeRunSync()
          .persisted
          .buffers
          .values
          .find(_.document.filePath.contains(tempFile))
          .map(_.document.content)

      currentText shouldBe defined
      effects.head shouldBe LspEffect.FileOpened(tempFile.toUri.toString, LanguageId.Scala, RopeText("object Change"))
      effects(1) shouldBe LspEffect.FileChanged(tempFile.toUri.toString, LanguageId.Scala, currentText.get, 2)
    finally
      Files.deleteIfExists(tempFile)
      sm.applyEvent(Quit).unsafeRunSync()
  }

  it should "not emit document changes for cursor-only events" in {
    val sm       = makeStateManager()
    val tempFile = Files.createTempFile("test-lsp-no-change", ".scala")
    Files.writeString(tempFile, "object Still")
    try
      sm.applyEvent(LoadFile(tempFile)).unsafeRunSync()
      sm.lspEffectSource.lspEffectStream.take(1).timeout(2.seconds).compile.drain.unsafeRunSync()

      sm.applyEvent(MoveRight).unsafeRunSync()

      sm.lspEffectSource.lspEffectStream.interruptAfter(300.millis).compile.toList.unsafeRunSync() shouldBe empty
    finally
      Files.deleteIfExists(tempFile)
      sm.applyEvent(Quit).unsafeRunSync()
  }

  it should "coalesce rapid changes without blocking editor input when no consumer is running" in {
    val sm       = makeStateManager()
    val tempFile = Files.createTempFile("test-lsp-stalled", ".scala")
    Files.writeString(tempFile, "object Stalled")
    try
      sm.applyEvent(LoadFile(tempFile)).unsafeRunSync()
      val opened = sm.lspEffectSource.lspEffectStream.take(1).timeout(2.seconds).compile.toList.unsafeRunSync()

      (1 to 600).foreach(_ => sm.applyEvent(InsertChar('x')).unsafeRunSync())

      val effects =
        opened ++ sm.lspEffectSource.lspEffectStream.take(1).timeout(2.seconds).compile.toList.unsafeRunSync()
      val currentText =
        sm.getCurrentState
          .unsafeRunSync()
          .persisted
          .buffers
          .values
          .find(_.document.filePath.contains(tempFile))
          .map(_.document.content)

      effects should have size 2
      currentText shouldBe defined
      effects(1) shouldBe LspEffect.FileChanged(tempFile.toUri.toString, LanguageId.Scala, currentText.get, 2)
    finally
      Files.deleteIfExists(tempFile)
      sm.applyEvent(Quit).unsafeRunSync()
  }

  it should "open the saved location after save as changes the LSP document URI" in {
    val sm     = makeStateManager()
    val source = Files.createTempFile("test-lsp-save-as-source", ".scala")
    val target = Files.createTempFile("test-lsp-save-as-target", ".md")
    Files.writeString(source, "object Saved")
    try
      sm.applyEvent(LoadFile(source)).unsafeRunSync()
      sm.lspEffectSource.lspEffectStream.take(1).timeout(2.seconds).compile.drain.unsafeRunSync()
      val bufferId =
        sm.getCurrentState.unsafeRunSync().persisted.buffers.values.find(_.document.filePath.contains(source)).map(_.id)

      bufferId shouldBe defined
      sm.fileService.saveBufferAs(bufferId.get, target).unsafeRunSync()

      sm.lspEffectSource.lspEffectStream.take(2).timeout(2.seconds).compile.toList.unsafeRunSync() shouldBe List(
        LspEffect.FileClosed(source.toUri.toString, LanguageId.Scala),
        LspEffect.FileOpened(target.toUri.toString, LanguageId.Markdown, RopeText("object Saved"))
      )
    finally
      Files.deleteIfExists(source)
      Files.deleteIfExists(target)
      sm.applyEvent(Quit).unsafeRunSync()
  }
