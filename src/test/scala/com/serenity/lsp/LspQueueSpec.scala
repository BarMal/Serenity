package com.serenity.lsp

import java.nio.file.Files

import scala.concurrent.duration.*

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.keystroke.events.*
import com.serenity.lsp.config.LanguageId
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class LspQueueSpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private def makeStateManager(): StateManager =
    val logger = LoggerFactory[IO].getLogger(using LoggerName("LspQueueSpec"))
    StateManager.apply(logger).unsafeRunSync()

  private def executeCommandThroughRunner(stateManager: StateManager, searchTerm: String): Unit =
    stateManager.applyEvent(ToggleCommandRunner).unsafeRunSync()
    searchTerm.foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())
    stateManager.applyEvent(Enter).unsafeRunSync()

  private def setBufferLanguageThroughRunner(stateManager: StateManager, searchTerm: String, submenuIndex: Int): Unit =
    stateManager.applyEvent(ToggleCommandRunner).unsafeRunSync()
    searchTerm.foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())
    stateManager.applyEvent(Enter).unsafeRunSync()
    (0 until submenuIndex).foreach(_ => stateManager.applyEvent(MoveDown).unsafeRunSync())
    stateManager.applyEvent(Enter).unsafeRunSync()

  "lspEffectStream" should "emit FileOpened when a Scala file is loaded" in {
    val sm       = makeStateManager()
    val tempFile = Files.createTempFile("test-lsp", ".scala")
    Files.writeString(tempFile, "object Foo")
    try
      sm.applyEvent(LoadFile(tempFile)).unsafeRunSync()

      val effects = sm.lspEffectStream
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
          text shouldBe "object Foo"
        case other => fail(s"Expected FileOpened, got $other")
    finally
      Files.deleteIfExists(tempFile)
      sm.applyEvent(Quit).unsafeRunSync()
  }

  it should "emit FileClosed when a buffer with a file path is closed" in {
    val sm       = makeStateManager()
    val tempFile = Files.createTempFile("test-lsp-close", ".scala")
    Files.writeString(tempFile, "object Bar")
    try
      sm.applyEvent(LoadFile(tempFile)).unsafeRunSync()

      val fileOpened = sm.lspEffectStream
        .take(1)
        .timeout(2.seconds)
        .compile
        .toList
        .unsafeRunSync()

      fileOpened should have size 1

      val bufferId = sm.getCurrentState
        .unsafeRunSync()
        .buffers
        .values
        .find(_.filePath.contains(tempFile))
        .map(_.id)

      bufferId shouldBe defined
      sm.closeBuffer(bufferId.get).unsafeRunSync()

      val fileClosed = sm.lspEffectStream
        .take(1)
        .timeout(2.seconds)
        .compile
        .toList
        .unsafeRunSync()

      fileClosed should have size 1
      fileClosed.head match
        case LspEffect.FileClosed(uri, lang) =>
          lang shouldBe LanguageId.Scala
        case other => fail(s"Expected FileClosed, got $other")
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

      val effects = sm.lspEffectStream
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

      sm.lspEffectStream.take(1).timeout(2.seconds).compile.toList.unsafeRunSync() should have size 1

      setBufferLanguageThroughRunner(sm, "lang-markdown", submenuIndex = 13)

      val effects = sm.lspEffectStream
        .take(2)
        .timeout(2.seconds)
        .compile
        .toList
        .unsafeRunSync()

      effects should have size 2
      effects.head shouldBe LspEffect.FileClosed(tempFile.toUri.toString, LanguageId.Scala)
      effects(1) shouldBe LspEffect.FileOpened(tempFile.toUri.toString, LanguageId.Markdown, "object Baz")
    finally
      Files.deleteIfExists(tempFile)
      sm.applyEvent(Quit).unsafeRunSync()
  }
