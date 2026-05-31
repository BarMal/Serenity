package com.serenity.lsp

import java.nio.file.Files

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.keystroke.events.{LoadFile, Quit}
import com.serenity.lsp.config.LanguageId
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

import scala.concurrent.duration.*

class LspQueueSpec extends AnyFlatSpec with Matchers:

  given Balance            = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private def makeStateManager(): StateManager =
    val logger = LoggerFactory[IO].getLogger(using LoggerName("LspQueueSpec"))
    StateManager.apply(logger).unsafeRunSync()

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
          uri  should include("test-lsp")
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

      val bufferId = sm.getCurrentState.unsafeRunSync()
        .buffers.values
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
