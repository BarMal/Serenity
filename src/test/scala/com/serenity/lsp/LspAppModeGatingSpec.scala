package com.serenity.lsp

import java.nio.file.Files
import java.util.concurrent.TimeoutException

import scala.concurrent.duration.*

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.command.*
import com.serenity.config.AppMode
import com.serenity.lsp.config.LanguageId
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.models.BufferId
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

/** Prose-mode workspaces have no code to analyse, so switching a buffer's language must not spawn an LSP connection
  * (issue #1296) -- even though the same buffer would happily get one in code mode.
  */
class LspAppModeGatingSpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private def createStateManager(): StateManager =
    val logger = LoggerFactory[IO].getLogger(using LoggerName("LspAppModeGatingSpec"))
    StateManager.apply(logger).unsafeRunSync()

  private def setBufferPath(stateManager: StateManager, file: java.nio.file.Path): Unit =
    stateManager
      .updateState { state =>
        val original = state.persisted.buffers(BufferId(0))
        val buffer   = original.copy(document = original.document.copy(filePath = Some(file)))
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers + (BufferId(0) -> buffer)))
      }
      .unsafeRunSync()

  private def setLanguage(stateManager: StateManager, language: LanguageId): Unit =
    stateManager
      .executeCommand(
        Command.typed(
          "set-buffer-language-scala",
          "Set buffer language to Scala",
          CommandIntent.File(FileIntent.SetBufferLanguage(Some(language))),
          CommandCategory.Edit
        )
      )
      .unsafeRunSync()

  "Setting a buffer's language in code mode" should "enqueue an LSP FileOpened effect" in {
    val stateManager = createStateManager()
    val file         = Files.createTempFile("lsp-mode-gating-code", ".scala")
    try
      setBufferPath(stateManager, file)
      setLanguage(stateManager, LanguageId.Scala)

      val effect = stateManager.lspEffectStream.take(1).compile.lastOrError.timeout(3.seconds).unsafeRunSync()
      effect shouldBe LspEffect.FileOpened(file.toUri.toString, LanguageId.Scala, "")
    finally Files.deleteIfExists(file): Unit
  }

  "Setting a buffer's language in prose mode" should "not enqueue an LSP FileOpened effect" in {
    val stateManager = createStateManager()
    val file         = Files.createTempFile("lsp-mode-gating-prose", ".scala")
    try
      stateManager
        .executeCommand(
          Command.typed(
            "app-mode-prose",
            "Switch to prose mode",
            CommandIntent.View(ViewIntent.SetAppMode(AppMode.Prose)),
            CommandCategory.Settings
          )
        )
        .unsafeRunSync()
      setBufferPath(stateManager, file)
      setLanguage(stateManager, LanguageId.Scala)

      val outcome =
        stateManager.lspEffectStream.take(1).compile.lastOrError.timeout(500.millis).attempt.unsafeRunSync()
      outcome.left.map(_.getClass) shouldBe Left(classOf[TimeoutException])
    finally Files.deleteIfExists(file): Unit
  }
