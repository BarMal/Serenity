package com.serenity.lsp

import java.nio.file.Files

import scala.concurrent.duration.*

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.command.*
import com.serenity.lsp.config.LanguageId
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.models.{BufferId, CursorPosition}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class LspCommandRoutingSpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private def createStateManager(): StateManager =
    val logger = LoggerFactory[IO].getLogger(using LoggerName("LspCommandRoutingSpec"))
    StateManager.apply(logger).unsafeRunSync()

  "LSP command routing" should "register hover, completion, and definition commands" in {
    val commandNames = CommandRegistry.default.getAllCommands.map(_.name)

    commandNames should contain("lsp-hover")
    commandNames should contain("lsp-completion")
    commandNames should contain("lsp-definition")
  }

  it should "enqueue a hover request for the active language buffer" in {
    val stateManager = createStateManager()
    val file         = Files.createTempFile("lsp-hover", ".scala")
    try
      stateManager
        .updateState { state =>
          val original = state.persisted.buffers(BufferId(0))
          val buffer = original.copy(
            document = original.document.copy(filePath = Some(file), language = Some(LanguageId.Scala)),
            editing = original.editing.copy(cursors = List(CursorPosition(3, 7)))
          )
          state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers + (BufferId(0) -> buffer)))
        }
        .unsafeRunSync()

      stateManager
        .executeCommand(
          Command.typed(
            "lsp-hover",
            "Show LSP hover information.",
            CommandIntent.RequestLspHover,
            CommandCategory.Edit
          )
        )
        .unsafeRunSync()

      val effect = stateManager.lspEffectStream.take(1).compile.lastOrError.timeout(3.seconds).unsafeRunSync()
      effect shouldBe LspEffect.HoverRequested(
        uri = file.toUri.toString,
        languageId = LanguageId.Scala,
        line = 3,
        character = 7,
        anchor = CursorPosition(3, 7)
      )
    finally Files.deleteIfExists(file)
  }

  it should "enqueue a definition request with the word under the active cursor" in {
    val stateManager = createStateManager()
    val file         = Files.createTempFile("lsp-definition", ".scala")
    try
      stateManager
        .updateState { state =>
          val original = state.persisted.buffers(BufferId(0))
          val buffer = original.copy(
            document = original.document.copy(
              content = com.serenity.rope.Rope("val total = subtotal + 1"),
              filePath = Some(file),
              language = Some(LanguageId.Scala)
            ),
            editing = original.editing.copy(cursors = List(CursorPosition(0, 14)))
          )
          state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers + (BufferId(0) -> buffer)))
        }
        .unsafeRunSync()

      stateManager
        .executeCommand(
          Command.typed(
            "lsp-definition",
            "Go to the symbol definition.",
            CommandIntent.RequestLspDefinition,
            CommandCategory.Edit
          )
        )
        .unsafeRunSync()

      val effect = stateManager.lspEffectStream.take(1).compile.lastOrError.timeout(3.seconds).unsafeRunSync()
      effect shouldBe LspEffect.DefinitionRequested(
        uri = file.toUri.toString,
        languageId = LanguageId.Scala,
        line = 0,
        character = 14,
        anchor = CursorPosition(0, 14),
        symbol = "subtotal"
      )
    finally Files.deleteIfExists(file)
  }

  it should "enqueue a completion request for the active language buffer" in {
    val stateManager = createStateManager()
    val file         = Files.createTempFile("lsp-completion", ".scala")
    try
      stateManager
        .updateState { state =>
          val original = state.persisted.buffers(BufferId(0))
          val buffer = original.copy(
            document = original.document.copy(filePath = Some(file), language = Some(LanguageId.Scala)),
            editing = original.editing.copy(cursors = List(CursorPosition(2, 5)))
          )
          state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers + (BufferId(0) -> buffer)))
        }
        .unsafeRunSync()

      stateManager
        .executeCommand(
          Command.typed(
            "lsp-completion",
            "Request LSP completion candidates.",
            CommandIntent.RequestLspCompletion,
            CommandCategory.Edit
          )
        )
        .unsafeRunSync()

      val effect = stateManager.lspEffectStream.take(1).compile.lastOrError.timeout(3.seconds).unsafeRunSync()
      effect shouldBe LspEffect.CompletionRequested(
        uri = file.toUri.toString,
        languageId = LanguageId.Scala,
        line = 2,
        character = 5,
        anchor = CursorPosition(2, 5)
      )
    finally Files.deleteIfExists(file)
  }
