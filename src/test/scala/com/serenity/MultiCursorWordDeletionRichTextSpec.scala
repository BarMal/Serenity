package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.command.CommandRegistry
import com.serenity.keystroke.events.DeleteWordBackward
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.models.{CursorPosition, PaneId, Selection}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

/** Regression coverage for the drift `#1072` flagged: `applyMergedDeletionEdits` (reached from a multi-cursor
  * `DeleteWordBackward`/`DeleteWordForward`/`DeleteBackward`/`DeleteForward` with no active selection) silently kept
  * the buffer's pre-edit `richTextDocument`, unlike every sibling edit path, which either updates it
  * (`applyTrackedEdits`) or has none to update. Once the plain-text content changed under it, the stale document no
  * longer matched -- `RichTextDocument.matchesPlainText` -- and every subsequent rich-text edit at that buffer would
  * silently stop applying (`richTextDocumentAfterEdit` no-ops whenever the plain text has drifted).
  */
class MultiCursorWordDeletionRichTextSpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private def createStateManager(): StateManager =
    val logger = LoggerFactory[IO].getLogger(using LoggerName("MultiCursorWordDeletionRichTextSpec"))
    StateManager.apply(logger).unsafeRunSync()

  "a merged multi-cursor word deletion" should "keep richTextDocument in sync with the edited content" in {
    val stateManager = createStateManager()
    val bufferId     = stateManager.createBuffer("alpha beta gamma").unsafeRunSync()
    stateManager.setBufferForPane(PaneId(0), bufferId).unsafeRunSync()

    // Populate richText.richTextDocument the same way any authoring session would: format a selection.
    val boldSelection = Selection(CursorPosition(0, 6), CursorPosition(0, 10))
    stateManager
      .updateState { state =>
        state.copy(persisted =
          state.persisted.copy(
            buffers = state.persisted.buffers.updated(
              bufferId,
              state.persisted
                .buffers(bufferId)
                .copy(editing =
                  state.persisted
                    .buffers(bufferId)
                    .editing
                    .copy(
                      selection = Some(boldSelection),
                      cursors = List(boldSelection.focus)
                    )
                )
            )
          )
        )
      }
      .unsafeRunSync()
    val boldCommand = CommandRegistry.withToggleUI.findCommand("bold").getOrElse(fail("missing bold"))
    stateManager.executeCommand(boldCommand).unsafeRunSync()

    stateManager.getCurrentState.unsafeRunSync().persisted.buffers(bufferId).richText.richTextDocument shouldBe defined

    // Two cursors, no selection, one word before each -- routes through applyMultiCursorWordDeletion ->
    // applyMergedDeletionEdits.
    stateManager
      .updateState { state =>
        state.copy(persisted =
          state.persisted.copy(
            buffers = state.persisted.buffers.updated(
              bufferId,
              state.persisted
                .buffers(bufferId)
                .copy(editing =
                  state.persisted
                    .buffers(bufferId)
                    .editing
                    .copy(
                      selection = None,
                      selections = Nil,
                      cursors = List(CursorPosition(0, 5), CursorPosition(0, 17))
                    )
                )
            )
          )
        )
      }
      .unsafeRunSync()

    stateManager.applyEvent(DeleteWordBackward).unsafeRunSync()

    val buffer       = stateManager.getCurrentState.unsafeRunSync().persisted.buffers(bufferId)
    val plainContent = buffer.document.content.collect()
    plainContent should not be "alpha beta gamma"

    buffer.richText.richTextDocument shouldBe defined
    buffer.richText.richTextDocument.map(_.plainText) shouldBe Some(plainContent)
  }
