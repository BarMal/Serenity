package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.command.CommandRegistry
import com.serenity.keystroke.events.{Cut, Paste, ReverseTabKey, TabKey}
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.models.{CursorPosition, PaneId, Selection}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

/** Regression coverage for issue #1291: saving a formatted document loses formatting on export. `#1072` centralised
  * `richTextDocument` remapping behind `Buffer.withEditedContent`, but four edit paths never adopted it -- a
  * single-cursor paste, Tab-to-indent, Shift+Tab-to-unindent, and a no-selection line Cut all mutate
  * `document.content` while leaving `richText.richTextDocument` untouched or stale. Once the two drift,
  * `RichTextDocument.matchesPlainText` fails permanently for that buffer, and every later save -- RTF, DOCX, ODT, or
  * Markdown alike -- falls back to `RichTextDocument.fromPlainText`, silently discarding every mark, heading and
  * alignment the user had applied.
  */
class RichTextEditPathDesyncSpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private def createStateManager(): StateManager =
    val logger = LoggerFactory[IO].getLogger(using LoggerName("RichTextEditPathDesyncSpec"))
    StateManager.apply(logger).unsafeRunSync()

  private def boldSelection(stateManager: StateManager, bufferId: com.serenity.state.models.BufferId, selection: Selection): Unit =
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
                    .copy(selection = Some(selection), selections = Nil, cursors = List(selection.focus))
                )
            )
          )
        )
      }
      .unsafeRunSync()
    val boldCommand = CommandRegistry.withToggleUI.findCommand("bold").getOrElse(fail("missing bold"))
    stateManager.executeCommand(boldCommand).unsafeRunSync()

  private def setCursorAndSelection(
    stateManager: StateManager,
    bufferId: com.serenity.state.models.BufferId,
    cursor: CursorPosition,
    selection: Option[Selection]
  ): Unit =
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
                    .copy(selection = selection, selections = Nil, cursors = List(cursor))
                )
            )
          )
        )
      }
      .unsafeRunSync()

  "a single-cursor paste" should "keep richTextDocument in sync with the pasted content" in {
    val stateManager = createStateManager()
    val bufferId     = stateManager.createBuffer("alpha beta gamma").unsafeRunSync()
    stateManager.setBufferForPane(PaneId(0), bufferId).unsafeRunSync()

    boldSelection(stateManager, bufferId, Selection(CursorPosition(0, 6), CursorPosition(0, 10)))
    stateManager.getCurrentState.unsafeRunSync().persisted.buffers(bufferId).richText.richTextDocument shouldBe defined

    setCursorAndSelection(stateManager, bufferId, CursorPosition(0, 17), None)
    stateManager.updateState(state => state.copy(runtime = state.runtime.copy(clipboard = Some(" delta")))).unsafeRunSync()

    stateManager.applyEvent(Paste).unsafeRunSync()

    val buffer       = stateManager.getCurrentState.unsafeRunSync().persisted.buffers(bufferId)
    val plainContent = buffer.document.content.collect()
    plainContent shouldBe "alpha beta gamma delta"
    buffer.richText.richTextDocument shouldBe defined
    buffer.richText.richTextDocument.map(_.plainText) shouldBe Some(plainContent)
  }

  "Tab-to-indent" should "keep richTextDocument in sync with the indented content" in {
    val stateManager = createStateManager()
    val bufferId     = stateManager.createBuffer("alpha\nbeta\ngamma").unsafeRunSync()
    stateManager.setBufferForPane(PaneId(0), bufferId).unsafeRunSync()

    boldSelection(stateManager, bufferId, Selection(CursorPosition(0, 0), CursorPosition(0, 5)))
    stateManager.getCurrentState.unsafeRunSync().persisted.buffers(bufferId).richText.richTextDocument shouldBe defined

    setCursorAndSelection(
      stateManager,
      bufferId,
      CursorPosition(1, 4),
      Some(Selection(CursorPosition(1, 0), CursorPosition(1, 4)))
    )

    stateManager.applyEvent(TabKey).unsafeRunSync()

    val buffer       = stateManager.getCurrentState.unsafeRunSync().persisted.buffers(bufferId)
    val plainContent = buffer.document.content.collect()
    plainContent should not be "alpha\nbeta\ngamma"
    buffer.richText.richTextDocument shouldBe defined
    buffer.richText.richTextDocument.map(_.plainText) shouldBe Some(plainContent)
  }

  "Shift+Tab-to-unindent" should "keep richTextDocument in sync with the unindented content" in {
    val stateManager = createStateManager()
    val bufferId     = stateManager.createBuffer("alpha\n\tbeta\ngamma").unsafeRunSync()
    stateManager.setBufferForPane(PaneId(0), bufferId).unsafeRunSync()

    boldSelection(stateManager, bufferId, Selection(CursorPosition(0, 0), CursorPosition(0, 5)))
    stateManager.getCurrentState.unsafeRunSync().persisted.buffers(bufferId).richText.richTextDocument shouldBe defined

    setCursorAndSelection(stateManager, bufferId, CursorPosition(1, 1), None)

    stateManager.applyEvent(ReverseTabKey).unsafeRunSync()

    val buffer       = stateManager.getCurrentState.unsafeRunSync().persisted.buffers(bufferId)
    val plainContent = buffer.document.content.collect()
    plainContent shouldBe "alpha\nbeta\ngamma"
    buffer.richText.richTextDocument shouldBe defined
    buffer.richText.richTextDocument.map(_.plainText) shouldBe Some(plainContent)
  }

  "a no-selection line Cut" should "keep richTextDocument in sync with the remaining content" in {
    val stateManager = createStateManager()
    val bufferId     = stateManager.createBuffer("alpha\nbeta\ngamma").unsafeRunSync()
    stateManager.setBufferForPane(PaneId(0), bufferId).unsafeRunSync()

    boldSelection(stateManager, bufferId, Selection(CursorPosition(0, 0), CursorPosition(0, 5)))
    stateManager.getCurrentState.unsafeRunSync().persisted.buffers(bufferId).richText.richTextDocument shouldBe defined

    setCursorAndSelection(stateManager, bufferId, CursorPosition(1, 2), None)

    stateManager.applyEvent(Cut).unsafeRunSync()

    val buffer       = stateManager.getCurrentState.unsafeRunSync().persisted.buffers(bufferId)
    val plainContent = buffer.document.content.collect()
    plainContent shouldBe "alpha\ngamma"
    buffer.richText.richTextDocument shouldBe defined
    buffer.richText.richTextDocument.map(_.plainText) shouldBe Some(plainContent)
  }
