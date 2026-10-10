package com.serenity

import com.serenity.config.{AppConfig, MarkdownViewMode}
import com.serenity.keystroke.events.{
  EditorEvent,
  ExtendSelectionLeft,
  ExtendSelectionRight,
  MoveLeft,
  MoveRight,
  MoveToEnd,
  MoveToStart
}
import com.serenity.lsp.config.LanguageId
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.{Layout, WorkspaceNode, WorkspaceNodeId, WorkspaceTree}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Hidden Markdown markers have no width, so the caret may only rest in front of a character the writer can see (or at
  * the end of the line): one key press then always moves it somewhere visible.
  */
class MarkdownReadModeCaretSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val bufferId = BufferId(1)
  private val paneId   = PaneId(1)

  // "a **b** c": the markers are columns 2, 3, 5 and 6.
  private val line = "a **b** c"

  private def stateAt(source: String, mode: MarkdownViewMode, cursor: CursorPosition): AppState =
    val base = Buffer.fromString(bufferId, source)
    val buffer = base.copy(
      document = base.document.copy(language = Some(LanguageId.Markdown)),
      editing = EditingState(List(cursor))
    )
    AppState.empty.copy(
      persisted = AppState.empty.persisted.copy(
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = Layout(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(WorkspaceTree(WorkspaceNode.Leaf(WorkspaceNodeId(s"editor-${paneId.value}"), paneId)))
        ),
        focus = Focus.EditorPane(paneId),
        config = AppConfig.default.withMarkdownViewMode(mode)
      )
    )

  private def after(event: EditorEvent, state: AppState): Buffer =
    VerticalNavSupport
      .dispatch(event, paneId, state)
      .state
      .persisted
      .buffers
      .getOrElse(bufferId, fail("buffer missing"))

  private def caretAfter(event: EditorEvent, source: String, mode: MarkdownViewMode, column: Int): CursorPosition =
    after(event, stateAt(source, mode, CursorPosition(0, column))).editing.cursorPositions.head

  "Moving right in read mode" should "step over a hidden marker to the next visible character" in {
    caretAfter(MoveRight, line, MarkdownViewMode.Read, 1) shouldBe CursorPosition(0, 4)
  }

  it should "step over the closing marker and the visible character before it in one press" in {
    caretAfter(MoveRight, line, MarkdownViewMode.Read, 4) shouldBe CursorPosition(0, 7)
  }

  it should "stop at the end of the line" in {
    caretAfter(MoveRight, line, MarkdownViewMode.Read, 8) shouldBe CursorPosition(0, 9)
  }

  "Moving left in read mode" should "step back over a hidden marker to the previous visible character" in {
    caretAfter(MoveLeft, line, MarkdownViewMode.Read, 7) shouldBe CursorPosition(0, 4)
    caretAfter(MoveLeft, line, MarkdownViewMode.Read, 4) shouldBe CursorPosition(0, 1)
  }

  "Home in read mode" should "land in front of the first visible character when the line opens with markers" in {
    caretAfter(MoveToStart, "**b** c", MarkdownViewMode.Read, 4) shouldBe CursorPosition(0, 2)
  }

  "End in read mode" should "land at the end of the line" in {
    val caret = caretAfter(MoveToEnd, line, MarkdownViewMode.Read, 0)

    (caret.line, caret.column) shouldBe (0, 9)
  }

  "Extending a selection in read mode" should "put its focus in front of a visible character" in {
    val state     = stateAt(line, MarkdownViewMode.Read, CursorPosition(0, 4))
    val selection = after(ExtendSelectionRight, state).primarySelection

    selection.map(_.anchor) shouldBe Some(CursorPosition(0, 4))
    selection.map(_.focus) shouldBe Some(CursorPosition(0, 7))
  }

  it should "extend backwards over hidden markers too" in {
    val selection =
      after(ExtendSelectionLeft, stateAt(line, MarkdownViewMode.Read, CursorPosition(0, 7))).primarySelection

    selection.map(_.focus) shouldBe Some(CursorPosition(0, 4))
  }

  "Moving in live preview or source mode" should "step one column at a time, markers being visible there" in {
    caretAfter(MoveRight, line, MarkdownViewMode.LivePreview, 1) shouldBe CursorPosition(0, 2)
    caretAfter(MoveRight, line, MarkdownViewMode.Source, 4) shouldBe CursorPosition(0, 5)
  }

  "Moving in read mode inside a fenced code block" should "step one column at a time" in {
    val fenced = "```\na **b** c\n```"
    val state  = stateAt(fenced, MarkdownViewMode.Read, CursorPosition(1, 1))

    after(MoveRight, state).editing.cursorPositions.head shouldBe CursorPosition(1, 2)
  }
