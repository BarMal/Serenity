package com.serenity

import com.serenity.keystroke.events.{MoveDown, MoveUp}
import com.serenity.lsp.config.LanguageId
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.state.reducers.EditorEventReducer
import com.serenity.ui.layout.{Layout, ViewportSize}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Verifies that MoveUp / MoveDown navigate correctly when using a single shared snapshot (exercising the
  * navigationSnapshot extraction in B3).
  */
class EditorEventSnapshotSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val paneId   = PaneId(0)
  private val bufferId = BufferId(1)

  private def stateWith(
    content: String,
    cursor: CursorPosition,
    language: Option[LanguageId] = Some(LanguageId.Scala)
  ): AppState =
    val buffer = Buffer
      .fromString(bufferId, content)
      .copy(
        cursors = List(cursor),
        language = language,
        viewport = Viewport(topLine = 0, leftColumn = 0, visibleColumns = 20, visibleLines = 10)
      )
    val pane = EditorPane.withBuffer(paneId, bufferId)
    AppState.initial.copy(
      buffers = Map(bufferId -> buffer),
      bufferOrder = List(bufferId),
      layout = Layout(
        editorPanes = Map(paneId -> pane),
        activeEditorPaneId = Some(paneId)
      ),
      viewportSize = Some(ViewportSize(80, 24))
    )

  "EditorEventReducer (navigationSnapshot)" should "move cursor up from line 1 col 2 to line 0 col 2" in {
    val state     = stateWith("hello\nworld", CursorPosition(line = 1, column = 2))
    val result    = EditorEventReducer.reduce(MoveUp, paneId, state).state
    val newCursor = result.buffers(bufferId).cursors.head
    newCursor.line shouldBe 0
    newCursor.column shouldBe 2
  }

  it should "move cursor down from line 0 col 2 to line 1 col 2" in {
    val state     = stateWith("hello\nworld", CursorPosition(line = 0, column = 2))
    val result    = EditorEventReducer.reduce(MoveDown, paneId, state).state
    val newCursor = result.buffers(bufferId).cursors.head
    newCursor.line shouldBe 1
    newCursor.column shouldBe 2
  }

  it should "return to the original column after MoveDown then MoveUp" in {
    val state      = stateWith("hello\nworld", CursorPosition(line = 0, column = 2))
    val afterDown  = EditorEventReducer.reduce(MoveDown, paneId, state).state
    val afterRound = EditorEventReducer.reduce(MoveUp, paneId, afterDown).state
    afterRound.buffers(bufferId).cursors.head.column shouldBe 2
  }
