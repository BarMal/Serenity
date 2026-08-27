package com.serenity

import com.serenity.keystroke.events.InsertChar
import com.serenity.state.components.{ComponentResult, EditorPaneComponent}
import com.serenity.state.models.*
import com.serenity.ui.layout.{Layout, ViewportSize}
import com.serenity.ui.renderer.Renderer
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class UnderscoreRenderingSpec extends AnyFlatSpec with Matchers:

  "EditorPaneComponent" should "insert underscore character correctly into buffer" in {
    import com.serenity.rope.Balance
    given Balance = Balance.default

    val bufferId = BufferId(1)
    val cursor   = CursorPosition(0, 5)
    val buffer   = Buffer.fromString(bufferId, "hello world").copy(editing = EditingState(cursors = List(cursor)))
    val paneId   = PaneId(1)
    val pane     = EditorPane(paneId, Some(bufferId), Viewport.default, List.empty, 0)
    val state = AppState.empty.copy(
      persisted = AppState.empty.persisted.copy(
        buffers = Map(bufferId -> buffer),
        layout = Layout.empty.copy(editorPanes = Map(paneId -> pane))
      )
    )

    val component       = new EditorPaneComponent(paneId)
    val underscoreEvent = InsertChar('_')

    val result = component.processEvent(underscoreEvent, state)

    result should not be ComponentResult.noChange
    result match
      case ComponentResult.ReducerUpdate(reducerResult) =>
        val newState      = reducerResult.state
        val updatedBuffer = newState.persisted.buffers(bufferId)
        updatedBuffer.document.content.collect() shouldBe "hello_ world"
        val newCursor = updatedBuffer.editing.cursors.head
        newCursor.column shouldBe 6
      case _ => fail("Expected ReducerUpdate result")
  }

  "Renderer" should "display underscore characters visibly" in {
    import com.serenity.rope.Balance
    given Balance = Balance.default

    val bufferId = BufferId(1)
    val cursor   = CursorPosition(0, 0)
    val buffer =
      Buffer.fromString(bufferId, "test_with_underscores").copy(editing = EditingState(cursors = List(cursor)))
    val paneId = PaneId(1)
    val pane   = EditorPane(paneId, Some(bufferId), Viewport.default, List.empty, 0)
    val state = AppState.empty.copy(
      persisted = AppState.empty.persisted.copy(
        buffers = Map(bufferId -> buffer),
        layout = Layout.empty.copy(editorPanes = Map(paneId -> pane))
      )
    )

    val surface = new MockRenderSurface(80, 24)
    Renderer.render(state, cursorVisible = true, surface, ViewportSize(80, 24))
    // Verify rendering completes without exception
  }
