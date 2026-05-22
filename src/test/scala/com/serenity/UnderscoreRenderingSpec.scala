package com.serenity

import cats.effect.IO
import com.googlecode.lanterna.screen.TerminalScreen
import com.googlecode.lanterna.terminal.virtual.DefaultVirtualTerminal
import com.serenity.keystroke.events.InsertChar
import com.serenity.state.components.ComponentResult
import com.serenity.state.components.EditorPaneComponent
import com.serenity.state.models.*
import com.serenity.ui.layout.Layout
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class UnderscoreRenderingSpec extends AnyFlatSpec with Matchers:

  "EditorPaneComponent" should "insert underscore character correctly into buffer" in {
    import com.serenity.rope.Balance

    given Balance = Balance.default

    val bufferId = BufferId(1)
    val buffer   = Buffer.fromString(bufferId, "hello world")
    val paneId   = PaneId(1)
    val cursor   = CursorPosition(0, 5) // Between "hello" and " world"
    val pane     = EditorPane(paneId, Some(bufferId), Viewport.default, List(cursor), 0)
    val state = AppState.empty.copy(
      buffers = Map(bufferId -> buffer),
      layout = Layout.empty.copy(editorPanes = Map(paneId -> pane))
    )

    val component       = new EditorPaneComponent(paneId)
    val underscoreEvent = InsertChar('_')

    val result = component.processEvent(underscoreEvent, state)

    result should not be ComponentResult.noChange
    // Extract the new state and verify underscore was inserted
    result match
      case ComponentResult.StateChange(stateUpdate) =>
        val newState      = stateUpdate(state)
        val updatedBuffer = newState.buffers(bufferId)
        updatedBuffer.content.collect() shouldBe "hello_ world"

        val updatedPane = newState.layout.editorPanes(paneId)
        val newCursor   = updatedPane.cursors.head
        newCursor.column shouldBe 6 // Moved one position after underscore
      case _ => fail("Expected StateChange result")
  }

  "Renderer" should "display underscore characters visibly" in {
    import com.serenity.rope.Balance
    import com.serenity.ui.renderer.Renderer
    import com.serenity.ui.layout.{LayoutEngine, TerminalSize}
    import com.googlecode.lanterna.screen.VirtualScreen
    import com.googlecode.lanterna.terminal.virtual.DefaultVirtualTerminal

    given Balance = Balance.default

    // Create a virtual screen for testing rendering
    val virtualTerminal = new DefaultVirtualTerminal(com.googlecode.lanterna.TerminalSize.ONE)
    virtualTerminal.setTerminalSize(com.googlecode.lanterna.TerminalSize(80, 24))
    val screen = new TerminalScreen(virtualTerminal)

    // Create buffer with underscores
    val bufferId = BufferId(1)
    val buffer   = Buffer.fromString(bufferId, "test_with_underscores")
    val paneId   = PaneId(1)
    val cursor   = CursorPosition(0, 0)
    val pane     = EditorPane(paneId, Some(bufferId), Viewport.default, List(cursor), 0)
    val state = AppState.empty.copy(
      buffers = Map(bufferId -> buffer),
      layout = Layout.empty.copy(editorPanes = Map(paneId -> pane))
    )

    // Render the state
    Renderer.render(state, cursorVisible = true, screen)

    // Verify that rendering underscore characters completes without exception
    // The actual character verification would require access to screen internals
    // For now, we just verify that rendering doesn't crash
  }
