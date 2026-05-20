package com.serenity

import cats.effect.IO
import com.serenity.keystroke.events.InsertChar
import com.serenity.state.models.*
import com.serenity.state.components.EditorPaneComponent
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class UnderscoreRenderingSpec extends AnyFlatSpec with Matchers:

  "EditorPaneComponent" should "insert underscore character correctly into buffer" in {
    import com.serenity.rope.Balance
    
    given Balance = Balance.default
    
    val bufferId = BufferId(1)
    val buffer = Buffer.fromString(bufferId, "hello world")
    val paneId = PaneId(1)
    val cursor = CursorPosition(0, 5) // Between "hello" and " world"
    val pane = EditorPane(Some(bufferId), List(cursor), Viewport.default)
    val state = AppState.empty.copy(
      buffers = Map(bufferId -> buffer),
      layout = Layout.empty.copy(editorPanes = Map(paneId -> pane))
    )
    
    val component = new EditorPaneComponent(paneId)
    val underscoreEvent = InsertChar('_')
    
    val result = component.processEvent(underscoreEvent, state)
    
    result should not be ComponentResult.noChange
    // Extract the new state and verify underscore was inserted
    result match
      case ComponentResult.StateChange(stateUpdate) =>
        val newState = stateUpdate(state)
        val updatedBuffer = newState.buffers(bufferId)
        updatedBuffer.content.collect() shouldBe "hello_ world"
        
        val updatedPane = newState.layout.editorPanes(paneId)
        val newCursor = updatedPane.cursors.head
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
    val screen = new VirtualScreen(virtualTerminal)
    
    // Create buffer with underscores
    val bufferId = BufferId(1)
    val buffer = Buffer.fromString(bufferId, "test_with_underscores")
    val paneId = PaneId(1)
    val cursor = CursorPosition(0, 0)
    val pane = EditorPane(Some(bufferId), List(cursor), Viewport.default)
    val state = AppState.empty.copy(
      buffers = Map(bufferId -> buffer),
      layout = Layout.empty.copy(editorPanes = Map(paneId -> pane))
    )
    
    // Render the state
    Renderer.render(state, screen)
    
    // Check that underscores are rendered as visible characters (not blank)
    // Note: We check that the underscore character shows up in the screen buffer
    val terminalSize = screen.getTerminalSize
    val layout = LayoutEngine.calculateLayout(state, TerminalSize(terminalSize.getColumns, terminalSize.getRows))
    val panelRect = layout.editorPanelRect
    
    // Check positions where underscores should be
    val underscoreChar1 = screen.getChar(panelRect.x + 4, panelRect.y)  // position of first _
    val underscoreChar2 = screen.getChar(panelRect.x + 9, panelRect.y)  // position of second _
    
    underscoreChar1 should not be ' '
    underscoreChar1 should not be '\u0000'  // null character
    underscoreChar2 should not be ' '
    underscoreChar2 should not be '\u0000'
    
    // Ideally these should be underscore characters
    underscoreChar1 shouldBe '_'
    underscoreChar2 shouldBe '_'
  }