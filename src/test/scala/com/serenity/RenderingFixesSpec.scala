package com.serenity

import cats.effect.unsafe.implicits.global
import com.serenity.keystroke.events.{InsertChar, ToggleSyntaxHighlighting}
import com.serenity.state.components.EditorPaneComponent
import com.serenity.state.models.*
import com.serenity.ui.renderer.CharacterRenderer
import com.serenity.rope.Balance
import com.googlecode.lanterna.screen.VirtualScreen
import com.googlecode.lanterna.terminal.virtual.DefaultVirtualTerminal
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RenderingFixesSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  "Syntax highlighting toggle" should "work correctly" in {
    val bufferId = BufferId(1)
    val buffer = Buffer.fromString(bufferId, "test content")
    val paneId = PaneId(1)
    val cursor = CursorPosition(0, 0)
    val pane = EditorPane(Some(bufferId), List(cursor), Viewport.default)
    val state = AppState.empty.copy(
      buffers = Map(bufferId -> buffer),
      layout = Layout.empty.copy(editorPanes = Map(paneId -> pane)),
      syntaxHighlightingEnabled = false // Default off
    )
    
    val component = new EditorPaneComponent(paneId)
    
    // Initially syntax highlighting should be off
    state.syntaxHighlightingEnabled shouldBe false
    
    // Toggle it on
    val result = component.processEvent(ToggleSyntaxHighlighting, state)
    result should not be ComponentResult.noChange
    
    result match
      case ComponentResult.StateChange(update) =>
        val newState = update(state)
        newState.syntaxHighlightingEnabled shouldBe true
        
        // Toggle it off again
        val result2 = component.processEvent(ToggleSyntaxHighlighting, newState)
        result2 match
          case ComponentResult.StateChange(update2) =>
            val finalState = update2(newState)
            finalState.syntaxHighlightingEnabled shouldBe false
          case _ => fail("Expected StateChange result")
      case _ => fail("Expected StateChange result")
  }

  "Tab character rendering" should "expand to proper width" in {
    val virtualTerminal = new DefaultVirtualTerminal(com.googlecode.lanterna.TerminalSize.ONE)
    virtualTerminal.setTerminalSize(com.googlecode.lanterna.TerminalSize(80, 24))
    val screen = new VirtualScreen(virtualTerminal)
    val graphics = screen.newTextGraphics()
    
    // Test tab expansion with default 4-space width
    CharacterRenderer.renderStringPlain(graphics, 0, 0, "a\tb")
    
    // The 'a' should be at position 0, the 'b' should be at position 4
    screen.getChar(0, 0) shouldBe 'a'
    screen.getChar(1, 0) shouldBe ' ' // First tab space
    screen.getChar(2, 0) shouldBe ' ' // Second tab space 
    screen.getChar(3, 0) shouldBe ' ' // Third tab space
    screen.getChar(4, 0) shouldBe 'b' // Character after tab
  }

  "Underscore character" should "render visibly" in {
    val virtualTerminal = new DefaultVirtualTerminal(com.googlecode.lanterna.TerminalSize.ONE)
    virtualTerminal.setTerminalSize(com.googlecode.lanterna.TerminalSize(80, 24))
    val screen = new VirtualScreen(virtualTerminal)
    val graphics = screen.newTextGraphics()
    
    // Test underscore rendering
    CharacterRenderer.renderStringPlain(graphics, 0, 0, "test_underscore")
    
    // Verify underscore is rendered at position 4
    screen.getChar(4, 0) shouldBe '_'
    screen.getChar(4, 0) should not be ' '
    screen.getChar(4, 0) should not be '\u0000'
  }

  "Default syntax highlighting" should "be off" in {
    val state = AppState.empty
    state.syntaxHighlightingEnabled shouldBe false
  }

  "Character rendering" should "handle special cases" in {
    val virtualTerminal = new DefaultVirtualTerminal(com.googlecode.lanterna.TerminalSize.ONE)
    virtualTerminal.setTerminalSize(com.googlecode.lanterna.TerminalSize(80, 24))
    val screen = new VirtualScreen(virtualTerminal)
    val graphics = screen.newTextGraphics()
    
    // Test various special characters
    CharacterRenderer.renderStringPlain(graphics, 0, 0, "a_b\tc")
    
    // Should have: 'a' at 0, '_' at 1, 'b' at 2, spaces for tab, 'c' at appropriate position
    screen.getChar(0, 0) shouldBe 'a'
    screen.getChar(1, 0) shouldBe '_'
    screen.getChar(2, 0) shouldBe 'b'
    // Tab should start at position 3, and 'c' should be at position 4 (next tab stop)
    screen.getChar(4, 0) shouldBe 'c'
  }