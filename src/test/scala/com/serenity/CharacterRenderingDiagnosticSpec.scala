package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.googlecode.lanterna.screen.VirtualScreen
import com.googlecode.lanterna.terminal.virtual.DefaultVirtualTerminal
import com.serenity.keystroke.events.InsertChar
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import com.serenity.ui.renderer.Renderer
import com.serenity.rope.Balance
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CharacterRenderingDiagnosticSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  "Character Rendering Diagnostics" should "verify that various special characters render correctly" in {
    // Create a virtual screen for testing
    val virtualTerminal = new DefaultVirtualTerminal(com.googlecode.lanterna.TerminalSize.ONE)
    virtualTerminal.setTerminalSize(com.googlecode.lanterna.TerminalSize(80, 24))
    val screen = new VirtualScreen(virtualTerminal)
    
    // Test various characters that might have rendering issues
    val testChars = List('_', '-', '=', '+', '*', '#', '@', '%', '&', '|', '\\', '/', '~', '`')
    
    for (char <- testChars) do
      // Create buffer with the character
      val bufferId = BufferId(1)
      val buffer = Buffer.fromString(bufferId, s"test${char}char")
      val paneId = PaneId(1)
      val cursor = CursorPosition(0, 0)
      val pane = EditorPane(Some(bufferId), List(cursor), Viewport.default)
      val state = AppState.empty.copy(
        buffers = Map(bufferId -> buffer),
        layout = Layout.empty.copy(editorPanes = Map(paneId -> pane))
      )
      
      // Render the state
      Renderer.render(state, screen)
      
      // Check that the character is rendered correctly (position 4 in "test_char")
      val layout = com.serenity.ui.layout.LayoutEngine.calculateLayout(
        state, 
        com.serenity.ui.layout.TerminalSize(80, 24)
      )
      val panelRect = layout.editorPanelRect
      val renderedChar = screen.getChar(panelRect.x + 4, panelRect.y)
      
      withClue(s"Character '$char' should render as itself, but got '$renderedChar'") {
        renderedChar should not be ' '
        renderedChar should not be '\u0000'
        renderedChar shouldBe char
      }
  }

  it should "verify underscore specifically in various contexts" in {
    val virtualTerminal = new DefaultVirtualTerminal(com.googlecode.lanterna.TerminalSize.ONE)
    virtualTerminal.setTerminalSize(com.googlecode.lanterna.TerminalSize(80, 24))
    val screen = new VirtualScreen(virtualTerminal)
    
    val testStrings = List(
      "_",                    // Standalone underscore
      "_underscore",          // At beginning
      "underscore_",          // At end
      "under_score",          // In middle
      "___",                  // Multiple consecutive
      "a_b_c_d_e",           // Multiple separated
      "snake_case_variable"   // Real-world example
    )
    
    for (testString <- testStrings) do
      // Create buffer with test string
      val bufferId = BufferId(1)
      val buffer = Buffer.fromString(bufferId, testString)
      val paneId = PaneId(1)
      val cursor = CursorPosition(0, 0)
      val pane = EditorPane(Some(bufferId), List(cursor), Viewport.default)
      val state = AppState.empty.copy(
        buffers = Map(bufferId -> buffer),
        layout = Layout.empty.copy(editorPanes = Map(paneId -> pane))
      )
      
      // Render the state
      Renderer.render(state, screen)
      
      // Verify each underscore position
      val layout = com.serenity.ui.layout.LayoutEngine.calculateLayout(
        state, 
        com.serenity.ui.layout.TerminalSize(80, 24)
      )
      val panelRect = layout.editorPanelRect
      
      for ((char, index) <- testString.zipWithIndex if char == '_') do
        val renderedChar = screen.getChar(panelRect.x + index, panelRect.y)
        withClue(s"Underscore at position $index in '$testString' should render correctly") {
          renderedChar should not be ' '
          renderedChar should not be '\u0000'
          renderedChar shouldBe '_'
        }
  }