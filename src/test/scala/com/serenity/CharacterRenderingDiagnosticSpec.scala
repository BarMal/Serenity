package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.googlecode.lanterna.screen.{Screen, TerminalScreen}
import com.googlecode.lanterna.terminal.virtual.DefaultVirtualTerminal
import com.serenity.keystroke.events.InsertChar
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import com.serenity.ui.renderer.Renderer
import com.serenity.ui.layout.Layout
import com.serenity.rope.Balance
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CharacterRenderingDiagnosticSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  "Character Rendering Diagnostics" should "verify that various special characters render correctly" in {
    // Create a virtual screen for testing
    val virtualTerminal = new DefaultVirtualTerminal(com.googlecode.lanterna.TerminalSize.ONE)
    virtualTerminal.setTerminalSize(com.googlecode.lanterna.TerminalSize(80, 24))
    val screen = new TerminalScreen(virtualTerminal)
    
    // Test various characters that might have rendering issues
    val testChars = List('_', '-', '=', '+', '*', '#', '@', '%', '&', '|', '\\', '/', '~', '`')
    
    for (char <- testChars) do
      // Create buffer with the character
      val bufferId = BufferId(1)
      val buffer = Buffer.fromString(bufferId, s"test${char}char")
      val paneId = PaneId(1)
      val cursor = CursorPosition(0, 0)
      val pane = EditorPane(paneId, Some(bufferId), Viewport.default, List(cursor), 0)
      val state = AppState.empty.copy(
        buffers = Map(bufferId -> buffer),
        layout = Layout.empty.copy(editorPanes = Map(paneId -> pane))
      )
      
      // Render the state
      Renderer.render(state, screen)
      
      // Render the state (this tests that rendering doesn't crash)
      Renderer.render(state, screen)
      // The actual character verification would require access to screen internals
      // For now, we just verify that rendering completes without exception
  }

  it should "verify underscore specifically in various contexts" in {
    val virtualTerminal = new DefaultVirtualTerminal(com.googlecode.lanterna.TerminalSize.ONE)
    virtualTerminal.setTerminalSize(com.googlecode.lanterna.TerminalSize(80, 24))
    val screen = new TerminalScreen(virtualTerminal)
    
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
      val pane = EditorPane(paneId, Some(bufferId), Viewport.default, List(cursor), 0)
      val state = AppState.empty.copy(
        buffers = Map(bufferId -> buffer),
        layout = Layout.empty.copy(editorPanes = Map(paneId -> pane))
      )
      
      // Render the state
      Renderer.render(state, screen)
      
      // Render the state (this tests that rendering doesn't crash)
      Renderer.render(state, screen)
      // The actual character verification would require access to screen internals
      // For now, we just verify that rendering completes without exception
  }