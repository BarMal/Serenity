package com.serenity

import cats.effect.IO
import com.serenity.keystroke.events.{InsertChar, TextEntryEvent}
import com.serenity.keystroke.{KeyStrokeInfo, Modifier}
import com.serenity.keystroke.translators.TextEntryTranslator
import com.googlecode.lanterna.input.KeyType
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TabInsertionSpec extends AnyFlatSpec with Matchers:

  "TextEntryTranslator" should "convert Tab key to InsertChar with tab character" in {
    val translator = new TextEntryTranslator()
    val tabKeyStroke = com.googlecode.lanterna.input.KeyStroke(KeyType.Tab, '\t', false, false, false)
    
    val result = translator.translate(tabKeyStroke)
    
    result shouldBe InsertChar('\t')
  }

  it should "allow tab characters in printable character validation" in {
    val translator = new TextEntryTranslator()
    val tabChar = '\t'
    
    // This tests the private isPrintableChar method indirectly
    val tabKeyStroke = com.googlecode.lanterna.input.KeyStroke(KeyType.Character, tabChar, false, false, false)
    val result = translator.translate(tabKeyStroke)
    
    result shouldBe InsertChar('\t')
  }

  "EditorPaneComponent" should "insert tab character into buffer correctly" in {
    import com.serenity.rope.Balance
    import com.serenity.state.models.*
    import com.serenity.state.components.EditorPaneComponent
    
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
    val tabEvent = InsertChar('\t')
    
    val result = component.processEvent(tabEvent, state)
    
    result should not be ComponentResult.noChange
    // Extract the new state and verify tab was inserted
    result match
      case ComponentResult.StateChange(stateUpdate) =>
        val newState = stateUpdate(state)
        val updatedBuffer = newState.buffers(bufferId)
        updatedBuffer.content.collect() shouldBe "hello\t world"
        
        val updatedPane = newState.layout.editorPanes(paneId)
        val newCursor = updatedPane.cursors.head
        newCursor.column shouldBe 6 // Moved one position after tab
      case _ => fail("Expected StateChange result")
  }