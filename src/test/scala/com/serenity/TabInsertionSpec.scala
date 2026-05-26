package com.serenity

import cats.effect.IO
import com.googlecode.lanterna.input.KeyType
import com.serenity.keystroke.events.{InsertChar, TabKey, TextEntryEvent}
import com.serenity.keystroke.translators.TextEntryTranslator
import com.serenity.keystroke.{KeyStrokeInfo, Modifier}
import com.serenity.state.components.ComponentResult
import com.serenity.ui.layout.Layout
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TabInsertionSpec extends AnyFlatSpec with Matchers:

  "TextEntryTranslator" should "convert Tab key to a dedicated tab event" in {
    val translator   = new TextEntryTranslator()
    val tabKeyStroke = com.googlecode.lanterna.input.KeyStroke(KeyType.Tab, false, false, false)

    val result = translator.translate(tabKeyStroke)

    result shouldBe TabKey
  }

  it should "allow tab characters in printable character validation" in {
    val translator = new TextEntryTranslator()
    val tabChar    = '\t'

    // This tests the private isPrintableChar method indirectly
    val tabKeyStroke = com.googlecode.lanterna.input.KeyStroke(tabChar, false, false, false)
    val result       = translator.translate(tabKeyStroke)

    result shouldBe InsertChar('\t')
  }

  "EditorPaneComponent" should "insert tab character into buffer correctly" in {
    import com.serenity.rope.Balance
    import com.serenity.state.models.*
    import com.serenity.state.components.EditorPaneComponent

    given Balance = Balance.default

    val bufferId = BufferId(1)
    val cursor   = CursorPosition(0, 5) // Between "hello" and " world"
    val buffer   = Buffer.fromString(bufferId, "hello world").copy(cursors = List(cursor))
    val paneId   = PaneId(1)
    val pane     = EditorPane(paneId, Some(bufferId), Viewport.default, List.empty, 0)
    val state = AppState.empty.copy(
      buffers = Map(bufferId -> buffer),
      layout = Layout.empty.copy(editorPanes = Map(paneId -> pane))
    )

    val component = new EditorPaneComponent(paneId)
    val tabEvent  = TabKey

    val result = component.processEvent(tabEvent, state)

    result should not be ComponentResult.noChange
    // Extract the new state and verify tab was inserted
    result match
      case ComponentResult.StateChange(stateUpdate) =>
        val newState      = stateUpdate(state)
        val updatedBuffer = newState.buffers(bufferId)
        updatedBuffer.content.collect() shouldBe "hello\t world"

        val newCursor = updatedBuffer.cursors.head
        newCursor.column shouldBe 6 // Moved one position after tab
      case _ => fail("Expected StateChange result")
  }
