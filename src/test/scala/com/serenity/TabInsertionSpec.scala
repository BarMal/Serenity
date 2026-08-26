package com.serenity

import com.serenity.keystroke.events.{InsertChar, TabKey}
import com.serenity.keystroke.translators.TextEntryTranslator
import com.serenity.keystroke.{InputKey, KeyStrokeInfo}
import com.serenity.state.components.ComponentResult
import com.serenity.ui.layout.Layout
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TabInsertionSpec extends AnyFlatSpec with Matchers:

  "TextEntryTranslator" should "convert Tab key to a dedicated tab event" in {
    val translator = new TextEntryTranslator()
    val tabInfo    = KeyStrokeInfo(InputKey.Tab, None, Set.empty)

    val result = translator.translate(tabInfo)

    result shouldBe TabKey
  }

  it should "allow tab characters in printable character validation" in {
    val translator = new TextEntryTranslator()
    val tabInfo    = KeyStrokeInfo(InputKey.Character, Some('\t'), Set.empty)

    val result = translator.translate(tabInfo)

    result shouldBe InsertChar('\t')
  }

  "EditorPaneComponent" should "insert fixed spaces into the buffer when tab is pressed" in {
    import com.serenity.rope.Balance
    import com.serenity.state.models.*
    import com.serenity.state.components.EditorPaneComponent

    given Balance = Balance.default

    val bufferId = BufferId(1)
    val cursor   = CursorPosition(0, 5)
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
    result match
      case ComponentResult.ReducerUpdate(reducerResult) =>
        val newState      = reducerResult.state
        val updatedBuffer = newState.buffers(bufferId)
        updatedBuffer.content.collect() shouldBe "hello     world"

        val newCursor = updatedBuffer.cursors.head
        newCursor.column shouldBe 9
      case _ => fail("Expected ReducerUpdate result")
  }
