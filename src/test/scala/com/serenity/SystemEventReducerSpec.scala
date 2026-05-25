package com.serenity

import com.serenity.keystroke.events.{ResizeEvent, UnhandledEvent}
import com.serenity.rope.Balance
import com.serenity.state.reducers.{ReducerResult, SystemEventReducer}
import com.serenity.keystroke.translators.TextEntryTranslator
import com.serenity.ui.layout.{LayoutEngine, TerminalSize}
import com.googlecode.lanterna.input.{KeyStroke, KeyType}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SystemEventReducerSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  "SystemEventReducer" should "recalculate buffer viewport dimensions on resize" in {
    val initialState = com.serenity.state.models.AppState.initial
    val newSize      = TerminalSize(120, 40)

    val ReducerResult(updatedState, effects) =
      SystemEventReducer.reduce(ResizeEvent(newSize), initialState)

    effects shouldBe Nil
    updatedState.terminalSize shouldBe Some(newSize)

    val expectedLayout = LayoutEngine.calculateLayout(updatedState, newSize)
    val bufferId       = updatedState.bufferOrder.head
    val buffer         = updatedState.buffers(bufferId)

    buffer.viewport.visibleColumns shouldBe expectedLayout.editorPanelRect.width
    buffer.viewport.visibleLines shouldBe expectedLayout.editorPanelRect.height
  }

  it should "leave unrelated system events as no-ops" in {
    val initialState = com.serenity.state.models.AppState.initial
    val unhandled    = UnhandledEvent(new KeyStroke(KeyType.Unknown), new TextEntryTranslator())

    val result = SystemEventReducer.reduce(unhandled, initialState)

    result shouldBe ReducerResult.noEffects(initialState)
  }
