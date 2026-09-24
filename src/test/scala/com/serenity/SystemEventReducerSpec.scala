package com.serenity

import com.serenity.keystroke.events.{ResizeEvent, UnhandledEvent}
import com.serenity.keystroke.translators.TextEntryTranslator
import com.serenity.keystroke.{InputKey, KeyStrokeInfo}
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.state.reducers.{ReducerResult, SystemEventReducer}
import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SystemEventReducerSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  "SystemEventReducer" should "recalculate buffer viewport dimensions on resize" in {
    val initialState = AppState.initial
    val newSize      = ViewportSize(120, 40)

    val ReducerResult(updatedState, effects) =
      SystemEventReducer.reduce(ResizeEvent(newSize), initialState)

    effects shouldBe Nil
    updatedState.runtime.viewportSize shouldBe Some(newSize)

    val expectedLayout = LayoutEngine.calculateLayout(updatedState, newSize)
    val contentRect = LayoutEngine
      .calculateEditorPaneLayouts(updatedState, expectedLayout)(PaneId(0))
      .contentRect
    val bufferId = updatedState.persisted.bufferOrder.head
    val buffer   = updatedState.persisted.buffers(bufferId)

    buffer.viewport.visibleColumns shouldBe contentRect.width
    buffer.viewport.visibleLines shouldBe contentRect.height
  }

  it should "leave unrelated system events as no-ops" in {
    val initialState = AppState.initial
    val unhandled    = UnhandledEvent(KeyStrokeInfo(InputKey.Unknown, None, Set.empty), new TextEntryTranslator())

    val result = SystemEventReducer.reduce(unhandled, initialState)

    result shouldBe ReducerResult.noEffects(initialState)
  }
