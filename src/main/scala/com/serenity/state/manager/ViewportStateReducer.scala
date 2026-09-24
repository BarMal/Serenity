package com.serenity.state.manager

import com.serenity.keystroke.events.ResizeEvent
import com.serenity.state.models.*
import com.serenity.state.reducers.{AppEventReducer, Focused, ReducerResult, SystemEventReducer}
import com.serenity.ui.layout.ViewportSize

object ViewportStateReducer:

  def ensureCursorVisible(paneId: PaneId, state: AppState): ReducerResult =
    val scrolled = Focused.bufferOf(state, paneId).map { buffer =>
      val cursor   = buffer.editing.cursors.head.position
      val viewport = CursorViewport.adjustForCursor(buffer, state, cursor)
      Focused.replaceBuffer(state, buffer.copy(viewport = viewport))
    }
    ReducerResult.noEffects(scrolled.getOrElse(state))

  /** `targetLine` comes from a click against the minimap as last rendered; a concurrent edit or undo can have shrunk
    * the document since, so it is clamped rather than trusted to be in bounds.
    */
  def clickMinimap(paneId: PaneId, targetLine: Int, state: AppState): ReducerResult =
    val jumped = Focused.bufferOf(state, paneId).map { buffer =>
      val clampedLine = math.max(0, math.min(targetLine, math.max(0, buffer.document.content.lineCount - 1)))
      val newTopLine  = math.max(0, clampedLine - buffer.viewport.visibleLines / 2)
      Focused.replaceBuffer(
        state,
        buffer.copy(
          editing = EditingState(List(CursorPosition(clampedLine, 0))),
          viewport = buffer.viewport.copy(topLine = newTopLine, topVisualLine = 0)
        )
      )
    }
    ReducerResult.noEffects(jumped.getOrElse(state))

  def resize(newSize: ViewportSize, state: AppState): ReducerResult =
    val resized = SystemEventReducer.reduce(ResizeEvent(newSize), state).state
    ReducerResult.noEffects(AppEventReducer.rebalancePanes(resized, resized.focusedBufferId))
