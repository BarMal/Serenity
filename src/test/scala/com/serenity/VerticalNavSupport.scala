package com.serenity

import com.serenity.keystroke.events.{EditorEvent, TextEntryEvent, VerticalNavigationEvent}
import com.serenity.state.manager.EditorGeometryProducer
import com.serenity.state.models.{AppState, EditorGeometry, NavigationGeometry, PaneId}
import com.serenity.state.reducers.{EditorEventReducer, ReducerResult}

/** Routes an editor event to the right reducer entry in tests: vertical navigation gets geometry built exactly the way
  * the effect boundary builds it, everything else goes straight to `reduce`. Lets specs keep dispatching the whole
  * editor event family through one call while the production split lives at the pipeline.
  */
object VerticalNavSupport:

  def dispatch(event: EditorEvent, paneId: PaneId, state: AppState)(using
    balance: com.serenity.rope.Balance
  ): ReducerResult =
    event match
      case vertical: VerticalNavigationEvent =>
        val geometry = EditorGeometryProducer
          .forPane(state, paneId)
          .getOrElse(EditorGeometry(NavigationGeometry(Vector.empty), charWidthPx = 8, panelWidthColumns = 80))
        EditorEventReducer.reduceVerticalNavigation(vertical, paneId, state, geometry)
      case textEvent: TextEntryEvent =>
        EditorEventReducer.reduce(textEvent, paneId, state)
