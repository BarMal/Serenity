package com.serenity.state.components

import com.serenity.keystroke.events.*
import com.serenity.state.models.*
import com.serenity.state.reducers.EditorEventReducer

class EditorPaneComponent(
    paneId: PaneId
)(using balance: com.serenity.rope.Balance)
    extends FocusedComponent:

  def processEvent(event: Event, currentState: AppState): ComponentResult =
    currentState.layout.editorPanes.get(paneId) match
      case Some(pane) => processEventForPane(event, pane, currentState)
      case None       => ComponentResult.noChange

  private def processEventForPane(
    event: Event,
    _pane: EditorPane,
    currentState: AppState
  ): ComponentResult =
    event match
      case ToggleSyntaxHighlighting =>
        ComponentResult.updateState { state =>
          state.copy(config = state.config.withSyntaxHighlighting(!state.syntaxHighlightingEnabled))
        }
      case editorEvent: EditorEvent =>
        ComponentResult.updateState(_ => EditorEventReducer.reduce(editorEvent, paneId, currentState).state)
      case _ =>
        ComponentResult.noChange
