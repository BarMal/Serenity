package com.serenity.state.components

import com.serenity.keystroke.events.*
import com.serenity.state.models.*
import com.serenity.state.reducers.{EditorEventReducer, Reducer}

class EditorPaneComponent(
    paneId: PaneId
)(using balance: com.serenity.rope.Balance)
    extends TypedFocusedComponent[TextEntryEvent]:
  private val reducer: Reducer[TextEntryEvent] = EditorEventReducer.reducer(paneId)

  protected def decodeEvent(event: Event): Option[TextEntryEvent] =
    event match
      case ToggleSyntaxHighlighting  => None
      case textEvent: TextEntryEvent => Some(textEvent)
      case _                         => None

  protected def processTypedEvent(event: TextEntryEvent, currentState: AppState): ComponentResult =
    currentState.layout.editorPanes.get(paneId) match
      case Some(pane) => processEventForPane(event, pane, currentState)
      case None       => ComponentResult.noChange

  override protected def processFallbackEvent(event: Event, currentState: AppState): ComponentResult =
    event match
      case ToggleSyntaxHighlighting =>
        ComponentResult.updateState { state =>
          state.copy(config = state.config.withSyntaxHighlighting(!state.syntaxHighlightingEnabled))
        }
      case _ =>
        ComponentResult.noChange

  private def processEventForPane(
    event: TextEntryEvent,
    _pane: EditorPane,
    currentState: AppState
  ): ComponentResult =
    ComponentResult.updateState(_ => reducer.reduce(event, currentState).state)
