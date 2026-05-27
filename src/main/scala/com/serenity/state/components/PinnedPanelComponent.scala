package com.serenity.state.components

import com.serenity.keystroke.events.*
import com.serenity.state.models.{AppState, Focus, SurfacePresentation}
import com.serenity.ui.layout.PanelPosition

class PinnedPanelComponent(
    position: PanelPosition
) extends TypedFocusedComponent[PanelInputEvent]:

  protected def decodeEvent(event: Event): Option[PanelInputEvent] =
    PanelInputEvent.fromEvent(event)

  protected def processTypedEvent(event: PanelInputEvent, currentState: AppState): ComponentResult =
    currentState.uiSurfaces.find {
      _.presentation match
        case SurfacePresentation.Pinned(pos, _) if pos == position => true
        case _                                                     => false
    } match
      case Some(_) =>
        processPanelEvent(event, currentState)
      case None => ComponentResult.noChange

  private def processPanelEvent(event: PanelInputEvent, currentState: AppState): ComponentResult =
    event match
      case PanelInputEvent.Navigate(_) | PanelInputEvent.NoOp =>
        ComponentResult.noChange
      case PanelInputEvent.ReturnFocus =>
        currentState.layout.activeEditorPaneId match
          case Some(paneId) => ComponentResult.transferFocus(Focus.EditorPane(paneId))
          case None         => ComponentResult.noChange
