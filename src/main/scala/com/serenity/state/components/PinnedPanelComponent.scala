package com.serenity.state.components

import com.googlecode.lanterna.input.KeyType
import com.serenity.keystroke.KeyStrokeInfo
import com.serenity.keystroke.events.*
import com.serenity.state.models.{AppState, Focus, SurfacePresentation}
import com.serenity.ui.layout.PanelPosition

class PinnedPanelComponent(
    position: PanelPosition
) extends FocusedComponent:

  def processEvent(event: Event, currentState: AppState): ComponentResult =
    currentState.uiSurfaces.find {
      _.presentation match
        case SurfacePresentation.Pinned(pos, _) if pos == position => true
        case _                                                     => false
    } match
      case Some(_) =>
        event match
          case textEvent: TextEntryEvent => processPanelTextEvent(textEvent, currentState)
          case UnhandledEvent(keyStroke, _) =>
            val keyInfo = KeyStrokeInfo.fromKeyStroke(keyStroke)
            processPanelKeyStroke(keyInfo, currentState)
          case _ => ComponentResult.noChange
      case None => ComponentResult.noChange

  private def processPanelTextEvent(event: TextEntryEvent, currentState: AppState): ComponentResult =
    event match
      case MoveUp | MoveDown | MoveLeft | MoveRight => ComponentResult.noChange
      case _ =>
        currentState.layout.activeEditorPaneId match
          case Some(paneId) => ComponentResult.transferFocus(Focus.EditorPane(paneId))
          case None         => ComponentResult.noChange

  private def processPanelKeyStroke(keyInfo: KeyStrokeInfo, currentState: AppState): ComponentResult =
    keyInfo.keyType match
      case KeyType.Escape =>
        currentState.layout.activeEditorPaneId match
          case Some(paneId) => ComponentResult.transferFocus(Focus.EditorPane(paneId))
          case None         => ComponentResult.noChange
      case KeyType.Enter => ComponentResult.noChange
      case _ => ComponentResult.noChange
