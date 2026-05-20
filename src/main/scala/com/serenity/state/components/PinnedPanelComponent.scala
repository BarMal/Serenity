package com.serenity.state.components

import com.googlecode.lanterna.input.KeyType
import com.serenity.keystroke.KeyStrokeInfo
import com.serenity.keystroke.events.*
import com.serenity.state.models.{AppState, Focus}
import com.serenity.ui.layout.PanelPosition

class PinnedPanelComponent(
    position: PanelPosition
) extends FocusedComponent:

  def processEvent(event: Event, currentState: AppState): ComponentResult =
    currentState.layout.pinnedPanels.get(position) match
      case Some(panel) =>
        event match
          case textEvent: TextEntryEvent => processPanelTextEvent(textEvent, currentState)
          case UnhandledEvent(keyStroke, _) =>
            val keyInfo = KeyStrokeInfo.fromKeyStroke(keyStroke)
            processPanelKeyStroke(keyInfo, currentState)
          case _ => ComponentResult.noChange
      case None => ComponentResult.noChange

  private def processPanelTextEvent(event: TextEntryEvent, currentState: AppState): ComponentResult =
    event match
      case MoveUp | MoveDown | MoveLeft | MoveRight =>
        // TODO: Handle navigation within panel content
        ComponentResult.noChange
      case _ =>
        // Transfer focus back to editor for other text events
        currentState.layout.activeEditorPaneId match
          case Some(paneId) => ComponentResult.transferFocus(Focus.EditorPane(paneId))
          case None         => ComponentResult.noChange

  private def processPanelKeyStroke(keyInfo: KeyStrokeInfo, currentState: AppState): ComponentResult =
    keyInfo.keyType match
      case KeyType.Escape =>
        // Transfer focus back to editor
        currentState.layout.activeEditorPaneId match
          case Some(paneId) => ComponentResult.transferFocus(Focus.EditorPane(paneId))
          case None         => ComponentResult.noChange
      case KeyType.Enter =>
        // TODO: Handle enter action based on panel content type
        ComponentResult.noChange
      case _ => ComponentResult.noChange
