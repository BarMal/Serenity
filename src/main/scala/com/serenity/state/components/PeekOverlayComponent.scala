package com.serenity.state.components

import com.googlecode.lanterna.input.KeyType
import com.serenity.keystroke.KeyStrokeInfo
import com.serenity.keystroke.events.*
import com.serenity.state.models.AppState

class PeekOverlayComponent() extends FocusedComponent:

  def processEvent(event: Event, currentState: AppState): ComponentResult =
    event match
      case textEvent: TextEntryEvent => processTextEvent(textEvent, currentState)
      case UnhandledEvent(keyStroke, _) =>
        val keyInfo = com.serenity.keystroke.KeyStrokeInfo.fromKeyStroke(keyStroke)
        processKeyStroke(keyInfo, currentState)
      case _ => ComponentResult.noChange

  private def processTextEvent(event: TextEntryEvent, currentState: AppState): ComponentResult =
    event match
      case MoveUp | MoveDown | MoveLeft | MoveRight =>
        // Navigation within peek content - for now just dismiss
        ComponentResult.dismiss
      case _ =>
        // Other text events dismiss the peek overlay
        ComponentResult.dismiss

  private def processKeyStroke(keyInfo: KeyStrokeInfo, currentState: AppState): ComponentResult =
    keyInfo.keyType match
      case KeyType.Escape => ComponentResult.dismiss
      case KeyType.Enter  =>
        // TODO: Implement action based on peek content type
        ComponentResult.dismiss
      case _ => ComponentResult.noChange
