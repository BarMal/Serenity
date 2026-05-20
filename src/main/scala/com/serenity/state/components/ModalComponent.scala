package com.serenity.state.components

import com.googlecode.lanterna.input.KeyType
import com.serenity.keystroke.KeyStrokeInfo
import com.serenity.keystroke.events.*
import com.serenity.state.models.{AppState, ModalType}

class ModalComponent(
    modalType: ModalType
) extends FocusedComponent:

  def processEvent(event: Event, currentState: AppState): ComponentResult =
    modalType match
      case ModalType.CommandPalette =>
        processCommandPaletteEvent(event, currentState)
      case ModalType.FileSearch =>
        processFileSearchEvent(event, currentState)
      case ModalType.QuickOpen =>
        processQuickOpenEvent(event, currentState)
      case ModalType.Custom(name) =>
        processCustomModalEvent(name, event, currentState)

  private def processCommandPaletteEvent(event: Event, currentState: AppState): ComponentResult =
    event match
      case textEvent: TextEntryEvent => processModalTextEvent(textEvent, currentState)
      case UnhandledEvent(keyStroke, _) =>
        val keyInfo = KeyStrokeInfo.fromKeyStroke(keyStroke)
        processModalKeyStroke(keyInfo, currentState)
      case _ => ComponentResult.noChange

  private def processFileSearchEvent(event: Event, currentState: AppState): ComponentResult =
    event match
      case textEvent: TextEntryEvent => processModalTextEvent(textEvent, currentState)
      case UnhandledEvent(keyStroke, _) =>
        val keyInfo = KeyStrokeInfo.fromKeyStroke(keyStroke)
        processModalKeyStroke(keyInfo, currentState)
      case _ => ComponentResult.noChange

  private def processQuickOpenEvent(event: Event, currentState: AppState): ComponentResult =
    event match
      case textEvent: TextEntryEvent => processModalTextEvent(textEvent, currentState)
      case UnhandledEvent(keyStroke, _) =>
        val keyInfo = KeyStrokeInfo.fromKeyStroke(keyStroke)
        processModalKeyStroke(keyInfo, currentState)
      case _ => ComponentResult.noChange

  private def processCustomModalEvent(name: String, event: Event, currentState: AppState): ComponentResult =
    event match
      case textEvent: TextEntryEvent => processModalTextEvent(textEvent, currentState)
      case UnhandledEvent(keyStroke, _) =>
        val keyInfo = KeyStrokeInfo.fromKeyStroke(keyStroke)
        processModalKeyStroke(keyInfo, currentState)
      case _ => ComponentResult.noChange

  private def processModalTextEvent(event: TextEntryEvent, currentState: AppState): ComponentResult =
    event match
      case InsertChar(_) =>
        // TODO: Handle text input for search/command input
        ComponentResult.noChange
      case DeleteBackward =>
        // TODO: Handle backspace in search/command input
        ComponentResult.noChange
      case MoveUp | MoveDown =>
        // TODO: Handle navigation in suggestions/results
        ComponentResult.noChange
      case _ => ComponentResult.noChange

  private def processModalKeyStroke(keyInfo: KeyStrokeInfo, currentState: AppState): ComponentResult =
    keyInfo.keyType match
      case KeyType.Escape => ComponentResult.dismiss
      case KeyType.Enter  =>
        // TODO: Execute selected command/action
        ComponentResult.dismiss
      case _ => ComponentResult.noChange
