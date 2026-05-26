package com.serenity.state.components

import com.googlecode.lanterna.input.KeyType
import com.serenity.keystroke.KeyStrokeInfo
import com.serenity.keystroke.events.*
import com.serenity.state.models.*
import com.serenity.state.reducers.ModalEventReducer

class ModalComponent(
    modalType: ModalType
) extends FocusedComponent:

  def processEvent(event: Event, currentState: AppState): ComponentResult =
    modalType match
      case ModalType.GotoLine =>
        ComponentResult.reducerResult(ModalEventReducer.reduce(ModalType.GotoLine, event, currentState))
      case ModalType.Find =>
        ComponentResult.reducerResult(ModalEventReducer.reduce(ModalType.Find, event, currentState))
      case ModalType.FileWorkflow =>
        ComponentResult.reducerResult(ModalEventReducer.reduce(ModalType.FileWorkflow, event, currentState))
      case ModalType.ReplaceWorkflow =>
        ComponentResult.reducerResult(ModalEventReducer.reduce(ModalType.ReplaceWorkflow, event, currentState))
      case ModalType.CloseWorkflow =>
        ComponentResult.reducerResult(ModalEventReducer.reduce(ModalType.CloseWorkflow, event, currentState))
      case ModalType.Custom(name) =>
        processCustomModalEvent(name, event, currentState)

  private def processCustomModalEvent(name: String, event: Event, currentState: AppState): ComponentResult =
    event match
      case textEvent: TextEntryEvent => processModalTextEvent(textEvent)
      case UnhandledEvent(keyStroke, _) =>
        val keyInfo = KeyStrokeInfo.fromKeyStroke(keyStroke)
        processModalKeyStroke(keyInfo)
      case _ => ComponentResult.noChange

  private def processModalTextEvent(event: TextEntryEvent): ComponentResult =
    event match
      case InsertChar(_) =>
        ComponentResult.noChange
      case DeleteBackward =>
        ComponentResult.noChange
      case MoveUp | MoveDown =>
        ComponentResult.noChange
      case _ =>
        ComponentResult.noChange

  private def processModalKeyStroke(keyInfo: KeyStrokeInfo): ComponentResult =
    keyInfo.keyType match
      case KeyType.Escape => ComponentResult.dismiss
      case KeyType.Enter  => ComponentResult.dismiss
      case _              => ComponentResult.noChange
