package com.serenity.state.components

import com.serenity.keystroke.events.*
import com.serenity.keystroke.{InputKey, KeyStrokeInfo}
import com.serenity.state.models.*
import com.serenity.state.reducers.{ModalEventReducer, Reducer}

class ModalComponent(
    modalType: ModalType
) extends TypedFocusedComponent[ModalInputEvent]:
  private val reducer: Reducer[ModalInputEvent] = ModalEventReducer.reducer(modalType)

  protected def decodeEvent(event: Event): Option[ModalInputEvent] =
    modalType match
      case ModalType.Custom(_) => None
      case _                   => ModalInputEvent.fromEvent(event)

  protected def processTypedEvent(event: ModalInputEvent, currentState: AppState): ComponentResult =
    ComponentResult.reducerResult(reducer.reduce(event, currentState))

  override protected def processFallbackEvent(event: Event, currentState: AppState): ComponentResult =
    modalType match
      case ModalType.Custom(name) =>
        processCustomModalEvent(name, event, currentState)
      case _ =>
        ComponentResult.noChange

  private def processCustomModalEvent(name: String, event: Event, currentState: AppState): ComponentResult =
    event match
      case textEvent: TextEntryEvent => processModalTextEvent(textEvent)
      case UnhandledEvent(info, _)   => processModalKeyInfo(info)
      case _                         => ComponentResult.noChange

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

  private def processModalKeyInfo(info: KeyStrokeInfo): ComponentResult =
    info.keyType match
      case InputKey.Escape => ComponentResult.dismiss
      case InputKey.Enter  => ComponentResult.dismiss
      case _               => ComponentResult.noChange
