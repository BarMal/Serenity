package com.serenity.state.components

import com.serenity.keystroke.events.*
import com.serenity.state.manager.CursorViewport
import com.serenity.state.models.*
import com.serenity.state.reducers.{ModalEventReducer, Reducer}

class ModalComponent(
    modalType: ModalType
) extends TypedFocusedComponent[ModalInputEvent]:
  private val reducer: Reducer[ModalInputEvent] = ModalEventReducer.reducer(modalType)

  protected def decodeEvent(event: Event): Option[ModalInputEvent] =
    ModalInputEvent.fromEvent(event)

  protected def processTypedEvent(event: ModalInputEvent, currentState: AppState): ComponentResult =
    val result = reducer.reduce(event, currentState)
    ComponentResult.reducerResult(
      result.copy(state = CursorViewport.ensureVisibleCursors(currentState, result.state))
    )
