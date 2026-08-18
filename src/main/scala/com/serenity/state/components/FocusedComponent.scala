package com.serenity.state.components

import com.serenity.keystroke.events.Event
import com.serenity.state.models.AppState

trait FocusedComponent:
  def processEvent(event: Event, currentState: AppState): ComponentResult

trait LocalEventHandler:
  type E <: Event

  protected def decodeEvent(event: Event): Option[E]

  protected def processTypedEvent(event: E, currentState: AppState): ComponentResult

  protected def processFallbackEvent(event: Event, currentState: AppState): ComponentResult =
    ComponentResult.noChange

  final def processEvent(event: Event, currentState: AppState): ComponentResult =
    decodeEvent(event) match
      case Some(typedEvent) =>
        processTypedEvent(typedEvent, currentState)
      case None =>
        processFallbackEvent(event, currentState)

object NoOpLocalEventHandler extends LocalEventHandler:
  final type E = Nothing

  protected def decodeEvent(event: Event): Option[Nothing] =
    None

  protected def processTypedEvent(event: Nothing, currentState: AppState): ComponentResult =
    ComponentResult.noChange

trait TypedFocusedComponent[E0 <: Event] extends FocusedComponent, LocalEventHandler:
  final type E = E0
