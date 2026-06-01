package com.serenity.keystroke.events

trait StartupPageEvent extends Event

case object StartupPageMoveUp   extends StartupPageEvent
case object StartupPageMoveDown extends StartupPageEvent
case object StartupPageSubmit   extends StartupPageEvent
case object StartupPageDismiss  extends StartupPageEvent

object StartupPageEvent:

  def fromEvent(event: Event): Option[StartupPageEvent] =
    event match
      case MoveUp                         => Some(StartupPageMoveUp)
      case MoveDown                       => Some(StartupPageMoveDown)
      case Enter | NewLine                => Some(StartupPageSubmit)
      case Escape                         => Some(StartupPageDismiss)
      case startupEvent: StartupPageEvent => Some(startupEvent)
      case _                              => None
