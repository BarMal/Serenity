package com.serenity.keystroke.events

trait StartupPageEvent extends Event

case object StartupPageMoveUp   extends StartupPageEvent
case object StartupPageMoveDown extends StartupPageEvent
case object StartupPageSubmit   extends StartupPageEvent
case object StartupPageDismiss  extends StartupPageEvent
case class StartupPageSelect(index: Int) extends StartupPageEvent

object StartupPageEvent:

  def fromEvent(event: Event): Option[StartupPageEvent] =
    event match
      case MoveUp                         => Some(StartupPageMoveUp)
      case MoveDown                       => Some(StartupPageMoveDown)
      case Enter | NewLine                => Some(StartupPageSubmit)
      case Escape                         => Some(StartupPageDismiss)
      case InsertChar(char) if char.isDigit && char != '0' => Some(StartupPageSelect(char.asDigit - 1))
      case startupEvent: StartupPageEvent => Some(startupEvent)
      case _                              => None
