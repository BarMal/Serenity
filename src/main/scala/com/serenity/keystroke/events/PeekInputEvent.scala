package com.serenity.keystroke.events

sealed trait PeekInputEvent extends Event

object PeekInputEvent:
  case class Navigate(direction: Direction) extends PeekInputEvent
  case object Accept                        extends PeekInputEvent
  case object Dismiss                       extends PeekInputEvent
  case object OtherInput                    extends PeekInputEvent

  def fromEvent(event: Event): Option[PeekInputEvent] =
    event match
      case MoveUp                  => Some(Navigate(Direction.Up))
      case MoveDown                => Some(Navigate(Direction.Down))
      case MoveLeft                => Some(Navigate(Direction.Left))
      case MoveRight               => Some(Navigate(Direction.Right))
      case Enter | NewLine         => Some(Accept)
      case Escape                  => Some(Dismiss)
      case InsertChar(_)           => Some(OtherInput)
      case DeleteBackward          => Some(OtherInput)
      case DeleteForward           => Some(OtherInput)
      case TabKey                  => Some(OtherInput)
      case ReverseTabKey           => Some(OtherInput)
      case peekEvent: PeekInputEvent => Some(peekEvent)
      case _                       => None
