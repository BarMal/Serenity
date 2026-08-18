package com.serenity.keystroke.events

sealed trait PanelInputEvent extends Event

object PanelInputEvent:
  final case class Navigate(direction: Direction) extends PanelInputEvent
  case object ReturnFocus                         extends PanelInputEvent
  case object Activate                            extends PanelInputEvent
  case object NoOp                                extends PanelInputEvent

  def fromEvent(event: Event): Option[PanelInputEvent] =
    event match
      case MoveUp                      => Some(Navigate(Direction.Up))
      case MoveDown                    => Some(Navigate(Direction.Down))
      case MoveLeft                    => Some(Navigate(Direction.Left))
      case MoveRight                   => Some(Navigate(Direction.Right))
      case InsertChar(_)               => Some(ReturnFocus)
      case DeleteBackward              => Some(ReturnFocus)
      case DeleteForward               => Some(ReturnFocus)
      case TabKey                      => Some(ReturnFocus)
      case ReverseTabKey               => Some(ReturnFocus)
      case Escape                      => Some(ReturnFocus)
      case Enter | NewLine             => Some(Activate)
      case panelEvent: PanelInputEvent => Some(panelEvent)
      case _                           => None
