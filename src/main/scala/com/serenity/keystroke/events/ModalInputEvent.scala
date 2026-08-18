package com.serenity.keystroke.events

sealed trait ModalInputEvent extends Event

final case class ModalInsertChar(char: Char)                           extends ModalInputEvent
case object ModalDeleteBackward                                        extends ModalInputEvent
case object ModalDeleteForward                                         extends ModalInputEvent
case object ModalDeleteWordBackward                                    extends ModalInputEvent
case object ModalDeleteWordForward                                     extends ModalInputEvent
final case class ModalNavigate(direction: Direction)                   extends ModalInputEvent
case object ModalNextField                                             extends ModalInputEvent
case object ModalPreviousField                                         extends ModalInputEvent
case object ModalSubmit                                                extends ModalInputEvent
case object ModalFindNext                                              extends ModalInputEvent
case object ModalDismiss                                               extends ModalInputEvent
final case class ModalClick(focusId: String, actionId: Option[String]) extends ModalInputEvent

object ModalInputEvent:

  def fromEvent(event: Event): Option[ModalInputEvent] =
    event match
      case InsertChar(char)            => Some(ModalInsertChar(char))
      case DeleteBackward              => Some(ModalDeleteBackward)
      case DeleteForward               => Some(ModalDeleteForward)
      case DeleteWordBackward          => Some(ModalDeleteWordBackward)
      case DeleteWordForward           => Some(ModalDeleteWordForward)
      case MoveUp                      => Some(ModalNavigate(Direction.Up))
      case MoveDown                    => Some(ModalNavigate(Direction.Down))
      case MoveLeft                    => Some(ModalNavigate(Direction.Left))
      case MoveRight                   => Some(ModalNavigate(Direction.Right))
      case TabKey                      => Some(ModalNextField)
      case ReverseTabKey               => Some(ModalPreviousField)
      case Enter | NewLine             => Some(ModalSubmit)
      case FindNext                    => Some(ModalFindNext)
      case Escape                      => Some(ModalDismiss)
      case modalEvent: ModalInputEvent => Some(modalEvent)
      case _                           => None
