package com.serenity.keystroke.events

sealed trait ModalInputEvent

final case class ModalInsertChar(char: Char)         extends ModalInputEvent
case object ModalDeleteBackward                      extends ModalInputEvent
case object ModalDeleteForward                       extends ModalInputEvent
case object ModalDeleteWordBackward                  extends ModalInputEvent
case object ModalDeleteWordForward                   extends ModalInputEvent
final case class ModalNavigate(direction: Direction) extends ModalInputEvent
case object ModalNextField                           extends ModalInputEvent
case object ModalPreviousField                       extends ModalInputEvent
case object ModalSubmit                              extends ModalInputEvent
case object ModalFindNext                            extends ModalInputEvent
case object ModalDismiss                             extends ModalInputEvent

/** Creates a file workflow's missing directories immediately, in one step (issue #1253) -- the explicit counterpart to
  * submitting twice (`missingPathSegments` flagged, then `confirmCreateDirectories` on a second submit). Modal-only:
  * reached solely via `ModalKeyAction.CreateDirectory`'s binding, never through the shared `FocusIntent` vocabulary
  * other surfaces translate through.
  */
case object ModalCreateDirectory                                       extends ModalInputEvent
final case class ModalClick(focusId: String, actionId: Option[String]) extends ModalInputEvent

object ModalInputEvent:

  given SurfaceInput[ModalInputEvent] with

    def fromIntent(intent: FocusIntent): Option[ModalInputEvent] =
      intent match
        case FocusIntent.Insert(char)        => Some(ModalInsertChar(char))
        case FocusIntent.DeleteBackward      => Some(ModalDeleteBackward)
        case FocusIntent.DeleteForward       => Some(ModalDeleteForward)
        case FocusIntent.DeleteWordBackward  => Some(ModalDeleteWordBackward)
        case FocusIntent.DeleteWordForward   => Some(ModalDeleteWordForward)
        case FocusIntent.Navigate(direction) => Some(ModalNavigate(direction))
        case FocusIntent.NextGroup           => Some(ModalNextField)
        case FocusIntent.PreviousGroup       => Some(ModalPreviousField)
        case FocusIntent.Submit              => Some(ModalSubmit)
        case FocusIntent.Dismiss             => Some(ModalDismiss)
        case FocusIntent.Paste               => None

  def fromEvent(event: Event): Option[ModalInputEvent] =
    event match
      case modalEvent: ModalInputEvent => Some(modalEvent)
      case FindNext                    => Some(ModalFindNext)
      case other                       => SurfaceInput.translate[ModalInputEvent](other)
