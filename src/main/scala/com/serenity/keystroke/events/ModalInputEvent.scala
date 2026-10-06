package com.serenity.keystroke.events

import com.serenity.state.models.FindOption
import com.serenity.ui.layout.SurfaceAction

sealed trait ModalInputEvent

final case class ModalInsertChar(char: Char) extends ModalInputEvent
case object ModalDeleteBackward              extends ModalInputEvent
case object ModalDeleteForward               extends ModalInputEvent
case object ModalDeleteWordBackward          extends ModalInputEvent
case object ModalDeleteWordForward           extends ModalInputEvent

/** Inserts `AppState.runtime.clipboard`, which the input layer refreshes from the system clipboard before a paste. */
case object ModalPaste                               extends ModalInputEvent
final case class ModalNavigate(direction: Direction) extends ModalInputEvent

/** Home and End: the ends of the focused text, or of a list with no text to move through. */
case object ModalLineStart extends ModalInputEvent
case object ModalLineEnd   extends ModalInputEvent

/** Ctrl+Home and Ctrl+End: the first and last item of a list, whether or not it also has text. */
case object ModalFirst extends ModalInputEvent
case object ModalLast  extends ModalInputEvent

/** PageUp (`pages = -1`) and PageDown (`pages = 1`). */
final case class ModalPage(pages: Int) extends ModalInputEvent

/** Moves the selected item of a reorderable list, rather than the selection (Alt+Up/Down by default). */
final case class ModalMove(direction: Direction) extends ModalInputEvent
case object ModalNextField                       extends ModalInputEvent
case object ModalPreviousField                   extends ModalInputEvent
case object ModalSubmit                          extends ModalInputEvent
case object ModalFindNext                        extends ModalInputEvent
case object ModalFindPrevious                    extends ModalInputEvent

/** Flips one find option -- match case, whole word or regex (Alt+C/W/R by default, as in VS Code). */
final case class ModalToggleFindOption(option: FindOption) extends ModalInputEvent
case object ModalDismiss                                   extends ModalInputEvent

/** Creates a file workflow's missing directories immediately, in one step (issue #1253) -- the explicit counterpart to
  * submitting twice (`missingPathSegments` flagged, then `confirmCreateDirectories` on a second submit). Modal-only:
  * reached solely via `ModalKeyAction.CreateDirectory`'s binding, never through the shared `FocusIntent` vocabulary
  * other surfaces translate through.
  */
case object ModalCreateDirectory extends ModalInputEvent

/** Opens the directory currently browsed in an Open dialog as a project root (issue #1525), reusing the same
  * `ExplorerEffect.OpenRoot` pin-panel machinery a UI preset's docked directory tree already uses. Modal-only, like
  * `ModalCreateDirectory`: reached solely via `ModalKeyAction.OpenAsProjectRoot`'s binding.
  */
case object ModalOpenAsProjectRoot                                     extends ModalInputEvent
final case class ModalClick(focusId: String, actionId: Option[String]) extends ModalInputEvent

/** A click on an item whose hit region carries a typed [[SurfaceAction]], sent in place of a [[ModalClick]]. */
final case class ModalActionClick(action: SurfaceAction) extends ModalInputEvent

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
        case FocusIntent.Paste               => Some(ModalPaste)

  def fromEvent(event: Event): Option[ModalInputEvent] =
    event match
      case modalEvent: ModalInputEvent => Some(modalEvent)
      case FindNext                    => Some(ModalFindNext)
      case FindPrevious                => Some(ModalFindPrevious)
      case other                       => SurfaceInput.translate[ModalInputEvent](other)
