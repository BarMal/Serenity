package com.serenity.state.components

import com.serenity.command.{Command, CommandCategory, CommandIntent, CommentsIntent}
import com.serenity.keystroke.events.*
import com.serenity.state.models.*
import com.serenity.ui.widget.{TextField, WidgetInput}

class CommentLensComponent extends TypedFocusedComponent[ModalInputEvent]:

  protected def decodeEvent(event: Event): Option[ModalInputEvent] =
    ModalInputEvent.fromEvent(event)

  protected def processTypedEvent(event: ModalInputEvent, state: AppState): ComponentResult =
    state.commentLensSurface match
      case None => ComponentResult.dismiss
      case Some(surface) =>
        surface.content match
          case SurfaceContent.CommentLens(lens @ CommentLensState(_, _, _, Some(target), CommentLensMode.Editable)) =>
            event match
              case ModalSubmit =>
                saveAndDismiss(state, surface, target, lens.draft)
              case ModalDismiss =>
                ComponentResult.updateState(_ => dismiss(state, surface))
              case other =>
                editing(other).fold(ComponentResult.noChange) { input =>
                  ComponentResult.updateState(_ => replaceLens(state, surface, edited(lens, input)))
                }
          case SurfaceContent.CommentLens(_) =>
            // A read-only lens only responds to dismiss; entering edit state is a mouse gesture (click-in-body),
            // handled by `CommentLensMouseHitTesting` before this component ever sees a keystroke. Every other key
            // belongs to the editor beneath it (#1674).
            event match
              case ModalDismiss =>
                ComponentResult.updateState(_ => dismiss(state, surface))
              case _ =>
                ComponentResult.unhandled
          case _ =>
            ComponentResult.noChange

  private def editing(event: ModalInputEvent): Option[WidgetInput] =
    event match
      case ModalInsertChar(char)          => Some(WidgetInput.Insert(char))
      case ModalDeleteBackward            => Some(WidgetInput.DeleteBackward)
      case ModalDeleteForward             => Some(WidgetInput.DeleteForward)
      case ModalDeleteWordBackward        => Some(WidgetInput.DeleteWordBackward)
      case ModalDeleteWordForward         => Some(WidgetInput.DeleteWordForward)
      case ModalNavigate(Direction.Left)  => Some(WidgetInput.Left)
      case ModalNavigate(Direction.Right) => Some(WidgetInput.Right)
      case _                              => None

  private def edited(lens: CommentLensState, input: WidgetInput): CommentLensState =
    val (field, _) = TextField(lens.draft).movedTo(lens.clampedCursor).update(input)
    lens.copy(draft = field.text, cursor = field.caret)

  private def replaceLens(state: AppState, surface: UiSurface, lens: CommentLensState): AppState =
    state.copy(runtime =
      state.runtime.copy(uiSurfaces =
        state.runtime.uiSurfaces.replacedWhere(_.id == surface.id)(_.copy(content = SurfaceContent.CommentLens(lens)))
      )
    )

  /** The text is written by a command, so the edit is stamped from the effect clock this layer does not have. */
  private def saveAndDismiss(
    state: AppState,
    surface: UiSurface,
    target: CommentLensTarget,
    draft: String
  ): ComponentResult =
    ComponentResult.composite(
      ComponentResult.updateState(_ => dismiss(state, surface)),
      ComponentResult.executeCommand(
        Command.typed(
          "save-comment-draft",
          "Save the comment lens draft.",
          CommandIntent.Comments(CommentsIntent.SaveCommentDraft(target.id, draft)),
          CommandCategory.Edit
        )
      )
    )

  private def dismiss(state: AppState, surface: UiSurface): AppState =
    val withoutLens =
      state.copy(runtime = state.runtime.copy(uiSurfaces = state.runtime.uiSurfaces.filterNot(_.id == surface.id)))
    withoutLens.popFocus

end CommentLensComponent
