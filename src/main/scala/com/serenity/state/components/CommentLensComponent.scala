package com.serenity.state.components

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
                ComponentResult.updateState(_ => saveAndDismiss(state, surface, target, lens.draft))
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

  private def saveAndDismiss(state: AppState, surface: UiSurface, target: CommentLensTarget, draft: String): AppState =
    dismiss(savedDraft(state, target, draft.trim), surface)

  /** An emptied draft deletes the comment. A target whose slot no longer holds the comment the lens opened on (the list
    * was restructured underneath it) is left alone rather than overwriting whichever comment now sits there.
    */
  private def savedDraft(state: AppState, target: CommentLensTarget, text: String): AppState =
    state.persisted.layout.activeEditorPaneId
      .flatMap(state.persisted.layout.editorPanes.get)
      .flatMap(_.bufferId)
      .flatMap(state.persisted.buffers.get)
      .fold(state) { buffer =>
        val comments = buffer.annotations.documentComments
        comments.lift(target.index).filter(_.text == target.comment.text).fold(state) { current =>
          val updatedComments =
            if text.isEmpty then comments.patch(target.index, Nil, 1)
            else comments.updated(target.index, current.copy(text = text))
          if updatedComments == comments then state
          else
            state.copy(persisted =
              state.persisted.copy(buffers =
                state.persisted.buffers + (buffer.id -> buffer.copy(
                  annotations = buffer.annotations.copy(documentComments = updatedComments),
                  document = buffer.document.copy(isDirty = true)
                ))
              )
            )
        }
      }

  private def dismiss(state: AppState, surface: UiSurface): AppState =
    val withoutLens =
      state.copy(runtime = state.runtime.copy(uiSurfaces = state.runtime.uiSurfaces.filterNot(_.id == surface.id)))
    withoutLens.popFocus

end CommentLensComponent
