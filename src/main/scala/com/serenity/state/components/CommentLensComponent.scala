package com.serenity.state.components

import com.serenity.keystroke.events.*
import com.serenity.state.models.*
import com.serenity.text.TextEditing

class CommentLensComponent extends TypedFocusedComponent[ModalInputEvent]:

  protected def decodeEvent(event: Event): Option[ModalInputEvent] =
    ModalInputEvent.fromEvent(event)

  protected def processTypedEvent(event: ModalInputEvent, state: AppState): ComponentResult =
    state.commentLensSurface match
      case None => ComponentResult.dismiss
      case Some(surface) =>
        surface.content match
          case SurfaceContent.CommentLens(lens) =>
            event match
              case ModalInsertChar(char) =>
                ComponentResult.updateState(_ => replaceLens(state, surface, insertChar(lens, char)))
              case ModalDeleteBackward =>
                ComponentResult.updateState(_ => replaceLens(state, surface, deleteBackward(lens)))
              case ModalDeleteForward =>
                ComponentResult.updateState(_ => replaceLens(state, surface, deleteForward(lens)))
              case ModalDeleteWordBackward =>
                ComponentResult.updateState(_ => replaceLens(state, surface, deleteWordBackward(lens)))
              case ModalDeleteWordForward =>
                ComponentResult.updateState(_ => replaceLens(state, surface, deleteWordForward(lens)))
              case ModalNavigate(Direction.Left) =>
                ComponentResult.updateState(_ => replaceLens(state, surface, moveCursor(lens, -1)))
              case ModalNavigate(Direction.Right) =>
                ComponentResult.updateState(_ => replaceLens(state, surface, moveCursor(lens, 1)))
              case ModalSubmit =>
                ComponentResult.updateState(_ => saveAndDismiss(state, surface, lens))
              case ModalDismiss =>
                ComponentResult.updateState(_ => dismiss(state, surface))
              case _ =>
                ComponentResult.noChange
          case _ =>
            ComponentResult.noChange

  private def insertChar(lens: CommentLensState, char: Char): CommentLensState =
    val cursor = lens.clampedCursor
    lens.copy(
      draft = lens.draft.substring(0, cursor) + char + lens.draft.substring(cursor),
      cursor = cursor + 1
    )

  private def deleteBackward(lens: CommentLensState): CommentLensState =
    val cursor = lens.clampedCursor
    if cursor == 0 then lens.copy(cursor = cursor)
    else
      lens.copy(
        draft = lens.draft.substring(0, cursor - 1) + lens.draft.substring(cursor),
        cursor = cursor - 1
      )

  private def deleteForward(lens: CommentLensState): CommentLensState =
    val cursor = lens.clampedCursor
    if cursor >= lens.draft.length then lens.copy(cursor = cursor)
    else
      lens.copy(
        draft = lens.draft.substring(0, cursor) + lens.draft.substring(cursor + 1),
        cursor = cursor
      )

  private def deleteWordBackward(lens: CommentLensState): CommentLensState =
    val cursor   = lens.clampedCursor
    val boundary = TextEditing.previousWordBoundary(lens.draft, cursor)
    lens.copy(
      draft = lens.draft.substring(0, boundary) + lens.draft.substring(cursor),
      cursor = boundary
    )

  private def deleteWordForward(lens: CommentLensState): CommentLensState =
    val cursor   = lens.clampedCursor
    val boundary = TextEditing.nextWordBoundary(lens.draft, cursor)
    lens.copy(
      draft = lens.draft.substring(0, cursor) + lens.draft.substring(boundary),
      cursor = cursor
    )

  private def moveCursor(lens: CommentLensState, delta: Int): CommentLensState =
    lens.copy(cursor = math.max(0, math.min(lens.clampedCursor + delta, lens.draft.length)))

  private def replaceLens(state: AppState, surface: UiSurface, lens: CommentLensState): AppState =
    state.copy(uiSurfaces = state.uiSurfaces.map {
      case current if current.id == surface.id =>
        current.copy(content = SurfaceContent.CommentLens(lens.copy(cursor = lens.clampedCursor)))
      case current =>
        current
    })

  private def saveAndDismiss(state: AppState, surface: UiSurface, lens: CommentLensState): AppState =
    val savedText = Option(lens.draft.trim).filter(_.nonEmpty).getOrElse("Comment")
    val withSavedComment = lens.target match
      case Some(target) =>
        state.layout.activeEditorPaneId
          .flatMap(state.layout.editorPanes.get)
          .flatMap(_.bufferId)
          .flatMap(state.buffers.get)
          .fold(state) { buffer =>
            val updatedComments = buffer.documentComments.map { comment =>
              if comment == target then comment.copy(text = savedText) else comment
            }
            if updatedComments == buffer.documentComments then state
            else
              state.copy(
                buffers = state.buffers + (buffer.id -> buffer.copy(
                  documentComments = updatedComments,
                  isDirty = true
                ))
              )
          }
      case None =>
        state
    dismiss(withSavedComment, surface)

  private def dismiss(state: AppState, surface: UiSurface): AppState =
    val withoutLens = state.copy(uiSurfaces = state.uiSurfaces.filterNot(_.id == surface.id))
    withoutLens.popFocus

end CommentLensComponent
