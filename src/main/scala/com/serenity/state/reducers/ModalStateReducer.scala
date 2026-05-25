package com.serenity.state.reducers

import com.serenity.state.models.{AppState, Focus, Modal, ModalType, PaneId}

object ModalStateReducer:

  def show(modal: Modal, state: AppState): ReducerResult =
    ReducerResult.noEffects(
      state.copy(
        modal = Some(modal),
        focus = Focus.Modal(modalType(modal))
      )
    )

  def dismiss(state: AppState): ReducerResult =
    ReducerResult.noEffects(
      state.copy(
        modal = None,
        focus = fallbackEditorFocus(state)
      )
    )

  private def modalType(modal: Modal): ModalType =
    modal match
      case Modal.CommandRunner(_, _, _) => ModalType.CommandPalette
      case Modal.FileSearch(_, _, _)    => ModalType.FileSearch
      case Modal.GotoLine(_)            => ModalType.GotoLine
      case Modal.Find(_, _, _)          => ModalType.Find

  private def fallbackEditorFocus(state: AppState): Focus =
    state.layout.activeEditorPaneId match
      case Some(paneId) => Focus.EditorPane(paneId)
      case None         => Focus.EditorPane(PaneId(0))

