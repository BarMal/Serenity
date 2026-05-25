package com.serenity.state.reducers

import com.serenity.state.models.{AppState, Focus, PaneId}
import com.serenity.ui.layout.{DirectoryTreeData, PanelContent, PanelPosition, PeekContent, PinnedPanel}

object PanelStateReducer:

  def pin(content: PanelContent, position: PanelPosition, size: Int, state: AppState): ReducerResult =
    val panel = PinnedPanel(position, content, size)
    ReducerResult.noEffects(
      state.copy(
        layout = state.layout.copy(
          pinnedPanels = state.layout.pinnedPanels + (position -> panel)
        )
      )
    )

  def focus(position: PanelPosition, state: AppState): ReducerResult =
    if state.layout.pinnedPanels.contains(position) then
      ReducerResult.noEffects(state.copy(focus = Focus.PinnedPanel(position)))
    else ReducerResult.noEffects(state)

  def unpin(position: PanelPosition, state: AppState): ReducerResult =
    val nextFocus =
      if state.focus == Focus.PinnedPanel(position) then fallbackEditorFocus(state)
      else state.focus

    ReducerResult.noEffects(
      state.copy(
        layout = state.layout.copy(pinnedPanels = state.layout.pinnedPanels - position),
        focus = nextFocus
      )
    )

  def pinPeekOverlay(position: PanelPosition, state: AppState): ReducerResult =
    state.peekOverlay match
      case Some(overlay) =>
        overlay.content match
          case PeekContent.DirectoryListing(path, _) =>
            val panel = PinnedPanel(
              position,
              PanelContent.DirectoryTree(DirectoryTreeData(path), Some(path)),
              30
            )
            ReducerResult.noEffects(
              state.copy(
                layout = state.layout.copy(pinnedPanels = state.layout.pinnedPanels + (position -> panel)),
                peekOverlay = None,
                focus = Focus.PinnedPanel(position)
              )
            )
          case _ =>
            ReducerResult.noEffects(state.copy(peekOverlay = None))
      case None =>
        ReducerResult.noEffects(state)

  private def fallbackEditorFocus(state: AppState): Focus =
    state.layout.activeEditorPaneId match
      case Some(paneId) => Focus.EditorPane(paneId)
      case None         => Focus.EditorPane(PaneId(0))

