package com.serenity.state.reducers

import com.serenity.keystroke.events.Direction
import com.serenity.state.models.{AppState, Focus}
import com.serenity.ui.layout.{DirectionalFocusLayout, FocusTarget, LayoutEngine, ViewportSize}

object DirectionalFocus:

  /** Focus moved to the nearest editor pane or docked panel in `direction`. Unchanged while a panel is maximised (it is
    * the only thing showing), while a floating surface holds focus, or when nothing lies that way.
    */
  def moved(state: AppState, direction: Direction): AppState =
    if state.persisted.layout.maximizedWorkspaceNodeId.isDefined then state
    else
      val layout = LayoutEngine.calculateLayoutWithUI(state, state.runtime.viewportSize.getOrElse(ViewportSize(80, 24)))
      val targets = DirectionalFocusLayout.targets(state, layout)
      val next = for
        from     <- focusedTarget(state)
        fromRect <- targets.collectFirst { case (`from`, rect) => rect }
        target   <- DirectionalFocusLayout.neighbour(fromRect, targets.filterNot(_._1 == from), direction)
      yield focusedOn(state, target)
      next.getOrElse(state)

  private def focusedTarget(state: AppState): Option[FocusTarget] =
    state.persisted.focus match
      case Focus.EditorPane(paneId) => Some(FocusTarget.Pane(paneId))
      case Focus.Surface(surfaceId) => Some(FocusTarget.Panel(surfaceId))
      case Focus.Modal              => None

  private def focusedOn(state: AppState, target: FocusTarget): AppState =
    target match
      case FocusTarget.Pane(paneId) =>
        state.copy(persisted =
          state.persisted.copy(
            focus = Focus.EditorPane(paneId),
            layout = state.persisted.layout.copy(activeEditorPaneId = Some(paneId))
          )
        )
      case FocusTarget.Panel(surfaceId) =>
        PanelStateReducer.focus(surfaceId, state).state
