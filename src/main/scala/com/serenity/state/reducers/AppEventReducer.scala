package com.serenity.state.reducers

import com.serenity.command.{CommandRegistry, CommandRunner}
import com.serenity.keystroke.events.*
import com.serenity.state.core.EditorState
import com.serenity.state.models.*

object AppEventReducer:

  def reduce(
    event: GlobalAppEvent,
    state: AppState,
    registry: CommandRegistry
  )(using com.serenity.rope.Balance): ReducerResult =
    event match
      case Quit =>
        ReducerResult.withEffect(state, AppEffect.CompleteQuit)

      case ToggleCommandRunner =>
        if state.startPageSurface.isDefined then ReducerResult.noEffects(state)
        else ReducerResult.noEffects(toggleCommandRunner(state, registry))

      case NewTab =>
        ReducerResult.noEffects(EditorState.openNewTab(state))

      case CloseTab =>
        ReducerResult.noEffects(closeFocusedTab(state))

      case NextTab =>
        ReducerResult.noEffects(EditorState.navigateToNextBuffer(state))

      case PreviousTab =>
        ReducerResult.noEffects(EditorState.navigateToPreviousBuffer(state))

      case FileSearch =>
        ReducerResult.withEffect(state, AppEffect.OpenFileSearch)

  def rebalancePanes(state: AppState, focusedBufferId: Option[BufferId] = None): AppState =
    EditorState.rebalancePanes(state, focusedBufferId)

  private def toggleCommandRunner(state: AppState, registry: CommandRegistry): AppState =
    state.commandRunnerSurface.flatMap(asCommandRunner) match
      case Some((surface, runner)) if runner.isActive =>
        val previousFocus = runner.previousFocus.getOrElse(Focus.EditorPane(PaneId(0)))
        state.copy(
          uiSurfaces = state.uiSurfaces.filterNot(_.id == surface.id),
          focus = previousFocus
        )
      case _ =>
        val activatedRunner = CommandRunner.empty
          .activate(registry, state.config)
          .withPreviousFocus(state.focus)
        val (stateWithId, surfaceId) =
          state.commandRunnerSurface.map(surface => (state, surface.id)).getOrElse(state.allocateSurfaceId)
        val surface = UiSurface(
          id = surfaceId,
          content = SurfaceContent.CommandPalette(activatedRunner),
          presentation = SurfacePresentation.Floating(state.activeCursorPosition, SurfacePlacement.BelowCursor)
        )
        stateWithId.copy(
          uiSurfaces = upsertSurface(stateWithId.uiSurfaces, surface),
          focus = Focus.Surface(surfaceId)
        )

  private def closeFocusedTab(state: AppState)(using com.serenity.rope.Balance): AppState =
    state.focus match
      case Focus.EditorPane(paneId) =>
        state.layout.editorPanes.get(paneId) match
          case Some(pane) =>
            pane.bufferId match
              case Some(bufferId) =>
                closePaneState(rebalancePanes(closeBufferState(state, bufferId)), paneId)
              case None =>
                closePaneState(state, paneId)
          case None =>
            state
      case _ =>
        state

  private def closeBufferState(state: AppState, bufferId: BufferId): AppState =
    val updatedPanes = state.layout.editorPanes.view.mapValues { pane =>
      if pane.bufferId.contains(bufferId) then pane.copy(bufferId = None) else pane
    }.toMap

    state.copy(
      buffers = state.buffers - bufferId,
      layout = state.layout.copy(editorPanes = updatedPanes)
    )

  private def closePaneState(state: AppState, paneId: PaneId): AppState =
    val updatedPanes = state.layout.editorPanes - paneId
    val updatedOrder = state.layout.paneOrder.filterNot(_ == paneId)
    val newActivePaneId =
      if state.layout.activeEditorPaneId.contains(paneId) then
        val idx = state.layout.orderedPaneIds.indexOf(paneId)
        updatedOrder.lift(idx).orElse(updatedOrder.lastOption)
      else state.layout.activeEditorPaneId

    val baseState = state.copy(
      layout = state.layout.copy(
        editorPanes = updatedPanes,
        paneOrder = updatedOrder,
        activeEditorPaneId = newActivePaneId
      )
    )
    newActivePaneId match
      case Some(id) => baseState.copy(focus = Focus.EditorPane(id))
      case None     => toggleCommandRunner(baseState.copy(focus = Focus.EditorPane(PaneId(0))), CommandRegistry.default)

  private def asCommandRunner(surface: UiSurface): Option[(UiSurface, CommandRunner)] =
    surface.content match
      case SurfaceContent.CommandPalette(runner) => Some((surface, runner))
      case _                                     => None

  private def upsertSurface(surfaces: List[UiSurface], surface: UiSurface): List[UiSurface] =
    surfaces.filterNot(_.id == surface.id) :+ surface
