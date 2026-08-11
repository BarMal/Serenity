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
        ReducerResult.withEffect(state, AppEffect.CompleteQuit())

      case ToggleCommandRunner =>
        if state.startPageSurface.isDefined then ReducerResult.noEffects(state)
        else ReducerResult.noEffects(toggleCommandRunner(state, registry))

      case ToggleContextualToolbar =>
        if state.startPageSurface.isDefined then ReducerResult.noEffects(state)
        else ReducerResult.noEffects(toggleContextualToolbar(state))

      case NewTab =>
        ReducerResult.noEffects(EditorState.openNewTab(state))

      case CloseTab =>
        ReducerResult.noEffects(closeTabState(state, registry))

      case NextTab =>
        ReducerResult.noEffects(EditorState.navigateToNextBuffer(state))

      case PreviousTab =>
        ReducerResult.noEffects(EditorState.navigateToPreviousBuffer(state))

      case FileSearch =>
        ReducerResult.withEffect(state, AppEffect.OpenFileSearch())

  def rebalancePanes(state: AppState, focusedBufferId: Option[BufferId] = None): AppState =
    EditorState.rebalancePanes(state, focusedBufferId)

  private def toggleCommandRunner(state: AppState, registry: CommandRegistry): AppState =
    state.commandRunnerSurface.flatMap(asCommandRunner) match
      case Some((surface, runner)) if runner.isActive =>
        state
          .copy(
            uiSurfaces = state.uiSurfaces.filterNot { current =>
              current.id == surface.id || isCommandPaletteSubmenu(current.content)
            }
          )
          .popFocus
      case _ =>
        val activatedRunner = CommandRunner.empty
          .activate(registry, state.config)
        val runnerWithPanelSelections = activatedRunner.copy(
          optionSelections = activatedRunner.optionSelections ++ CommandRunnerPanelSelections.fromState(state)
        )
        val (stateWithId, surfaceId) =
          state.commandRunnerSurface.map(surface => (state, surface.id)).getOrElse(state.allocateSurfaceId)
        val surface = UiSurface(
          id = surfaceId,
          content = SurfaceContent.CommandPalette(runnerWithPanelSelections),
          presentation = SurfacePresentation.Floating(state.activeCursorPosition, SurfacePlacement.BelowCursor)
        )
        val clearedSurfaces =
          stateWithId.uiSurfaces.filterNot(current => isFileSearch(current.content) || isModalWorkflow(current.content))
        stateWithId
          .copy(
            uiSurfaces = upsertSurface(clearedSurfaces, surface)
          )
          .pushFocus(Focus.Surface(surfaceId))

  private def toggleContextualToolbar(state: AppState): AppState =
    if !state.config.contextualToolbarEnabled then state
    else
      state.contextualToolbarSurface match
        case Some(surface) =>
          val cleared = state.copy(uiSurfaces = state.uiSurfaces.filterNot(_.id == surface.id))
          if state.focus == Focus.Surface(surface.id) then cleared.popFocus
          else cleared
        case None =>
          val items = ContextualToolbar.itemsFor(state)
          if items.isEmpty then state
          else
            val (stateWithId, surfaceId) = state.allocateSurfaceId
            val surface = UiSurface(
              id = surfaceId,
              content = SurfaceContent.ContextualToolbar(
                ContextualToolbarState(displayMode = state.config.contextualToolbarDisplayMode)
              ),
              presentation = SurfacePresentation.Floating(state.activeCursorPosition, SurfacePlacement.BelowCursor)
            )
            stateWithId
              .copy(uiSurfaces = upsertSurface(stateWithId.uiSurfaces, surface))

  private def closeTabState(state: AppState, registry: CommandRegistry)(using com.serenity.rope.Balance): AppState =
    val closedState = EditorState.closeFocusedTab(state)
    if closedState.layout.activeEditorPaneId.isDefined then closedState
    else toggleCommandRunner(closedState, registry)

  private def asCommandRunner(surface: UiSurface): Option[(UiSurface, CommandRunner)] =
    surface.content match
      case SurfaceContent.CommandPalette(runner) => Some((surface, runner))
      case _                                     => None

  private def isCommandPaletteSubmenu(content: SurfaceContent): Boolean =
    content match
      case SurfaceContent.CommandPaletteSubmenu(_, _, _) => true
      case _                                             => false

  private def isFileSearch(content: SurfaceContent): Boolean =
    content match
      case SurfaceContent.FileSearch(_) => true
      case _                            => false

  private def isModalWorkflow(content: SurfaceContent): Boolean =
    content match
      case SurfaceContent.ModalWorkflow(_) => true
      case _                               => false

  private def upsertSurface(surfaces: List[UiSurface], surface: UiSurface): List[UiSurface] =
    surfaces.filterNot(_.id == surface.id) :+ surface
