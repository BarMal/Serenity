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
        ReducerResult.noEffects(closeTabState(state, registry))

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
          uiSurfaces = state.uiSurfaces.filterNot { current =>
            current.id == surface.id || current.content.isInstanceOf[SurfaceContent.CommandPaletteSubmenu]
          },
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
        val clearedSurfaces = stateWithId.uiSurfaces.filterNot { current =>
          current.content.isInstanceOf[SurfaceContent.FileSearch] ||
          current.content.isInstanceOf[SurfaceContent.ModalWorkflow]
        }
        stateWithId.copy(
          uiSurfaces = upsertSurface(clearedSurfaces, surface),
          focus = Focus.Surface(surfaceId)
        )

  private def closeTabState(state: AppState, registry: CommandRegistry)(using com.serenity.rope.Balance): AppState =
    val closedState = EditorState.closeFocusedTab(state)
    if closedState.layout.activeEditorPaneId.isDefined then closedState
    else toggleCommandRunner(closedState, registry)

  private def asCommandRunner(surface: UiSurface): Option[(UiSurface, CommandRunner)] =
    surface.content match
      case SurfaceContent.CommandPalette(runner) => Some((surface, runner))
      case _                                     => None

  private def upsertSurface(surfaces: List[UiSurface], surface: UiSurface): List[UiSurface] =
    surfaces.filterNot(_.id == surface.id) :+ surface
