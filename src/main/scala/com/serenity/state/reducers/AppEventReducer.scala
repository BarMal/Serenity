package com.serenity.state.reducers

import com.serenity.command.{CommandRegistry, CommandRunner}
import com.serenity.input.CursorPeekDetector
import com.serenity.keystroke.Modifier
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
        ReducerResult.withEffect(state, AppEffect.Surface(SurfaceEffect.OpenFileSearch))

      case CursorPeekModifierPressed(modifier, atMillis) =>
        ReducerResult.noEffects(handleCursorPeekModifierPressed(state, registry, modifier, atMillis))

      case CursorPeekModifierReleased(modifier, atMillis) =>
        ReducerResult.noEffects(handleCursorPeekModifierReleased(state, modifier, atMillis))

      case CursorPeekOtherKeyPressed =>
        ReducerResult.noEffects(handleCursorPeekOtherKeyPressed(state))

  def rebalancePanes(state: AppState, focusedBufferId: Option[BufferId] = None): AppState =
    EditorState.rebalancePanes(state, focusedBufferId)

  private def toggleCommandRunner(state: AppState, registry: CommandRegistry): AppState =
    state.commandRunnerSurface.flatMap(asCommandRunner) match
      case Some((surface, runner)) if runner.isActive =>
        state
          .copy(
            runtime = state.runtime.copy(
              uiSurfaces = state.runtime.uiSurfaces.filterNot(_.id == surface.id)
            )
          )
          .popFocus
      case _ =>
        openCommandRunner(state, registry)

  private def openCommandRunner(state: AppState, registry: CommandRegistry): AppState =
    val activatedRunner = CommandRunner.empty
      .activate(registry, state.persisted.config, state.runtime.isTuiMode, state.runtime.keyboardFidelityTier)
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
      stateWithId.runtime.uiSurfaces.filterNot(current =>
        isFileSearch(current.content) || isModalWorkflow(current.content)
      )
    stateWithId
      .copy(
        runtime = stateWithId.runtime.copy(
          uiSurfaces = upsertSurface(clearedSurfaces, surface)
        )
      )
      .pushFocus(Focus.Surface(surfaceId))

  // --- Cursor-peek prototype (issue: command-runner-cursor-peek-prototype) -----------------------------------
  //
  // Experimental, off by default (`SurfaceConfig.commandRunnerCursorPeekEnabled`). `SwingInputHandler` always emits
  // the raw `CursorPeekModifierPressed`/`Released`/`OtherKeyPressed` events regardless of the flag -- like mouse-move
  // events, the translator emits unconditionally and this reducer is where the flag actually gates behaviour: every
  // handler below bails out to an unchanged `state` first when the flag is off, so disabling it is a true no-op, not
  // just an unused code path.
  //
  // `state.runtime.cursorPeekAnchor` is the cursor position frozen at the moment a peek begins (`PeekBegin`), and is
  // plain data -- reducers may not reach into `LayoutEngine` (`ArchitectureChecks.ForbiddenImports`), so resolving it
  // to an actual on-screen position via `LayoutEngine.resolveFrozenCursorPeekStack` is layout/render work, not yet
  // wired into this prototype stage (tracked as follow-up).

  private def cursorPeekEnabled(state: AppState): Boolean =
    state.persisted.config.surfaceConfig.commandRunnerCursorPeekEnabled

  private def peekModifier(state: AppState): Modifier =
    state.persisted.config.surfaceConfig.commandRunnerCursorPeekModifier

  private def handleCursorPeekModifierPressed(
    state: AppState,
    registry: CommandRegistry,
    modifier: Modifier,
    atMillis: Long
  ): AppState =
    if !cursorPeekEnabled(state) then state
    else
      CursorPeekDetector.modifierPressed(state.runtime.cursorPeekSession, modifier, peekModifier(state), atMillis) match
        case CursorPeekDetector.Outcome.PeekBegin(next) =>
          state.copy(runtime =
            state.runtime.copy(cursorPeekSession = next, cursorPeekAnchor = state.activeCursorPosition)
          )
        case CursorPeekDetector.Outcome.DoubleTapOpen(next) =>
          val cleared = state.copy(runtime = state.runtime.copy(cursorPeekSession = next, cursorPeekAnchor = None))
          openCommandRunnerFully(cleared, registry)
        case CursorPeekDetector.Outcome.PeekEnd(next) =>
          state.copy(runtime = state.runtime.copy(cursorPeekSession = next, cursorPeekAnchor = None))
        case CursorPeekDetector.Outcome.Unchanged(next) =>
          state.copy(runtime = state.runtime.copy(cursorPeekSession = next))

  private def handleCursorPeekModifierReleased(state: AppState, modifier: Modifier, atMillis: Long): AppState =
    if !cursorPeekEnabled(state) then state
    else
      CursorPeekDetector.modifierReleased(state.runtime.cursorPeekSession, modifier, peekModifier(state), atMillis) match
        case CursorPeekDetector.Outcome.PeekEnd(next) =>
          state.copy(runtime = state.runtime.copy(cursorPeekSession = next, cursorPeekAnchor = None))
        case CursorPeekDetector.Outcome.Unchanged(next) =>
          state.copy(runtime = state.runtime.copy(cursorPeekSession = next))
        // A release never begins a peek or opens the runner -- kept exhaustive rather than partial.
        case CursorPeekDetector.Outcome.PeekBegin(next) =>
          state.copy(runtime = state.runtime.copy(cursorPeekSession = next))
        case CursorPeekDetector.Outcome.DoubleTapOpen(next) =>
          state.copy(runtime = state.runtime.copy(cursorPeekSession = next, cursorPeekAnchor = None))

  private def handleCursorPeekOtherKeyPressed(state: AppState): AppState =
    if !cursorPeekEnabled(state) then state
    else
      CursorPeekDetector.otherKeyPressed(state.runtime.cursorPeekSession) match
        case CursorPeekDetector.Outcome.PeekEnd(next) =>
          state.copy(runtime = state.runtime.copy(cursorPeekSession = next, cursorPeekAnchor = None))
        case CursorPeekDetector.Outcome.Unchanged(next) =>
          state.copy(runtime = state.runtime.copy(cursorPeekSession = next))
        // otherKeyPressed never begins a peek or opens the runner -- kept exhaustive rather than partial.
        case CursorPeekDetector.Outcome.PeekBegin(next) =>
          state.copy(runtime = state.runtime.copy(cursorPeekSession = next))
        case CursorPeekDetector.Outcome.DoubleTapOpen(next) =>
          state.copy(runtime = state.runtime.copy(cursorPeekSession = next, cursorPeekAnchor = None))

  /** A double-tap always means "open fully", not toggle-close -- unlike `ToggleCommandRunner`, a second gesture that
    * lands while the runner happens to already be open (e.g. opened some other way mid-peek) is just a no-op, never
    * a close.
    */
  private def openCommandRunnerFully(state: AppState, registry: CommandRegistry): AppState =
    state.commandRunnerSurface.flatMap(asCommandRunner) match
      case Some((_, runner)) if runner.isActive => state
      case _                                    => openCommandRunner(state, registry)

  private def toggleContextualToolbar(state: AppState): AppState =
    if !state.persisted.config.surfaceConfig.contextualToolbarEnabled then state
    else
      state.contextualToolbarSurface match
        case Some(surface) =>
          val cleared =
            state.copy(runtime =
              state.runtime.copy(uiSurfaces = state.runtime.uiSurfaces.filterNot(_.id == surface.id))
            )
          if state.persisted.focus == Focus.Surface(surface.id) then cleared.popFocus
          else cleared
        case None =>
          val items = ContextualToolbar.itemsFor(state)
          if items.isEmpty then state
          else
            val (stateWithId, surfaceId) = state.allocateSurfaceId
            val surface = UiSurface(
              id = surfaceId,
              content = SurfaceContent.ContextualToolbar(
                ContextualToolbarState(displayMode = state.persisted.config.surfaceConfig.contextualToolbarDisplayMode)
              ),
              presentation = SurfacePresentation.Floating(state.activeCursorPosition, SurfacePlacement.BelowCursor)
            )
            stateWithId
              .copy(runtime =
                stateWithId.runtime.copy(uiSurfaces = upsertSurface(stateWithId.runtime.uiSurfaces, surface))
              )

  private def closeTabState(state: AppState, registry: CommandRegistry): AppState =
    val closedState = EditorState.closeFocusedTab(state)
    if closedState.persisted.layout.activeEditorPaneId.isDefined then closedState
    else toggleCommandRunner(closedState, registry)

  private def asCommandRunner(surface: UiSurface): Option[(UiSurface, CommandRunner)] =
    surface.content match
      case SurfaceContent.CommandPalette(runner) => Some((surface, runner))
      case _                                     => None

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
