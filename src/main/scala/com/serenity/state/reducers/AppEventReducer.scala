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
    registry: CommandRegistry,
    panelRegistry: PanelRegistry = PanelRegistry.empty
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

      case ToggleShortcutsHelp =>
        if state.startPageSurface.isDefined then ReducerResult.noEffects(state)
        else ReducerResult.noEffects(toggleShortcutsHelp(state))

      case ToggleTabList =>
        if state.startPageSurface.isDefined then ReducerResult.noEffects(state)
        else ReducerResult.noEffects(toggleTabList(state))

      case ToggleRecentFilesInMode =>
        if state.startPageSurface.isDefined then ReducerResult.noEffects(state)
        else ReducerResult.noEffects(toggleRecentFilesInMode(state))

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

      case TogglePanel(id) =>
        if state.startPageSurface.isDefined then ReducerResult.noEffects(state)
        else ReducerResult.noEffects(togglePanel(state, panelRegistry, id))

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
  // plain data -- reducers may not reach into `LayoutEngine` (`ArchitectureChecks.ForbiddenImports`). Resolving it to
  // an actual on-screen position -- once, cached, never re-derived -- is `CursorPeekAnchorResolution`'s job
  // (`state.manager`, which may use `LayoutEngine`); this reducer only ever clears `cursorPeekResolvedAnchor`
  // alongside `cursorPeekAnchor`, never sets it.
  //
  // The peek surface itself (`SurfaceId.CursorPeek`, `SurfaceContent.CommandRunnerPeek`) is created on `PeekBegin`
  // and removed on `PeekEnd`/`DoubleTapOpen` -- a distinct content case from `CommandPalette` (see UiSurface.scala's
  // doc comment on why), and never pushed into focus: "look but don't touch," the editor keeps focus throughout.

  private def cursorPeekEnabled(state: AppState): Boolean =
    state.persisted.config.surfaceConfig.commandRunnerCursorPeekEnabled

  private def peekModifier(state: AppState): Modifier =
    state.persisted.config.surfaceConfig.commandRunnerCursorPeekModifier

  private def beginCursorPeek(state: AppState, registry: CommandRegistry): AppState =
    val activatedRunner = CommandRunner.empty
      .activate(registry, state.persisted.config, state.runtime.isTuiMode, state.runtime.keyboardFidelityTier)
    val peekSurface = UiSurface(
      id = SurfaceId.CursorPeek,
      content = SurfaceContent.CommandRunnerPeek(activatedRunner),
      presentation = SurfacePresentation.Floating(
        state.activeCursorPosition,
        state.persisted.config.surfaceConfig.commandRunnerCursorPeekPlacement
      )
    )
    state.copy(
      runtime = state.runtime.copy(
        cursorPeekAnchor = state.activeCursorPosition,
        uiSurfaces = upsertSurface(state.runtime.uiSurfaces, peekSurface)
      )
    )

  private def endCursorPeek(state: AppState): AppState =
    state.copy(runtime =
      state.runtime.copy(
        cursorPeekAnchor = None,
        cursorPeekResolvedAnchor = None,
        uiSurfaces = state.runtime.uiSurfaces.filterNot(_.id == SurfaceId.CursorPeek)
      )
    )

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
          val begun = beginCursorPeek(state, registry)
          begun.copy(runtime = begun.runtime.copy(cursorPeekSession = next))
        case CursorPeekDetector.Outcome.DoubleTapOpen(next) =>
          val ended = endCursorPeek(state)
          openCommandRunnerFully(ended.copy(runtime = ended.runtime.copy(cursorPeekSession = next)), registry)
        case CursorPeekDetector.Outcome.PeekEnd(next) =>
          val ended = endCursorPeek(state)
          ended.copy(runtime = ended.runtime.copy(cursorPeekSession = next))
        case CursorPeekDetector.Outcome.Unchanged(next) =>
          state.copy(runtime = state.runtime.copy(cursorPeekSession = next))

  private def handleCursorPeekModifierReleased(state: AppState, modifier: Modifier, atMillis: Long): AppState =
    if !cursorPeekEnabled(state) then state
    else
      CursorPeekDetector.modifierReleased(
        state.runtime.cursorPeekSession,
        modifier,
        peekModifier(state),
        atMillis
      ) match
        case CursorPeekDetector.Outcome.PeekEnd(next) =>
          val ended = endCursorPeek(state)
          ended.copy(runtime = ended.runtime.copy(cursorPeekSession = next))
        case CursorPeekDetector.Outcome.Unchanged(next) =>
          state.copy(runtime = state.runtime.copy(cursorPeekSession = next))
        // A release never begins a peek or opens the runner -- kept exhaustive rather than partial.
        case CursorPeekDetector.Outcome.PeekBegin(next) =>
          state.copy(runtime = state.runtime.copy(cursorPeekSession = next))
        case CursorPeekDetector.Outcome.DoubleTapOpen(next) =>
          val ended = endCursorPeek(state)
          ended.copy(runtime = ended.runtime.copy(cursorPeekSession = next))

  private def handleCursorPeekOtherKeyPressed(state: AppState): AppState =
    if !cursorPeekEnabled(state) then state
    else
      CursorPeekDetector.otherKeyPressed(state.runtime.cursorPeekSession) match
        case CursorPeekDetector.Outcome.PeekEnd(next) =>
          val ended = endCursorPeek(state)
          ended.copy(runtime = ended.runtime.copy(cursorPeekSession = next))
        case CursorPeekDetector.Outcome.Unchanged(next) =>
          state.copy(runtime = state.runtime.copy(cursorPeekSession = next))
        // otherKeyPressed never begins a peek or opens the runner -- kept exhaustive rather than partial.
        case CursorPeekDetector.Outcome.PeekBegin(next) =>
          state.copy(runtime = state.runtime.copy(cursorPeekSession = next))
        case CursorPeekDetector.Outcome.DoubleTapOpen(next) =>
          val ended = endCursorPeek(state)
          ended.copy(runtime = ended.runtime.copy(cursorPeekSession = next))

  /** A double-tap always means "open fully", not toggle-close -- unlike `ToggleCommandRunner`, a second gesture that
    * lands while the runner happens to already be open (e.g. opened some other way mid-peek) is just a no-op, never a
    * close.
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

  /** Opens or closes the shortcuts-help reference (issue #1247) at the fixed `SurfaceId.ShortcutsHelp` id, mirroring
    * `endCursorPeek`/`beginCursorPeek`'s single-instance pattern. Deliberately never pushes focus -- like
    * `SurfaceContent.CommandRunnerPeek`, this is a "look but don't touch" reference surface, so normal editing (and the
    * command runner, if already open) keeps working unchanged while it is on screen.
    */
  private def toggleShortcutsHelp(state: AppState): AppState =
    state.shortcutsHelpSurface match
      case Some(surface) =>
        state.copy(runtime = state.runtime.copy(uiSurfaces = state.runtime.uiSurfaces.filterNot(_.id == surface.id)))
      case None =>
        val surface = UiSurface(
          id = SurfaceId.ShortcutsHelp,
          content = SurfaceContent.ShortcutsHelp(ShortcutsHelpContent.build(state.persisted.config)),
          presentation = SurfacePresentation.Floating(state.activeCursorPosition, SurfacePlacement.BelowCursor)
        )
        state.copy(runtime = state.runtime.copy(uiSurfaces = upsertSurface(state.runtime.uiSurfaces, surface)))

  /** Opens or closes the tab list at the fixed `SurfaceId.TabList` id (issue #1307), mirroring `toggleShortcutsHelp`'s
    * single-instance toggle exactly.
    */
  private def toggleTabList(state: AppState): AppState =
    state.tabListSurface match
      case Some(surface) =>
        state.copy(runtime = state.runtime.copy(uiSurfaces = state.runtime.uiSurfaces.filterNot(_.id == surface.id)))
      case None =>
        val surface = UiSurface(
          id = SurfaceId.TabList,
          content = TabListContent.build(state),
          presentation = SurfacePresentation.Floating(state.activeCursorPosition, SurfacePlacement.BelowCursor)
        )
        state.copy(runtime = state.runtime.copy(uiSurfaces = upsertSurface(state.runtime.uiSurfaces, surface)))

  /** Opens or closes the "recent in this mode" list at the fixed `SurfaceId.RecentFilesInMode` id (issue #1307),
    * mirroring `toggleTabList`/`toggleShortcutsHelp`.
    */
  private def toggleRecentFilesInMode(state: AppState): AppState =
    state.recentFilesInModeSurface match
      case Some(surface) =>
        state.copy(runtime = state.runtime.copy(uiSurfaces = state.runtime.uiSurfaces.filterNot(_.id == surface.id)))
      case None =>
        val surface = UiSurface(
          id = SurfaceId.RecentFilesInMode,
          content = RecentFilesInModeContent.build(state),
          presentation = SurfacePresentation.Floating(state.activeCursorPosition, SurfacePlacement.BelowCursor)
        )
        state.copy(runtime = state.runtime.copy(uiSurfaces = upsertSurface(state.runtime.uiSurfaces, surface)))

  /** Opens or closes a registered panel's floating (command-palette) presentation at a stable per-panel surface id
    * (issue #1310), mirroring `toggleTabList`/`toggleShortcutsHelp`'s single-instance toggle exactly. A panel id with
    * no registration, or one that doesn't declare `PanelDisplayMode.Palette` support, is a no-op -- the same "target
    * doesn't apply, ignore the request" policy `StateManagerSurfaceCapability`'s panel operations already use.
    */
  private def togglePanel(state: AppState, panelRegistry: PanelRegistry, id: PanelId): AppState =
    val surfaceId = SurfaceId(s"panel-${id.value}")
    state.surfaceById(surfaceId) match
      case Some(surface) =>
        state.copy(runtime = state.runtime.copy(uiSurfaces = state.runtime.uiSurfaces.filterNot(_.id == surface.id)))
      case None =>
        panelRegistry.get(id).filter(_.supportedModes.contains(PanelDisplayMode.Palette)) match
          case Some(registration) =>
            val surface = UiSurface(
              id = surfaceId,
              content = registration.buildContent(state),
              presentation = SurfacePresentation.Floating(state.activeCursorPosition, SurfacePlacement.BelowCursor)
            )
            state.copy(runtime = state.runtime.copy(uiSurfaces = upsertSurface(state.runtime.uiSurfaces, surface)))
          case None =>
            state

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
