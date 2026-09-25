package com.serenity.state.models

import com.serenity.animation.sprite.CompanionSpriteState
import com.serenity.config.{AppConfig, MotionFamily}
import com.serenity.input.CursorPeekState
import com.serenity.keystroke.KeyboardFidelityTier
import com.serenity.ui.layout.{ScreenPosition, ViewportSize}

/** State that is never persisted -- reset to defaults (or recomputed) on every session restore. */
final case class Runtime(
    uiSurfaces: List[UiSurface] = List.empty,
    // The explicit modal layer (#814): blocking dialogs (ModalStateReducer.isBlocking), ordered parent-to-topmost.
    // Structurally separate from uiSurfaces -- never persisted, matching every other transient dialog.
    modalStack: List[ModalDialog] = List.empty,
    actionStack: List[AppAction] = Nil,
    viewportSize: Option[ViewportSize] = None,
    nextBufferId: BufferId = BufferId(0),
    nextPaneId: PaneId = PaneId(0),
    nextSurfaceId: Int = 0,
    themeTransition: Option[ThemeTransition] = None,
    surfaceAnimations: Map[SurfaceId, SurfaceAnimationState] = Map.empty,
    // Column-based document layout (issue #1338, Phase 1 animation): the in-flight column-to-column transition for
    // each buffer whose active column just moved, if `MotionFamily.ColumnTransitions` is enabled. See
    // `ColumnTransitionState`'s doc comment for who seeds and advances it.
    columnTransitions: Map[BufferId, ColumnTransitionState] = Map.empty,
    // Panel scale-in/out (issue #1085 phase 1): the in-flight grow/shrink geometry for a pinned/docked panel opening
    // or closing, if `MotionFamily.PanelGeometry` is enabled -- keyed by the real panel's `SurfaceId` while opening, or
    // the transient close ghost's `SurfaceId` while closing. Independent of `surfaceAnimations`' colour fade, which is
    // gated by the separate `PinnedPanels` family; see `PinnedPanelAnimations` for who seeds this and
    // `AnimationChoreography.advancePanelGeometry` for who advances it and reclaims a completed close ghost.
    panelGeometry: Map[SurfaceId, PanelGeometryState] = Map.empty,
    clipboard: Option[String] = None,
    focusHistory: List[Focus] = List.empty,
    navigation: NavigationHistory = NavigationHistory(),
    hoveredEditorTarget: Option[HoveredEditorTarget] = None,
    typingActivity: TypingActivity = TypingActivity.idle,
    // The theme names the theme manager found on disk, listed at startup and after a reload or save; never persisted.
    availableThemeNames: List[String] = Nil,
    // The theme most recently asked for: a theme load that finishes after a newer request is dropped, not applied.
    requestedThemeName: Option[String] = None,
    companionSprite: CompanionSpriteState = CompanionSpriteState.default,
    diagnosticsState: DiagnosticsState = DiagnosticsState(),
    semanticTokensState: SemanticTokensState = SemanticTokensState(),
    // Never persisted -- set once at startup from the launch mode (see AppRuntime.run/AppStartup.initializeState) so
    // settings-surface rendering can hide or annotate controls that are inert in cell space (post-processing effects,
    // typography) without threading AppConfig itself into the command runner.
    isTuiMode: Boolean = false,
    // The buffer whose Markdown preview is showing in the TUI's spawned Swing window (issue #1113), or `None` when
    // that window is closed. Unused in GUI mode, where the in-app pinned panel (`PanelKind.MarkdownPreview`) is the
    // preview surface instead.
    markdownPreviewWindowBuffer: Option[BufferId] = None,
    // Never persisted -- set once at startup, mirroring `isTuiMode` above: `Full` in GUI mode (no protocol negotiation
    // happens there) and from `TerminalShell.keyboardProtocolTier` in TUI mode (see `TuiRuntime.run`). Consumed by
    // `CommandRunnerReducer.assignRecordedBinding` to warn when a just-recorded bare-modifier chord can't fire at the
    // negotiated tier (issue #1194).
    keyboardFidelityTier: KeyboardFidelityTier = KeyboardFidelityTier.Full,
    // Experimental cursor-peek prototype (off by default via `commandRunnerCursorPeekEnabled`), never persisted.
    // `cursorPeekSession` is `CursorPeekDetector`'s hold-vs-double-tap timing state, threaded through
    // `AppEventReducer`'s handling of `CursorPeekModifierPressed`/`Released`/`OtherKeyPressed` between separate
    // dispatches; `cursorPeekAnchor` is the cursor position frozen at the moment a peek begins (`None` once no peek
    // is showing) -- captured here as plain data because reducers may not reach into `LayoutEngine`
    // (`ArchitectureChecks.ForbiddenImports`). `cursorPeekResolvedAnchor` is that position resolved to an actual
    // on-screen position exactly once, by `CursorPeekAnchorResolution` (state.manager, which may use `LayoutEngine`)
    // after a peek begins -- cached and never re-derived for the rest of the peek session, so a reformat underneath
    // it cannot move the peek. Both clear together whenever `cursorPeekAnchor` does.
    cursorPeekSession: CursorPeekState = CursorPeekState.empty,
    cursorPeekAnchor: Option[CursorPosition] = None,
    cursorPeekResolvedAnchor: Option[ScreenPosition] = None,
    // The in-progress tab-bar drag-to-reorder gesture (issue #1079), if a primary press picked up a tab -- see
    // `TabDragSession`'s own doc comment for why this is reset by press rather than by a release this app never sees.
    tabDragSession: Option[TabDragSession] = None,
    // The UI-preset apply whose preset is still being loaded off the dispatcher (#1697), so its result can be dropped
    // once a later apply has been requested. Cleared when that request resolves.
    pendingUiPresetApply: Option[Long] = None,
    projectTasks: ProjectTasks = ProjectTasks()
):

  /** A typed character: the quiet window for cursor-adjacent surfaces always restarts; the companion sprite panel
    * reacts (issue #934 v2, merged in from the retired window sitter) only when its motion family and its own switch
    * are on.
    */
  def observeTyping(nowNanos: Long, config: AppConfig): Runtime =
    val motion = config.surfaceConfig.effectiveMotionConfiguration.family(MotionFamily.UiTransitions)
    val sprite =
      if motion.enabled && config.companionSpriteConfig.enabled then
        companionSprite.observeTyping(nowNanos, config.companionSpriteConfig)
      else companionSprite
    copy(companionSprite = sprite, typingActivity = typingActivity.observed)
