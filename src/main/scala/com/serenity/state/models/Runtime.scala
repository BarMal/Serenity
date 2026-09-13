package com.serenity.state.models

import com.serenity.animation.WindowSitter
import com.serenity.animation.sprite.CompanionSpriteState
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
    clipboard: Option[String] = None,
    focusHistory: List[Focus] = List.empty,
    navigation: NavigationHistory = NavigationHistory(),
    hoveredEditorTarget: Option[HoveredEditorTarget] = None,
    windowSitter: WindowSitter = WindowSitter.default,
    companionSprite: CompanionSpriteState = CompanionSpriteState(),
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
    // issue #1048: MRU (most-recently-used) command tracking, keyed by `Command.name` and valued by an incrementing
    // recency generation (see `CommandRunner.recordCommandUsage`) -- lives here, not on the transient `CommandRunner`
    // itself, specifically so it survives the palette closing and reopening (`CommandRunner.empty.activate(...)` is
    // reconstructed fresh on every open) within the same running session. Not persisted across restarts, matching
    // this whole case class's contract.
    commandUsage: Map[String, Int] = Map.empty
)
