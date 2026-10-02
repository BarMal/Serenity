package com.serenity.state.models

import com.serenity.animation.sprite.CompanionSpriteState
import com.serenity.config.{AppConfig, MotionFamily}
import com.serenity.frontend.FrontendCapabilities
import com.serenity.project.ProjectPresence
import com.serenity.ui.layout.ViewportSize

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
    nextSurfaceId: SurfaceIdSupply = SurfaceIdSupply.initial,
    // Surface/panel/column motion state (issue #1693): the per-surface fade animation, the per-buffer column-to-column
    // transition, and the per-surface panel scale-in/out geometry are grouped into their own sub-record since all
    // three are written from the same handful of reducers/effects and read back together by
    // `DamageProducer.fullRenderDamage`/`StateManagerEditorCapability`'s tick-active check/advance. See
    // `MotionState`'s own doc comment.
    motion: MotionState = MotionState(),
    clipboard: Option[String] = None,
    focusHistory: List[Focus] = List.empty,
    navigation: NavigationHistory = NavigationHistory(),
    typingActivity: TypingActivity = TypingActivity.idle,
    // Theme discovery/loading/transition state (issue #1693): grouped into its own sub-record since the available
    // theme names, the most recently requested theme, and the in-flight transition are all written together from
    // `ThemeStateReducer`/`StateManagerSurfacePopupEffects`'s theme-listing effect and read back together by the
    // render/tick paths. See `ThemeDiscoveryState`'s own doc comment.
    themeDiscovery: ThemeDiscoveryState = ThemeDiscoveryState(),
    companionSprite: CompanionSpriteState = CompanionSpriteState.default,
    // LSP diagnostics and semantic tokens (issue #1693): grouped into their own sub-record since both are written
    // from the same `SystemEventReducer` LSP handling and read back together by `AppState.annotationIndex`/
    // `semanticTokensAvailability`.
    languageService: LanguageServiceState = LanguageServiceState(),
    // Never persisted -- set once at startup from the selected `Frontend` (see AppRuntime.run/AppStartup.initializeState,
    // issue #1669) so settings-surface rendering can hide or annotate controls that are inert in cell space
    // (post-processing effects, typography), geometry can be measured on the right grid, and
    // `CommandRunnerReducer.assignRecordedBinding` can warn when a just-recorded bare-modifier chord can't fire at the
    // negotiated keyboard tier (issue #1194) -- all without threading a `Frontend` instance itself, or `AppConfig`,
    // into the pure core.
    capabilities: FrontendCapabilities = FrontendCapabilities.gui,
    // The buffer whose Markdown preview is showing in the TUI's spawned Swing window (issue #1113), or `None` when
    // that window is closed. Unused in GUI mode, where the in-app pinned panel (`PanelId.MarkdownPreview`) is the
    // preview surface instead.
    markdownPreviewWindowBuffer: Option[BufferId] = None,
    // Pointer/gesture state (issue #1693): the editor position under the mouse, the in-progress tab-drag gesture, and
    // the experimental cursor-peek prototype's timing/anchor fields are grouped into their own sub-record since
    // `TabDragSession`'s doc comment already named all five as "every other transient mouse-interaction state" before
    // this grouping existed. See `PointerGestureState`'s own doc comment.
    pointerGesture: PointerGestureState = PointerGestureState(),
    // The UI-preset apply whose preset is still being loaded off the dispatcher (#1697), so its result can be dropped
    // once a later apply has been requested. Cleared when that request resolves.
    pendingUiPresetApply: Option[Long] = None,
    projectTasks: ProjectTasks = ProjectTasks(),
    // Refreshed each time the command palette opens, which is where project commands are offered.
    projectPresence: ProjectPresence = ProjectPresence.Unchecked,
    // Never persisted: whether a chapter note's overview is painted, faded, under an empty chapter. The notes themselves
    // are untouched by hiding the ghosts.
    chapterGhostsVisible: Boolean = true,
    // Never persisted: the pane showing chapter notes, and whether it follows the cursor's chapter or is pinned.
    notesPane: Option[NotesPane] = None
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
