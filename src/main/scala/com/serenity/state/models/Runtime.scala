package com.serenity.state.models

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
    clipboard: Option[String] = None,
    // Never persisted: recent copies for Paste from History. Its newest entry also says whether `clipboard` still holds
    // a whole-line copy, which pastes above the caret line (#1962).
    clipboardHistory: ClipboardHistory = ClipboardHistory.empty,
    focusHistory: List[Focus] = List.empty,
    navigation: NavigationHistory = NavigationHistory(),
    typingActivity: TypingActivity = TypingActivity.idle,
    editClock: EditClock = EditClock(),
    // Theme discovery/loading state (issue #1693): grouped into its own sub-record since the available
    // theme names and the most recently requested theme are written together from
    // `ThemeStateReducer`/`StateManagerSurfacePopupEffects`'s theme-listing effect. See `ThemeDiscoveryState`'s own doc comment.
    themeDiscovery: ThemeDiscoveryState = ThemeDiscoveryState(),
    // LSP diagnostics and semantic tokens (issue #1693): grouped into their own sub-record since both are written
    // from the same `SystemEventReducer` LSP handling and read back together by `AppState.annotationIndex`/
    // `semanticTokensAvailability`.
    languageService: LanguageServiceState = LanguageServiceState(),
    // Never persisted -- set once at startup from the selected `Frontend` (see AppRuntime.run/AppStartup.initializeState,
    // issue #1669) so settings-surface rendering can hide or annotate controls that are inert in cell space
    // (typography), geometry can be measured on the right grid, and
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
    // Never persisted: set once at startup when this launch is in safe mode, so the status line can say so.
    safeMode: Boolean = false,
    // Refreshed each time the command palette opens, which is where project commands are offered.
    projectPresence: ProjectPresence = ProjectPresence.Unchecked,
    // Never persisted: whether a chapter note's overview is painted, faded, under an empty chapter. The notes themselves
    // are untouched by hiding the ghosts.
    chapterGhostsVisible: Boolean = true,
    // Never persisted: the pane showing chapter notes, and whether it follows the cursor's chapter or is pinned.
    notesPane: Option[NotesPane] = None,
    // Never persisted: the paned buffers' indexes as of the last commit (#1864), so the copies made in between reuse
    // them. See `AppState.withBufferIndexesRefreshed`.
    bufferIndexMemos: BufferIndexMemos = BufferIndexMemos.empty,
    // Never persisted: the notes pane's source headings as of its last retarget (#1848). See `NotesPaneSync`.
    chapterHeadingMemo: ChapterHeadingMemo = ChapterHeadingMemo.empty
):

  /** A typed character restarts the quiet window for cursor-adjacent surfaces. */
  def observeTyping(nowNanos: Long): Runtime =
    copy(typingActivity = typingActivity.observed(nowNanos))

  def observeEditKey(nowNanos: Long): Runtime =
    copy(editClock = editClock.observed(nowNanos))
