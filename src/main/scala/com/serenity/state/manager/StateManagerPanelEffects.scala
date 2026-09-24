package com.serenity.state.manager

import java.nio.file.Path

import cats.effect.{IO, Ref}
import com.serenity.command.{PanelKind, ViewIntent}
import com.serenity.config.{AppConfig, MarkdownViewMode}
import com.serenity.io.{FileEntry, FileManager, FileUtils}
import com.serenity.keystroke.events.Event
import com.serenity.state.effects.{Lane, LaneKey, LanePolicy}
import com.serenity.state.models.*
import com.serenity.state.reducers.{PanelStateReducer, PinnedPanelContentReducer}
import com.serenity.ui.layout.{DirEntry, PanelPosition, PanelTarget, SplitAxis}
import com.serenity.ui.tui.MarkdownPreviewWindowAvailability

/** Pinned-panel management: pinning/unpinning/moving/resizing the explorer, outline, comments, diagnostics, and
  * markdown-preview panels, plus the [[ViewIntent]] entry points that drive them. The changes themselves are
  * [[PanelTransitions]]; each commits as a single validated model write.
  */
final private[manager] class StateManagerPanelEffects(
    stateRef: Ref[IO, AppState],
    logger: org.typelevel.log4cats.Logger[IO],
    fileManager: FileManager,
    lanes: EffectLanePort,
    markdownPreviewWindow: MarkdownPreviewWindowAvailability,
    updateModelValidated: (Model => Option[Model]) => IO[Unit],
    enqueueEvent: Event => IO[Unit],
    showPeek: (com.serenity.ui.layout.PeekContent, CursorPosition) => IO[Unit],
    updateConfig: (AppConfig => AppConfig) => IO[AppConfig],
    unpinPanel: PanelTarget => IO[Unit],
    expandPinnedPanel: PanelTarget => IO[Unit],
    collapseExpandedPanel: () => IO[Unit],
    switchToPinnedPanel: PanelTarget => IO[Unit],
    resizePinnedPanel: (PanelTarget, Int) => IO[Unit],
    cancelProjectTaskSilently: IO[Unit]
):

  /** Floor for command/keyboard panel resize (issue #1310) -- prevents a panel from shrinking to zero or negative
    * cells; `WorkspaceTree.resizeSurface`'s own ratio clamp is the further backstop against it eating the viewport.
    */
  private val MinimumPanelSize = 4

  private def commitModel(transition: Model => Model): IO[Unit] =
    updateModelValidated(model => Some(transition(model)))

  private def commitApp(update: AppState => AppState): IO[Unit] =
    commitModel(model => model.copy(app = update(model.app)))

  private[manager] def interpret(intent: ViewIntent, state: AppState): IO[Unit] =
    intent match
      case ViewIntent.NextTab =>
        commitApp(com.serenity.state.core.EditorState.navigateToNextBuffer)
      case ViewIntent.PreviousTab =>
        commitApp(com.serenity.state.core.EditorState.navigateToPreviousBuffer)
      case ViewIntent.SplitPaneHorizontal =>
        commitApp(com.serenity.state.core.EditorState.splitFocusedPane(_, SplitAxis.Horizontal))
      case ViewIntent.SplitPaneVertical =>
        commitApp(com.serenity.state.core.EditorState.splitFocusedPane(_, SplitAxis.Vertical))
      case ViewIntent.ClosePane =>
        commitApp(com.serenity.state.core.EditorState.removeFocusedPane)
      case ViewIntent.PinExplorerPanel =>
        setPanelPin(PanelKind.Explorer, Some(PanelPosition.Left))
      case ViewIntent.PinOutlinePanel =>
        setPanelPin(PanelKind.Outline, Some(PanelPosition.Right))
      case ViewIntent.PinCommentsPanel =>
        setPanelPin(PanelKind.Comments, Some(PanelPosition.Right))
      case ViewIntent.PinDiagnosticsPanel =>
        setPanelPin(PanelKind.Diagnostics, Some(PanelPosition.Bottom))
      case ViewIntent.OpenMarkdownPreview =>
        // In-pane preview is structurally unavailable in the TUI (cell surfaces cannot `drawImage`) -- toggle the
        // spawned Swing window there instead of pinning the GUI-only panel (issue #1113).
        if state.runtime.isTuiMode then toggleMarkdownPreviewWindow(state)
        else setPanelPin(PanelKind.MarkdownPreview, Some(PanelPosition.Right))
      case ViewIntent.SetPanelPin(kind, position) =>
        setPanelPin(kind, position)
      case ViewIntent.MovePanelEarlier(kind) =>
        commitApp(PanelTransitions.reorderPanelKind(kind, delta = -1))
      case ViewIntent.MovePanelLater(kind) =>
        commitApp(PanelTransitions.reorderPanelKind(kind, delta = 1))
      case ViewIntent.SetMarkdownViewMode(mode) =>
        setMarkdownViewMode(mode)
      case ViewIntent.SetDefaultDocumentMode(mode) =>
        updateConfig(_.withDefaultDocumentMode(mode)).void
      case ViewIntent.SetAppMode(mode) =>
        updateConfig(_.withAppMode(mode)).void
      case ViewIntent.SetShowAllSettingsRegardlessOfMode(value) =>
        updateConfig(_.withShowAllSettingsRegardlessOfMode(value)).void
      case ViewIntent.FocusPanel(position) =>
        switchToPinnedPanel(PanelTarget.ByPosition(position))
      case ViewIntent.UnpinPanel(position) =>
        unpinViewPanel(state, position)
      case ViewIntent.ExpandPanel(position) =>
        expandPinnedPanel(PanelTarget.ByPosition(position))
      case ViewIntent.CollapseExpandedPanel =>
        collapseExpandedPanel()
      case ViewIntent.ToggleShortcutsHelp =>
        enqueueEvent(com.serenity.keystroke.events.ToggleShortcutsHelp)
      case ViewIntent.ToggleTabList =>
        enqueueEvent(com.serenity.keystroke.events.ToggleTabList)
      case ViewIntent.ToggleRecentFilesInMode =>
        enqueueEvent(com.serenity.keystroke.events.ToggleRecentFilesInMode)
      case ViewIntent.TogglePanel(id) =>
        enqueueEvent(com.serenity.keystroke.events.TogglePanel(id))
      case ViewIntent.SetPanelSize(surfaceId, delta) =>
        setPanelSize(surfaceId, delta)

  private def setMarkdownViewMode(mode: MarkdownViewMode): IO[Unit] =
    val updateConfigEffect = updateConfig(_.withMarkdownViewMode(mode)).void
    mode match
      case MarkdownViewMode.SplitPreview =>
        updateConfigEffect >> openMarkdownPreview
      case MarkdownViewMode.Source | MarkdownViewMode.InlineLens =>
        updateConfigEffect >> commitApp(PanelTransitions.removePanelKind(PanelKind.MarkdownPreview))

  // Closing the project-task output panel while its task is still running must actually stop it -- otherwise
  // the task's own 100ms output-refresh tick (`runProjectTask`) just re-pins it right back (issue #1294).
  private def unpinViewPanel(state: AppState, position: PanelPosition): IO[Unit] =
    val closingRunningTaskPanel = state.pinnedSurfaces.exists { surface =>
      surface.content match
        case SurfaceContent.Terminal(_, _) =>
          state.persisted.layout.workspaceTree.flatMap(_.positionForSurface(surface.id)).contains(position)
        case _ => false
    }
    unpinPanel(PanelTarget.ByPosition(position)) >>
      (if closingRunningTaskPanel then cancelProjectTaskSilently else IO.unit)

  private[manager] def openMarkdownPreview: IO[Unit] =
    pinPanelKind(PanelKind.MarkdownPreview, PanelPosition.Right, refreshSelections = false)

  private def setPanelPin(kind: PanelKind, position: Option[PanelPosition]): IO[Unit] =
    position match
      case None =>
        commitModel(PanelTransitions.panelChange(_, PanelTransitions.removePanelKind(kind), refreshSelections = true))
      case Some(targetPosition) =>
        pinPanelKind(kind, targetPosition, refreshSelections = true)

  /** The command/keyboard resize entry point (issue #1310) onto the same `resizePinnedPanel` -- and, through it,
    * `PanelStateReducer.resize` -- the existing mouse-drag path already uses: one shared resize state fed by all three
    * input methods rather than three separate implementations. A surface that isn't currently pinned is a no-op, the
    * same policy `resizePinnedPanel`'s own target resolution already applies.
    */
  private def setPanelSize(surfaceId: SurfaceId, delta: Int): IO[Unit] =
    stateRef.get.flatMap { state =>
      PanelStateReducer.currentSize(surfaceId, state) match
        case Some(currentSize) =>
          resizePinnedPanel(PanelTarget.ById(surfaceId), math.max(MinimumPanelSize, currentSize + delta))
        case None =>
          IO.unit
    }

  /** Only the kind-based pin/unpin mutations declare an undo boundary (#1016 PR4) -- not `MovePanelEarlier`/`Later`'s
    * same-edge reordering, which adjusts an already-pinned panel rather than pinning or unpinning one.
    */
  private def pinPanelKind(kind: PanelKind, position: PanelPosition, refreshSelections: Boolean): IO[Unit] =
    stateRef.get.flatMap { state =>
      val refreshed =
        if refreshSelections then commitApp(PanelTransitions.withCommandRunnerPanelSelections) else IO.unit
      PanelTransitions.pinPlan(kind, position, state) match
        case PanelPinPlan.Commit(update) =>
          commitModel(PanelTransitions.panelChange(_, update, refreshSelections))
        case PanelPinPlan.LoadExplorerRoot(size) =>
          FileUtils.getCurrentDirectory.flatMap(pinExplorerPanelEffect(position, _, size)) >> refreshed
        case PanelPinPlan.Report(message) =>
          showQuickInfo(state, message) >> refreshed
        case PanelPinPlan.Ignore(debugLog) =>
          logger.debug(debugLog) >> refreshed
    }

  /** Toggles the TUI's spawned Swing preview window (issue #1113): closes it when already open for the focused buffer,
    * opens it (following the focused buffer) when closed, and reports unavailability -- rather than silently no-op'ing
    * -- both when no display was reachable at startup and when there is no Markdown buffer to preview.
    */
  private def toggleMarkdownPreviewWindow(state: AppState): IO[Unit] =
    markdownPreviewWindow match
      case MarkdownPreviewWindowAvailability.Unavailable =>
        showQuickInfo(state, "Markdown preview window needs a graphical display.")
      case MarkdownPreviewWindowAvailability.Available(window) =>
        state.runtime.markdownPreviewWindowBuffer match
          case Some(_) =>
            window.hide() >> commitApp(PanelTransitions.withMarkdownPreviewWindowBuffer(_, None))
          case None =>
            PanelTransitions.markdownPreviewBufferId(state) match
              case Some(bufferId) =>
                window.show() >> commitApp(PanelTransitions.withMarkdownPreviewWindowBuffer(_, Some(bufferId)))
              case None =>
                showQuickInfo(state, "Markdown preview needs an active Markdown buffer.")

  private def showQuickInfo(state: AppState, message: String): IO[Unit] =
    showPeek(
      com.serenity.ui.layout.PeekContent.QuickInfo(message),
      state.activeCursorPosition.getOrElse(CursorPosition(0, 0))
    )

  private[manager] def pinExplorerPanelEffect(position: PanelPosition, path: Path, size: Int): IO[Unit] =
    commitModel { model =>
      val pinned = PinnedPanelContentReducer.pinExplorerRoot(position, path, size, model.app)
      ModelCommit.applyModelEffects(model.copy(app = pinned.state), pinned.effects)
    } >> listDirectoryOnLane(path)(EffectResult.ExplorerRootListed(position, path, _))

  private[manager] def loadPinnedDirectoryEffect(position: PanelPosition, path: Path): IO[Unit] =
    listDirectoryOnLane(path)(EffectResult.DirectoryListed(position, path, _))

  private def listDirectoryOnLane(path: Path)(listed: List[DirEntry] => EffectResult): IO[Unit] =
    lanes.submitEffect(
      Lane.Keyed(LaneKey.Directory(path.toAbsolutePath.normalize), LanePolicy.SwitchLatest),
      fileManager
        .listDirectory(path)
        .flatMap(entries => lanes.dispatchEffectResult(listed(toDirEntries(entries)), _ => IO.unit))
        .handleErrorWith(ex => logger.error(ex)(s"[FILE] Failed to load directory $path"))
    )

  private def toDirEntries(entries: List[FileEntry]): List[DirEntry] =
    entries.map(entry => DirEntry(entry.path, entry.name, entry.isDirectory))
