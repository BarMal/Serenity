package com.serenity.state.manager

import java.nio.file.Path

import cats.effect.{IO, Ref}
import com.serenity.command.{PanelKind, ViewIntent}
import com.serenity.config.{AppConfig, MarkdownViewMode}
import com.serenity.io.{FileEntry, FileManager, FileUtils}
import com.serenity.keystroke.events.{Event, ExplorerEvent}
import com.serenity.lsp.config.LanguageId
import com.serenity.state.models.*
import com.serenity.state.reducers.PanelStateReducer
import com.serenity.state.undo.HistoryEntry
import com.serenity.ui.layout.{DirEntry, PanelPosition, PanelTarget, SplitAxis, WorkspaceTree}
import com.serenity.ui.tui.MarkdownPreviewWindowAvailability

/** Pinned-panel management: pinning/unpinning/moving/resizing the explorer, outline, comments, diagnostics, and
  * markdown-preview panels, plus the [[ViewIntent]] entry points that drive them.
  */
final private[manager] class StateManagerPanelEffects(
    stateRef: Ref[IO, AppState],
    logger: org.typelevel.log4cats.Logger[IO],
    fileManager: FileManager,
    markdownPreviewWindow: MarkdownPreviewWindowAvailability,
    validateAndUpdateState: (AppState, AppState) => IO[Unit],
    enqueueEvent: Event => IO[Unit],
    showPeek: (com.serenity.ui.layout.PeekContent, CursorPosition) => IO[Unit],
    updateConfig: (AppConfig => AppConfig) => IO[AppConfig],
    unpinPanel: PanelTarget => IO[Unit],
    expandPinnedPanel: PanelTarget => IO[Unit],
    collapseExpandedPanel: () => IO[Unit],
    switchToPinnedPanel: PanelTarget => IO[Unit],
    resizePinnedPanel: (PanelTarget, Int) => IO[Unit],
    cancelProjectTaskSilently: IO[Unit],
    recordUndoBoundary: (HistoryEntry, Boolean) => IO[Unit]
):

  /** Floor for command/keyboard panel resize (issue #1310) -- prevents a panel from shrinking to zero or negative
    * cells; `WorkspaceTree.resizeSurface`'s own ratio clamp is the further backstop against it eating the viewport.
    */
  private val MinimumPanelSize = 4

  private[manager] def interpret(intent: ViewIntent, state: AppState): IO[Unit] =
    intent match
      case ViewIntent.NextTab =>
        stateRef.update(com.serenity.state.core.EditorState.navigateToNextBuffer)
      case ViewIntent.PreviousTab =>
        stateRef.update(com.serenity.state.core.EditorState.navigateToPreviousBuffer)
      case ViewIntent.SplitPaneHorizontal =>
        stateRef.update(com.serenity.state.core.EditorState.splitFocusedPane(_, SplitAxis.Horizontal))
      case ViewIntent.SplitPaneVertical =>
        stateRef.update(com.serenity.state.core.EditorState.splitFocusedPane(_, SplitAxis.Vertical))
      case ViewIntent.ClosePane =>
        stateRef.update(com.serenity.state.core.EditorState.removeFocusedPane)
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
        movePanelKind(kind, delta = -1)
      case ViewIntent.MovePanelLater(kind) =>
        movePanelKind(kind, delta = 1)
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
    val updateModeEffect = mode match
      case MarkdownViewMode.SplitPreview =>
        updateConfigEffect >> openMarkdownPreview
      case MarkdownViewMode.Source | MarkdownViewMode.InlineLens =>
        updateConfigEffect >> unpinMarkdownPreviewPanel()
    updateModeEffect

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
    pinPanelKind(PanelKind.MarkdownPreview, PanelPosition.Right)

  private def setPanelPin(kind: PanelKind, position: Option[PanelPosition]): IO[Unit] =
    val updateEffect = position match
      case None =>
        updatePanelStateWithUndo(removePanelKind(kind))
      case Some(targetPosition) =>
        pinPanelKind(kind, targetPosition)
    updateEffect >> refreshCommandRunnerPanelSelections

  private def movePanelKind(kind: PanelKind, delta: Int): IO[Unit] =
    updatePanelState(reorderPanelKind(kind, delta))

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

  private def pinPanelKind(kind: PanelKind, position: PanelPosition): IO[Unit] =
    kind match
      case PanelKind.Explorer =>
        stateRef.get.flatMap { state =>
          newestPanelKindSurface(kind, state) match
            case Some(surface) =>
              updatePanelStateWithUndo(
                upsertPanelKind(kind, surface.content, position, defaultPanelSize(kind, position))
              )
            case None =>
              FileUtils.getCurrentDirectory.flatMap(path =>
                pinExplorerPanelEffect(position, path, defaultPanelSize(kind, position))
              )
        }
      case PanelKind.Outline =>
        stateRef.get.flatMap { state =>
          val symbols = PanelSymbolLookup.outlineSymbols(state)
          updatePanelStateWithUndo(
            upsertPanelKind(
              kind,
              SurfaceContent.Outline(symbols, PanelSymbolLookup.currentSymbolActiveLocation(symbols, state)),
              position,
              defaultPanelSize(kind, position)
            )
          )
        }
      case PanelKind.Comments =>
        stateRef.get.flatMap { state =>
          val symbols = PanelSymbolLookup.commentPanelSymbols(state)
          updatePanelStateWithUndo(
            upsertPanelKind(
              kind,
              SurfaceContent.Comments(symbols, PanelSymbolLookup.currentSymbolActiveLocation(symbols, state)),
              position,
              defaultPanelSize(kind, position)
            )
          )
        }
      case PanelKind.Diagnostics =>
        updatePanelStateWithUndo(
          upsertPanelKind(kind, SurfaceContent.Diagnostics(Nil), position, defaultPanelSize(kind, position))
        )
      case PanelKind.MarkdownPreview =>
        stateRef.get.flatMap { state =>
          markdownPreviewContent(state) match
            case Some(content) =>
              updatePanelStateWithUndo(upsertPanelKind(kind, content, position, defaultPanelSize(kind, position)))
            case None =>
              logger.debug("[CMD] Markdown preview requested without an active Markdown buffer")
        }

  private def markdownPreviewContent(state: AppState): Option[SurfaceContent] =
    state.focusedBufferId
      .flatMap(state.persisted.buffers.get)
      .filter(_.document.language.contains(LanguageId.Markdown))
      .map { buffer =>
        val title = buffer.document.filePath
          .flatMap(path => Option(path.getFileName).map(_.toString))
          .getOrElse("Untitled")
        SurfaceContent.MarkdownPreview(buffer.id, title)
      }

  private def markdownPreviewBufferId(state: AppState): Option[BufferId] =
    state.focusedBufferId
      .flatMap(state.persisted.buffers.get)
      .filter(_.document.language.contains(LanguageId.Markdown))
      .map(_.id)

  /** Toggles the TUI's spawned Swing preview window (issue #1113): closes it when already open for the focused buffer,
    * opens it (following the focused buffer) when closed, and reports unavailability -- rather than silently no-op'ing
    * -- both when no display was reachable at startup and when there is no Markdown buffer to preview.
    */
  private def toggleMarkdownPreviewWindow(state: AppState): IO[Unit] =
    markdownPreviewWindow match
      case MarkdownPreviewWindowAvailability.Unavailable =>
        showMarkdownPreviewUnavailablePeek(state, "Markdown preview window needs a graphical display.")
      case MarkdownPreviewWindowAvailability.Available(window) =>
        state.runtime.markdownPreviewWindowBuffer match
          case Some(_) =>
            window.hide() >> stateRef.update(s => s.copy(runtime = s.runtime.copy(markdownPreviewWindowBuffer = None)))
          case None =>
            markdownPreviewBufferId(state) match
              case Some(bufferId) =>
                window.show() >>
                  stateRef.update(s => s.copy(runtime = s.runtime.copy(markdownPreviewWindowBuffer = Some(bufferId))))
              case None =>
                showMarkdownPreviewUnavailablePeek(state, "Markdown preview needs an active Markdown buffer.")

  private def showMarkdownPreviewUnavailablePeek(state: AppState, message: String): IO[Unit] =
    showPeek(
      com.serenity.ui.layout.PeekContent.QuickInfo(message),
      state.activeCursorPosition.getOrElse(CursorPosition(0, 0))
    )

  private def updatePanelState(update: AppState => AppState): IO[Unit] =
    stateRef.get.flatMap { state =>
      val updated = update(state)
      validateAndUpdateState(updated, state)
    }

  /** Same as `updatePanelState`, but also declares the change as an undo boundary (#1016 PR4). Used only by the
    * kind-based pin/unpin mutations (`setPanelPin`/`pinPanelKind`) -- the structural pin/unpin change the acceptance
    * criterion names -- and not by `movePanelKind`'s same-edge reordering, which is a view adjustment to an
    * already-pinned panel rather than a pin/unpin. A no-op update (e.g. unpinning a kind that isn't pinned) records
    * nothing, mirroring `AppEventReducer.closePaneResult`'s guard for pane close.
    */
  private def updatePanelStateWithUndo(update: AppState => AppState): IO[Unit] =
    stateRef.get.flatMap { state =>
      val updated = update(state)
      if updated == state then validateAndUpdateState(updated, state)
      else
        val entry = HistoryEntry.PanelChange.capture(state)
        validateAndUpdateState(updated, state) >> recordUndoBoundary(entry, false)
    }

  private def removePanelKind(kind: PanelKind)(state: AppState): AppState =
    val removedIds = state.runtime.uiSurfaces.collect {
      case surface if panelKindOf(surface.content).contains(kind) => surface.id
    }.toSet
    val nextFocus = state.persisted.focus match
      case Focus.Surface(surfaceId) if removedIds.contains(surfaceId) =>
        state.persisted.layout.activeEditorPaneId.map(Focus.EditorPane.apply).getOrElse(state.persisted.focus)
      case _ =>
        state.persisted.focus
    val prunedTree = removedIds.foldLeft(state.persisted.layout.workspaceTree) { (tree, id) =>
      tree.flatMap(_.removeSurface(id)).orElse(tree)
    }
    val maximized = state.persisted.layout.maximizedWorkspaceNodeId.filterNot(nodeId =>
      state.persisted.layout.workspaceTree.flatMap(_.surfaceIdForNode(nodeId)).exists(removedIds.contains)
    )
    state.copy(
      persisted = state.persisted.copy(
        focus = nextFocus,
        layout = state.persisted.layout.copy(
          workspaceTree = prunedTree.orElse(state.persisted.layout.workspaceTree),
          maximizedWorkspaceNodeId = maximized
        )
      ),
      runtime =
        state.runtime.copy(uiSurfaces = state.runtime.uiSurfaces.filterNot(surface => removedIds.contains(surface.id)))
    )

  /** The retained surface's own dock/move/resize -- collapsed into one tree update alongside the flat-list upsert
    * below, so this path (issue #817) keeps the workspace tree in step with `uiSurfaces` itself rather than relying on
    * a separate reconciliation pass to notice the drift.
    */
  private def placeInTree(
    tree: Option[WorkspaceTree],
    surfaceId: SurfaceId,
    position: PanelPosition,
    size: Int,
    isNewlyDocked: Boolean,
    state: AppState
  ): Option[WorkspaceTree] =
    tree.flatMap { workspaceTree =>
      if isNewlyDocked then
        val (splitId, leafId) = workspaceTree.nextDockIds(surfaceId)
        workspaceTree.dockSized(surfaceId, position, splitId, leafId, size, state.runtime.viewportSize)
      else if workspaceTree.positionForSurface(surfaceId).contains(position) then
        workspaceTree
          .allocationRatio(surfaceId, size, state.runtime.viewportSize)
          .flatMap(workspaceTree.resizeSurface(surfaceId, _))
      else
        val (splitId, _) = workspaceTree.nextDockIds(surfaceId)
        workspaceTree.moveSurface(surfaceId, position, splitId).flatMap { moved =>
          moved.allocationRatio(surfaceId, size, state.runtime.viewportSize).flatMap(moved.resizeSurface(surfaceId, _))
        }
    }

  private def upsertPanelKind(
    kind: PanelKind,
    content: SurfaceContent,
    position: PanelPosition,
    size: Int
  )(state: AppState): AppState =
    val matchingSurfaces = state.runtime.uiSurfaces.filter(surface => panelKindOf(surface.content).contains(kind))
    val retainedSurface  = matchingSurfaces.reverse.headOption
    val stateWithoutKind = state.copy(runtime =
      state.runtime.copy(uiSurfaces =
        state.runtime.uiSurfaces.filterNot(surface => panelKindOf(surface.content).contains(kind))
      )
    )
    val droppedIds = matchingSurfaces.filterNot(surface => retainedSurface.exists(_.id == surface.id)).map(_.id).toSet
    val treeWithoutDropped = droppedIds.foldLeft(state.persisted.layout.workspaceTree) { (tree, id) =>
      tree.flatMap(_.removeSurface(id)).orElse(tree)
    }
    val (stateWithId, surface, isNewlyDocked) = retainedSurface match
      case Some(existing) =>
        (
          stateWithoutKind,
          existing.copy(content = content, presentation = SurfacePresentation.Docked, dismissOnMove = false),
          false
        )
      case None =>
        val (allocatedState, surfaceId) = stateWithoutKind.allocateSurfaceId
        (allocatedState, UiSurface(surfaceId, content, SurfacePresentation.Docked, dismissOnMove = false), true)
    val placedTree =
      treeWithoutDropped
        .orElse(stateWithId.persisted.layout.workspaceTree)
        .fold(state.persisted.layout.workspaceTree)(tree =>
          placeInTree(Some(tree), surface.id, position, size, isNewlyDocked, stateWithId)
        )
    val nextFocus = state.persisted.focus match
      case Focus.Surface(surfaceId) if matchingSurfaces.exists(_.id == surfaceId) => Focus.Surface(surface.id)
      case _                                                                      => state.persisted.focus
    stateWithId.copy(
      persisted = stateWithId.persisted.copy(
        focus = nextFocus,
        layout =
          stateWithId.persisted.layout.copy(workspaceTree = placedTree.orElse(state.persisted.layout.workspaceTree))
      ),
      runtime = stateWithId.runtime.copy(uiSurfaces = stateWithId.runtime.uiSurfaces :+ surface)
    )

  /** Reorders a docked panel among the others sharing its edge (issue #1310) by rearranging the workspace tree's own
    * nesting for that edge (issue #817: the tree is the sole record of same-edge order, via `WorkspaceTree.dock`'s
    * insertion-order nesting), rather than splicing `uiSurfaces` and leaving a later reconciliation pass to notice.
    */
  private def reorderPanelKind(kind: PanelKind, delta: Int)(state: AppState): AppState =
    if delta == 0 then state
    else
      state.persisted.layout.workspaceTree match
        case None => state
        case Some(tree) =>
          def kindOf(surfaceId: SurfaceId): Option[PanelKind] =
            state.runtime.uiSurfaces.find(_.id == surfaceId).flatMap(surface => panelKindOf(surface.content))
          tree.dockedSurfaceIds.find(id => kindOf(id).contains(kind)) match
            case None => state
            case Some(targetId) =>
              tree.positionForSurface(targetId) match
                case None => state
                case Some(targetPosition) =>
                  val sameEdge =
                    tree.dockedSurfaceIds.filter(id => tree.positionForSurface(id).contains(targetPosition))
                  val currentIndex = sameEdge.indexOf(targetId)
                  val targetIndex  = (currentIndex + delta).max(0).min(sameEdge.length - 1)
                  if currentIndex < 0 || currentIndex == targetIndex then state
                  else
                    val desiredOrder  = moveWithinList(sameEdge, currentIndex, targetIndex)
                    val reorderedTree = tree.reorderAt(targetPosition, desiredOrder)
                    state.copy(persisted =
                      state.persisted.copy(layout = state.persisted.layout.copy(workspaceTree = Some(reorderedTree)))
                    )

  private def moveWithinList[A](values: List[A], from: Int, to: Int): List[A] =
    if from == to then values
    else
      values.lift(from) match
        case None => values
        case Some(value) =>
          val withoutValue = values.patch(from, Nil, 1)
          withoutValue.patch(to, List(value), 0)

  private def newestPanelKindSurface(kind: PanelKind, state: AppState): Option[UiSurface] =
    state.runtime.uiSurfaces.reverse.find(surface => panelKindOf(surface.content).contains(kind))

  private def panelKindOf(content: SurfaceContent): Option[PanelKind] =
    content match
      case SurfaceContent.DirectoryTree(_, _)   => Some(PanelKind.Explorer)
      case SurfaceContent.Outline(_, _)         => Some(PanelKind.Outline)
      case SurfaceContent.Comments(_, _)        => Some(PanelKind.Comments)
      case SurfaceContent.Diagnostics(_, _)     => Some(PanelKind.Diagnostics)
      case SurfaceContent.MarkdownPreview(_, _) => Some(PanelKind.MarkdownPreview)
      case _                                    => None

  private def defaultPanelSize(kind: PanelKind, position: PanelPosition): Int =
    kind match
      case PanelKind.MarkdownPreview => 40
      case PanelKind.Diagnostics =>
        position match
          case PanelPosition.Top | PanelPosition.Bottom => 10
          case PanelPosition.Left | PanelPosition.Right => 30
      case PanelKind.Explorer | PanelKind.Outline | PanelKind.Comments =>
        position match
          case PanelPosition.Top | PanelPosition.Bottom => 10
          case PanelPosition.Left | PanelPosition.Right => 30

  private def refreshCommandRunnerPanelSelections: IO[Unit] =
    stateRef.update { state =>
      val selections = com.serenity.state.reducers.CommandRunnerPanelSelections.fromState(state)
      val updatedSurfaces = state.runtime.uiSurfaces.map {
        case surface @ UiSurface(_, SurfaceContent.CommandPalette(runner), _, _) =>
          surface.copy(content =
            SurfaceContent.CommandPalette(runner.copy(optionSelections = runner.optionSelections ++ selections))
          )
        case other => other
      }
      state.copy(runtime = state.runtime.copy(uiSurfaces = updatedSurfaces))
    }

  private def unpinMarkdownPreviewPanel(): IO[Unit] =
    stateRef.update(removePanelKind(PanelKind.MarkdownPreview))

  private[manager] def pinExplorerPanelEffect(position: PanelPosition, path: Path, size: Int): IO[Unit] =
    for
      fileEntries <- fileManager.listDirectory(path)
      dirEntries = toDirEntries(fileEntries)
      _ <- enqueueEvent(
        ExplorerEvent.RootDirectoryLoaded(
          position = position,
          rootPath = path,
          size = size,
          entries = dirEntries,
          selectedPath = dirEntries.headOption.map(_.path)
        )
      )
    yield ()

  private[manager] def loadPinnedDirectoryEffect(position: PanelPosition, path: Path): IO[Unit] =
    (for
      fileEntries <- fileManager.listDirectory(path)
      dirEntries = toDirEntries(fileEntries)
      _ <- enqueueEvent(ExplorerEvent.DirectoryLoaded(position, path, dirEntries))
    yield ()).handleErrorWith(ex => logger.error(ex)(s"[FILE] Failed to load directory $path"))

  private def toDirEntries(entries: List[FileEntry]): List[DirEntry] =
    entries.map(entry => DirEntry(entry.path, entry.name, entry.isDirectory))
