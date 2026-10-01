package com.serenity.state.manager

import java.nio.file.Path

import cats.effect.IO
import cats.syntax.all.*
import com.serenity.command.ViewIntent
import com.serenity.config.{AppConfig, MarkdownViewMode, VisualFlairLevel}
import com.serenity.frontend.MarkdownPreviewWindowAvailability
import com.serenity.io.FileUtils
import com.serenity.keystroke.events.Event
import com.serenity.state.models.*
import com.serenity.state.reducers.{PanelStateReducer, PinnedPanelContentReducer}
import com.serenity.ui.layout.{PanelPosition, PanelTarget, SplitAxis}

/** Pinned-panel management: pinning/unpinning/moving/resizing the explorer, outline, comments, diagnostics, and
  * markdown-preview panels, plus the [[ViewIntent]] entry points that drive them. The changes themselves are
  * [[PanelTransitions]]; each commits as a single validated model write.
  */
final private[manager] class StateManagerPanelEffects(
    currentState: IO[AppState],
    logger: org.typelevel.log4cats.Logger[IO],
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
    setCompanionSpriteEnabled: Boolean => IO[Unit]
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
        pinAtDefaultEdge(PanelId.Explorer)
      case ViewIntent.PinOutlinePanel =>
        pinAtDefaultEdge(PanelId.Outline)
      case ViewIntent.PinCommentsPanel =>
        pinAtDefaultEdge(PanelId.Comments)
      case ViewIntent.PinDiagnosticsPanel =>
        pinAtDefaultEdge(PanelId.Diagnostics)
      case ViewIntent.OpenMarkdownPreview =>
        // In-pane preview is structurally unavailable on a fixed-cell surface (cell surfaces cannot `drawImage`) --
        // toggle the spawned Swing window there instead of pinning the GUI-only panel (issue #1113).
        if state.runtime.capabilities.isCellGrid then toggleMarkdownPreviewWindow(state)
        else pinAtDefaultEdge(PanelId.MarkdownPreview)
      case ViewIntent.SetPanelPin(id, position) =>
        setPanelPin(id, position)
      case ViewIntent.MovePanelEarlier(id) =>
        commitApp(PanelTransitions.reorderPanel(id, delta = -1))
      case ViewIntent.MovePanelLater(id) =>
        commitApp(PanelTransitions.reorderPanel(id, delta = 1))
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
        updateConfigEffect >> commitApp(PanelTransitions.removePanel(PanelId.MarkdownPreview))

  // The companion's visibility is the `ui.companion_sprite.enabled` setting, so closing it turns that off too --
  // otherwise it would come back at the next start.
  private def unpinViewPanel(state: AppState, position: PanelPosition): IO[Unit] =
    val closingCompanion =
      state.persisted.layout.workspaceTree.flatMap(_.positionForSurface(PanelId.Companion.surfaceId)).contains(position)
    unpinPanel(PanelTarget.ByPosition(position)) >> setCompanionSpriteEnabled(false).whenA(closingCompanion)

  private[manager] def openMarkdownPreview: IO[Unit] =
    pinPanel(
      PanelId.MarkdownPreview,
      PanelRegistry.registrationFor(PanelId.MarkdownPreview).defaultPosition,
      refreshSelections = false
    )

  private def pinAtDefaultEdge(id: PanelId): IO[Unit] =
    setPanelPin(id, Some(PanelRegistry.registrationFor(id).defaultPosition))

  private def setPanelPin(id: PanelId, position: Option[PanelPosition]): IO[Unit] =
    position match
      case None =>
        commitModel(PanelTransitions.panelChange(_, PanelTransitions.removePanel(id), refreshSelections = true)) >>
          setCompanionSpriteEnabled(false).whenA(id == PanelId.Companion)
      case Some(targetPosition) =>
        currentState.flatMap { state =>
          val showsCompanion =
            id == PanelId.Companion && state.persisted.config.visualFlairLevel != VisualFlairLevel.Off
          setCompanionSpriteEnabled(true)
            .whenA(showsCompanion) >> pinPanel(id, targetPosition, refreshSelections = true)
        }

  /** The command/keyboard resize entry point (issue #1310) onto the same `resizePinnedPanel` -- and, through it,
    * `PanelStateReducer.resize` -- the existing mouse-drag path already uses: one shared resize state fed by all three
    * input methods rather than three separate implementations. A surface that isn't currently pinned is a no-op, the
    * same policy `resizePinnedPanel`'s own target resolution already applies.
    */
  private def setPanelSize(surfaceId: SurfaceId, delta: Int): IO[Unit] =
    currentState.flatMap { state =>
      PanelStateReducer.currentSize(surfaceId, state) match
        case Some(currentSize) =>
          resizePinnedPanel(PanelTarget.ById(surfaceId), math.max(MinimumPanelSize, currentSize + delta))
        case None =>
          IO.unit
    }

  /** Only the per-panel pin/unpin mutations declare an undo boundary (#1016 PR4) -- not `MovePanelEarlier`/`Later`'s
    * same-edge reordering, which adjusts an already-pinned panel rather than pinning or unpinning one.
    */
  private def pinPanel(id: PanelId, position: PanelPosition, refreshSelections: Boolean): IO[Unit] =
    currentState.flatMap { state =>
      val refreshed =
        if refreshSelections then commitApp(PanelTransitions.withCommandRunnerPanelSelections) else IO.unit
      PanelTransitions.pinPlan(id, position, state) match
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

  /** Pins an explorer on `path`; the commit boundary lists it, as it does any directory a docked explorer shows. */
  private[manager] def pinExplorerPanelEffect(position: PanelPosition, path: Path, size: Int): IO[Unit] =
    commitModel { model =>
      val pinned = PinnedPanelContentReducer.pinExplorerRoot(position, path, size, model.app)
      ModelCommit.applyModelEffects(model.copy(app = pinned.state), pinned.effects)
    }
