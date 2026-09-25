package com.serenity.state.manager

import java.nio.file.Path

import cats.effect.{Deferred, IO, Ref}
import com.serenity.animation.AnimationState
import com.serenity.config.PreferredWindowSize
import com.serenity.io.FileManager
import com.serenity.keystroke.events.Event
import com.serenity.session.SessionPersistence
import com.serenity.state.effects.{Lane, LaneKey, LanePolicy}
import com.serenity.state.models.*
import com.serenity.ui.fonts.FontLoader.FontConfig
import com.serenity.ui.layout.{PanelContent, PanelPosition, PanelTarget, PeekContent}
import com.serenity.ui.presets.UiPresetStore
import com.serenity.ui.theme.config.AppThemeManager
import org.typelevel.log4cats.Logger

private[manager] trait EffectRuntimePort:
  def currentState: IO[AppState]
  def themeNamesRef: Ref[IO, List[String]]
  def quitSignal: Deferred[IO, Unit]
  def logger: Logger[IO]
  def themeManager: AppThemeManager
  def lspQueue: LspEffectQueue
  def runProjectTask: ProjectTaskLauncher
  def onFontConfigChanged: FontConfig => IO[Unit]
  def deviceTextScaleProvider: IO[Double]
  def configPersistencePath: Option[Path]
  def uiPresetStore: UiPresetStore
  def windowSizeProvider: IO[Option[PreferredWindowSize]]
  def markdownPreviewWindow: com.serenity.ui.tui.MarkdownPreviewWindowAvailability
  def trackRecentFile(current: List[Path], path: Path): List[Path] =
    (path :: current.filterNot(_ == path)).take(20)

/** How an effect family hands disk work to `EffectLanes` and brings its result back (#1697). */
private[manager] trait EffectLanePort:
  /** Returns once `job` is queued on `lane`; the job itself runs off the dispatcher. */
  def submitEffect(lane: Lane.Keyed, job: IO[Unit]): IO[Unit]

  /** Applies `result` on the dispatcher if it is still current, then runs `onApplied` there with the committed state;
    * returns once that has happened. For lane jobs only: code already on the dispatcher would deadlock waiting here.
    */
  def dispatchEffectResult(result: EffectResult, onApplied: AppState => IO[Unit]): IO[Unit]

/** The persistence lanes. Keybindings live in the config file, so they are written on [[PersistenceLanes.Config]] too:
  * two lanes writing one file would give no order between the writes.
  */
private[manager] object PersistenceLanes:
  val Config: Lane.Keyed  = Lane.Keyed(LaneKey.Config, LanePolicy.Sequential)
  val Presets: Lane.Keyed = Lane.Keyed(LaneKey.Presets, LanePolicy.Sequential)

private[manager] trait EffectEditorPort extends EffectLanePort:
  def enqueueEvent(event: Event): IO[Unit]
  def commitState(newState: AppState, fallbackState: AppState): IO[Unit]
  def updateModelValidated(transition: Model => Option[Model]): IO[Unit]
  def updateBufferAnimations(update: Map[BufferId, AnimationState] => Map[BufferId, AnimationState]): IO[Unit]
  def scheduleDocumentAnalysis(): IO[Unit]
  def scheduleFindSearch(request: FindSearchRequest): IO[Unit]

private[manager] trait EffectSurfacePort:
  def showPeek(content: PeekContent, at: CursorPosition): IO[Unit]
  def showModal(modal: Modal): IO[Unit]
  def pinPanel(content: PanelContent, position: PanelPosition, size: Int): IO[Unit]
  def pinOrUpdateTerminalPanel(text: String, position: PanelPosition, size: Int): IO[Unit]
  def unpinPanel(target: PanelTarget): IO[Unit]
  def expandPinnedPanel(target: PanelTarget): IO[Unit]
  def collapseExpandedPanel(): IO[Unit]
  def switchToPinnedPanel(target: PanelTarget): IO[Unit]
  def resizePinnedPanel(target: PanelTarget, newSize: Int): IO[Unit]

private[manager] trait EffectFilePort:
  def fileDialog: Option[com.serenity.io.FileDialog]
  def fileManager: FileManager
  // Returns once the save is queued; `onFailure` runs on the dispatcher if it fails (#1671).
  def submitSave(bufferId: BufferId, onFailure: Throwable => IO[Unit]): IO[Unit]
  def saveBufferAs(bufferId: BufferId, path: Path): IO[Unit]
  def loadFile(path: Path): IO[Unit]
  def openFromDialog(dialog: com.serenity.io.FileDialog): IO[Unit]
  def isSaving(path: Path): IO[Boolean]
  // #1623: re-reads the buffer's file from disk in place (same BufferId, cursor/viewport/undo state untouched),
  // replacing only its document/rich-text content and capturing a fresh revision.
  def reloadBuffer(bufferId: BufferId): IO[Unit]

private[manager] trait EffectSessionPort:
  def sessionPersistence: SessionPersistence
  def saveSession(): IO[Unit]
  def loadSession(): IO[Option[AppState]]
  def clearSession(): IO[Unit]

private[manager] trait EffectModalWorkflowPort:
  def clearCloseActions(state: AppState): AppState
  def beginCloseAction(scope: CloseScope, state: AppState): IO[Unit]
  def showSaveAsWorkflow(state: AppState, bufferId: BufferId, statusMessage: String): IO[Unit]
  def openFileWorkflowModal(mode: FileWorkflowMode, state: AppState): IO[Unit]
  def requestSaveAsFileDialog(state: AppState, bufferIdOverride: Option[BufferId]): IO[Unit]
  def refreshFileWorkflowEffect(surfaceId: SurfaceId): IO[Unit]
  def submitFileWorkflowEffect(surfaceId: SurfaceId): IO[Unit]
  def openFileWorkflowAsProjectRootEffect(surfaceId: SurfaceId, openProjectRoot: Path => IO[Unit]): IO[Unit]
  def submitReplaceWorkflowEffect(surfaceId: SurfaceId): IO[Unit]
  def submitCloseWorkflowEffect(surfaceId: SurfaceId): IO[Unit]
  // #1623: opens the reload/overwrite/cancel prompt for a buffer whose save was rejected as stale, or whose focus-in
  // re-check found the on-disk file changed underneath it; submitReloadConflictEffect resolves the user's choice.
  def openReloadConflictModal(state: AppState, bufferId: BufferId, bufferLabel: String): IO[Unit]
  def submitReloadConflictEffect(surfaceId: SurfaceId): IO[Unit]
  def createFileWorkflowDirectoriesEffect(surfaceId: SurfaceId): IO[Unit]
  def restoreSessionIntoCurrentViewport(restoredState: AppState, currentState: AppState): AppState
  def createStartupSession(): IO[Unit]
  def restoreStartupSession(): IO[Unit]
  def activeEditorBufferId(state: AppState): Option[BufferId]
  // Named sessions (issue #1390): `openSaveSessionAsPrompt`/`openSessionPicker` show the modal, called directly from
  // command interpretation; the `submit*` pair complete it once the modal's Enter routes back through
  // `WorkflowEffect`, exactly like `submitFileWorkflowEffect` does for `FileWorkflow`.
  def openSaveSessionAsPrompt(state: AppState): IO[Unit]
  def openSessionPicker(state: AppState, purpose: SessionListPurpose): IO[Unit]
  def submitSessionNamePromptEffect(surfaceId: SurfaceId): IO[Unit]
  def submitSessionListEffect(surfaceId: SurfaceId): IO[Unit]

/** What event routing needs besides the model, which it reaches through `StateManagerOperationBoundary.modelCommit`. */
private[manager] trait EventStatePort:
  def logger: Logger[IO]
  def mouseTargetCacheRef: Ref[IO, Option[MouseTargetCache]]

/** Effects and commands triggered by event routing, as a capability record rather than a trait -- nothing here breaks a
  * construction-order cycle (#1389), so mockability is the only reason this needs an interface at all, and a record
  * fakes trivially without one (#1017).
  */
final private[manager] case class EventEffectPort(
    interpretEffect: com.serenity.state.reducers.AppEffect => IO[Unit],
    interpretCommand: (com.serenity.command.Command, AppState) => IO[Unit]
)

/** Workflow operations requested by event routing. */
private[manager] trait EventWorkflowPort:
  def beginCloseAction(scope: CloseScope, state: AppState): IO[Unit]
