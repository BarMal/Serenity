package com.serenity.state.manager

import java.nio.file.Path

import cats.effect.{Deferred, Fiber, IO, Ref}
import com.serenity.config.PreferredWindowSize
import com.serenity.io.FileManager
import com.serenity.keystroke.events.Event
import com.serenity.session.SessionPersistence
import com.serenity.state.models.*
import com.serenity.state.undo.HistoryEntry
import com.serenity.ui.fonts.FontLoader.FontConfig
import com.serenity.ui.layout.{PanelContent, PanelPosition, PanelTarget, PeekContent}
import com.serenity.ui.presets.UiPresetStore
import com.serenity.ui.theme.config.AppThemeManager
import org.typelevel.log4cats.Logger

private[manager] trait EffectRuntimePort:
  def stateRef: Ref[IO, AppState]
  def themeNamesRef: Ref[IO, List[String]]
  def quitSignal: Deferred[IO, Unit]
  def logger: Logger[IO]
  def themeManager: AppThemeManager
  def lspQueue: LspEffectQueue
  def projectTaskFiberRef: Ref[IO, Option[ManagedProjectTask]]
  def projectTaskSemaphore: cats.effect.std.Semaphore[IO]
  def onFontConfigChanged: FontConfig => IO[Unit]
  def deviceTextScaleProvider: IO[Double]
  def configPersistencePath: Option[Path]
  def uiPresetStore: UiPresetStore
  def windowSizeProvider: IO[Option[PreferredWindowSize]]
  def bufferAnimationsRef: Ref[IO, Map[BufferId, com.serenity.animation.AnimationState]]
  def markdownPreviewWindow: com.serenity.ui.tui.MarkdownPreviewWindowAvailability
  def trackRecentFile(current: List[Path], path: Path): List[Path] =
    (path :: current.filterNot(_ == path)).take(20)

private[manager] trait EffectEditorPort:
  def updateState(update: AppState => AppState): IO[Unit]
  def enqueueEvent(event: Event): IO[Unit]
  def validateAndUpdateState(newState: AppState, fallbackState: AppState): IO[Unit]
  def scheduleDocumentAnalysis(): IO[Unit]
  def scheduleFindSearch(request: FindSearchRequest): IO[Unit]

private[manager] trait EffectSurfacePort:
  def showPeek(content: PeekContent, at: CursorPosition): IO[Unit]
  def pinPanel(content: PanelContent, position: PanelPosition, size: Int): IO[Unit]
  def pinOrUpdateTerminalPanel(text: String, position: PanelPosition, size: Int): IO[Unit]
  def unpinPanel(target: PanelTarget): IO[Unit]
  def expandPinnedPanel(target: PanelTarget): IO[Unit]
  def collapseExpandedPanel(): IO[Unit]
  def switchToPinnedPanel(target: PanelTarget): IO[Unit]
  def resizePinnedPanel(target: PanelTarget, newSize: Int): IO[Unit]
  def recordUndoBoundary(entry: HistoryEntry, groupable: Boolean): IO[Unit]

private[manager] trait EffectFilePort:
  def fileDialog: Option[com.serenity.io.FileDialog]
  def fileManager: FileManager
  def saveExistingBuffer(bufferId: BufferId): IO[Unit]
  def saveBufferAs(bufferId: BufferId, path: Path): IO[Unit]

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
  def submitReplaceWorkflowEffect(surfaceId: SurfaceId): IO[Unit]
  def submitCloseWorkflowEffect(surfaceId: SurfaceId): IO[Unit]
  def createFileWorkflowDirectoriesEffect(surfaceId: SurfaceId): IO[Unit]
  def restoreSessionIntoCurrentViewport(restoredState: AppState, currentState: AppState): AppState
  def createStartupSession(): IO[Unit]
  def restoreStartupSession(): IO[Unit]
  def activeEditorBufferId(state: AppState): Option[BufferId]

/** State and analysis ownership required while routing editor events. */
private[manager] trait EventStatePort:
  def stateRef: Ref[IO, AppState]
  def logger: Logger[IO]
  def documentAnalysisFiberRef: Ref[IO, Option[Fiber[IO, Throwable, Unit]]]
  def mouseTargetCacheRef: Ref[IO, Option[MouseTargetCache]]
  def bufferAnimationsRef: Ref[IO, Map[BufferId, com.serenity.animation.AnimationState]]

/** Effects and commands triggered by event routing, as a capability record rather than a trait -- nothing here breaks a
  * construction-order cycle (#1389), so mockability is the only reason this needs an interface at all, and a record
  * fakes trivially without one (#1017).
  */
final private[manager] case class EventEffectPort(
    interpretEffect: com.serenity.state.reducers.AppEffect => IO[Unit],
    interpretCommand: (com.serenity.command.Command, AppState) => IO[Unit],
    executeCommand: com.serenity.command.Command => IO[Unit]
)

/** Workflow operations requested by event routing. */
private[manager] trait EventWorkflowPort:
  def beginCloseAction(scope: CloseScope, state: AppState): IO[Unit]
  def createBuffer(content: String, filePath: Option[Path] = None): IO[BufferId]
  def createPane(bufferId: Option[BufferId] = None): IO[PaneId]
