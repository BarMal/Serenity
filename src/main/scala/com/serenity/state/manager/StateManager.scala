package com.serenity.state.manager

import java.nio.file.{Files, Path}

import cats.effect.*
import cats.effect.std.Queue
import com.serenity.command.{Command, CommandRunner, CommandSurfaceItem}
import com.serenity.config.{AppConfig, PreferredWindowSize}
import com.serenity.io.FileDialog
import com.serenity.keystroke.events.Event
import com.serenity.lsp.LspEffect
import com.serenity.rope.Balance
import com.serenity.session.SessionManager
import com.serenity.state.models.*
import com.serenity.state.undo.UndoState
import com.serenity.ui.fonts.FontLoader.FontConfig
import com.serenity.ui.layout.*
import com.serenity.ui.presets.UiPresetStore
import com.serenity.ui.theme.config.AppThemeManager
import fs2.Stream
import org.typelevel.log4cats.{Logger, LoggerFactory, LoggerName}

/** Applies editor and system events to application state. */
trait EventApplier:
  def applyEvent(event: Event): IO[Unit]

/** Reads the current immutable application state. */
trait StateReader:
  def getCurrentState: IO[AppState]

/** Applies an atomic transformation to application state. */
trait StateUpdater:
  def updateState(update: AppState => AppState): IO[Unit]

/** Advances renderer-visible animation state. */
trait AnimationTicker:
  def advanceAnimationFrames(): IO[Unit]
  def advanceAnimationsOnTick(): IO[Boolean]

/** Owns application shutdown and periodic session persistence. */
trait RuntimeLifecycle:
  def awaitQuit: IO[Unit]
  def forceQuit(): IO[Unit]
  def intervalSaveStream: Stream[IO, Unit]

/** Supplies effects for the language-server interpreter. */
trait LspEffectSource:
  def lspEffectStream: Stream[IO, LspEffect]

/** Reads persisted session metadata needed before startup restoration. */
trait SessionStartupInfo:
  def currentSessionThemeName: IO[Option[String]]
  def sessionExists: IO[Boolean]

/** Opens a file into editor state. */
trait FileOpener:
  def openFile(filePath: Path): IO[Unit]

/** Executes editor commands. */
trait CommandExecutor:
  def executeCommand(command: Command): IO[Unit]

/** Reads and changes editor focus. */
trait FocusManager:
  def getCurrentFocus: IO[Focus]
  def switchFocus(newFocus: Focus): IO[Unit]

/** Manages editor buffers. */
trait BufferManager:
  def getActiveBuffer: IO[Option[Buffer]]
  def createBuffer(content: String, filePath: Option[Path] = None): IO[BufferId]
  def createNewEmptyBuffer(): IO[BufferId]
  def updateBuffer(bufferId: BufferId, content: String): IO[Unit]
  def closeBuffer(bufferId: BufferId): IO[Unit]

/** Manages editor panes, tabs, and splits. */
trait PaneManager:
  def getActivePane: IO[Option[EditorPane]]
  def handleViewportResize(newSize: ViewportSize): IO[Unit]
  def createPane(bufferId: Option[BufferId] = None): IO[PaneId]
  def switchToPane(paneId: PaneId): IO[Unit]
  def closePane(paneId: PaneId): IO[Unit]
  def setBufferForPane(paneId: PaneId, bufferId: BufferId): IO[Unit]
  def setCursorPosition(paneId: PaneId, line: Int, column: Int): IO[Unit]
  def setViewport(paneId: PaneId, viewport: Viewport): IO[Unit]
  def setPaneProperties(paneId: PaneId, update: EditorPane => EditorPane): IO[Unit]
  def createPaneAfter(afterPaneId: PaneId, bufferId: Option[BufferId] = None): IO[PaneId]
  def getTabOrder(): IO[List[PaneId]]
  def splitPaneHorizontal(paneId: PaneId, bufferId: Option[BufferId] = None): IO[PaneId]
  def splitPaneVertical(paneId: PaneId, bufferId: Option[BufferId] = None): IO[PaneId]

/** Manages transient peek surfaces. */
trait PeekManager:
  def showPeek(content: PeekContent, at: CursorPosition): IO[Unit]
  def dismissPeek(): IO[Unit]
  def peekToPin(position: PanelPosition): IO[Unit]

/** Manages persisted editor sessions. */
trait SessionService:
  def saveSession(): IO[Unit]
  def loadSession(): IO[Option[AppState]]
  def clearSession(): IO[Unit]

/** Manages pinned panels and the file explorer. */
trait PanelManager:
  def pinPanel(content: PanelContent, position: PanelPosition, size: Int): IO[Unit]
  def unpinPanel(position: PanelPosition): IO[Unit]
  def expandPinnedPanel(position: PanelPosition): IO[Unit]
  def collapseExpandedPanel(): IO[Unit]
  def switchToPinnedPanel(position: PanelPosition): IO[Unit]
  def loadDirectoryTree(path: String, files: List[String]): IO[Unit]
  def selectFileInExplorer(filePath: String): IO[Unit]
  def resizePinnedPanel(position: PanelPosition, newSize: Int): IO[Unit]
  def dragFileToDirectory(sourceFile: String, targetDir: String): IO[Unit]

/** Manages modal surfaces. */
trait ModalService:
  def showModal(modal: Modal): IO[Unit]
  def dismissModal(): IO[Unit]

/** Manages buffer file paths and persistence. */
trait FileService:
  def setBufferFilePath(bufferId: BufferId, filePath: String): IO[Unit]
  def saveBuffer(bufferId: BufferId): IO[Unit]
  def saveBufferAs(bufferId: BufferId, filePath: String): IO[Unit]
  def markBufferSaved(bufferId: BufferId): IO[Unit]
  def checkUnsavedChanges(bufferId: Option[BufferId] = None): IO[Boolean]
  def forceCloseBuffer(bufferId: BufferId): IO[Unit]
  def getRecentFiles: IO[List[Path]]

/** Controls editor viewport scrolling. */
trait ScrollManager:
  def ensureCursorVisible(paneId: PaneId): IO[Unit]
  def smoothScrollTo(paneId: PaneId, targetLine: Int): IO[Unit]
  def progressSmoothScroll(paneId: PaneId, progress: Double): IO[Unit]
  def clickMinimap(paneId: PaneId, targetLine: Int): IO[Unit]

trait StateManager
    extends EventApplier,
      StateReader,
      StateUpdater,
      AnimationTicker,
      RuntimeLifecycle,
      LspEffectSource,
      SessionStartupInfo,
      FileOpener,
      CommandExecutor,
      FocusManager,
      BufferManager,
      PaneManager,
      PeekManager,
      SessionService,
      PanelManager,
      ModalService,
      FileService,
      ScrollManager

object StateManager:

  private val EphemeralSessionProperty = "serenity.test.ephemeralSessions"

  private def resolveSessionRootOverride(sessionRootOverride: Option[Path]): IO[Option[Path]] =
    sessionRootOverride match
      case some @ Some(_) =>
        IO.pure(some)
      case None if java.lang.Boolean.getBoolean(EphemeralSessionProperty) =>
        IO.blocking(Some(Files.createTempDirectory("serenity-state-manager-test")))
      case None =>
        IO.pure(None)

  def apply(
    parentLogger: Logger[IO],
    policy: SessionManager.SessionPolicy = SessionManager.SessionPolicy(),
    onFontConfigChanged: FontConfig => IO[Unit] = _ => IO.unit,
    deviceTextScaleProvider: IO[Double] = IO.pure(1.0),
    sessionRootOverride: Option[Path] = None,
    initialConfig: AppConfig = AppConfig.default,
    configPersistencePath: Option[Path] = None,
    uiPresetStore: UiPresetStore = UiPresetStore.default,
    windowSizeProvider: IO[Option[PreferredWindowSize]] = IO.pure(None),
    onPreferredWindowSizeChanged: PreferredWindowSize => IO[Unit] = _ => IO.unit,
    fileDialog: FileDialog = FileDialog.unavailable
  )(using Balance, LoggerFactory[IO]): IO[StateManager] =
    val themeManager = AppThemeManager.create
    for
      resolvedSessionRootOverride <- resolveSessionRootOverride(sessionRootOverride)
      stateRef                    <- Ref.of[IO, AppState](AppState.initial.copy(config = initialConfig))
      undoRef                     <- Ref.of[IO, UndoState](UndoState(maxUndoDepth = policy.maxUndoDepth))
      mouseTargetCacheRef         <- Ref.of[IO, Option[MouseTargetCache]](None)
      documentAnalysisFiberRef    <- Ref.of[IO, Option[Fiber[IO, Throwable, Unit]]](None)
      themeNamesRef <- themeManager.listAvailableThemes
        .handleErrorWith(_ => IO.pure(Nil))
        .flatMap(Ref.of[IO, List[String]])
      quitSignal <- Deferred[IO, Unit]
      lspQueue   <- Queue.bounded[IO, LspEffect](256)
      runtime = StateManagerRuntime.create(
        stateRef = stateRef,
        undoRef = undoRef,
        themeNamesRef = themeNamesRef,
        quitSignal = quitSignal,
        logger = LoggerFactory[IO].getLogger(using LoggerName("com.serenity.state.manager.StateManager")),
        policy = policy,
        sessionRootOverride = resolvedSessionRootOverride,
        themeManager = themeManager,
        lspQueue = lspQueue,
        mouseTargetCacheRef = mouseTargetCacheRef,
        documentAnalysisFiberRef = documentAnalysisFiberRef,
        onFontConfigChanged = onFontConfigChanged,
        deviceTextScaleProvider = deviceTextScaleProvider,
        configPersistencePath = configPersistencePath,
        uiPresetStore = uiPresetStore,
        windowSizeProvider = windowSizeProvider,
        onPreferredWindowSizeChanged = onPreferredWindowSizeChanged,
        fileDialog = fileDialog
      )
    yield new StateManagerImpl(runtime)

  def describeCommandRunnerEvent(event: Event, runner: CommandRunner): String =
    val modePart =
      if runner.searchTerm.isEmpty then s"mode=browse category=${runner.activeCategory}"
      else s"mode=search query=${runner.searchTerm} category=${runner.activeCategory}"
    val selectedPart =
      runner.selectedItem match
        case Some(CommandSurfaceItem.CommandItem(command)) => s"selected=command:${command.name}"
        case Some(option: CommandSurfaceItem.OptionItem)   => s"selected=option:${option.id}"
        case Some(item: CommandSurfaceItem.InputItem)      => s"selected=input:${item.id}"
        case Some(group: CommandSurfaceItem.GroupItem)     => s"selected=group:${group.id}"
        case None                                          => "selected=none"

    s"event=$event $modePart $selectedPart"

  def describeCommandExecution(command: Command): String =
    s"command=${command.name} category=${command.category} intent=${command.intent}"

  private class StateManagerImpl(runtime: StateManagerRuntime)(using Balance) extends StateManager:

    private val composition = new StateManagerComposition(
      runtime.stateRef,
      runtime.undoRef,
      runtime.themeNamesRef,
      runtime.quitSignal,
      runtime.logger,
      runtime.policy,
      runtime.themeManager,
      runtime.lspQueue,
      runtime.mouseTargetCacheRef,
      runtime.documentAnalysisFiberRef,
      runtime.onFontConfigChanged,
      runtime.deviceTextScaleProvider,
      runtime.configPersistencePath,
      runtime.uiPresetStore,
      runtime.windowSizeProvider,
      runtime.fileDialog,
      runtime.fileManager,
      runtime.sessionManager,
      runtime.sessionPersistence
    )

    export composition.*
