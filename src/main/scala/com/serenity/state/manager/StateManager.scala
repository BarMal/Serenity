package com.serenity.state.manager

import java.nio.file.{Files, Path}

import cats.effect.std.Queue
import cats.effect.{Deferred, IO, Ref}
import com.serenity.command.{Command, CommandRunner, CommandSurfaceItem}
import com.serenity.config.{AppConfig, PreferredWindowSize}
import com.serenity.io.FileDialog
import com.serenity.keystroke.events.Event
import com.serenity.lsp.LspEffect
import com.serenity.rope.Balance
import com.serenity.session.{SessionManager, SessionSaveTrigger}
import com.serenity.state.models.*
import com.serenity.state.undo.UndoState
import com.serenity.ui.fonts.FontLoader.FontConfig
import com.serenity.ui.layout.*
import com.serenity.ui.presets.UiPresetStore
import com.serenity.ui.theme.config.AppThemeManager
import fs2.Stream
import org.typelevel.log4cats.{Logger, LoggerFactory, LoggerName}

trait StateManager:
  def applyEvent(event: Event): IO[Unit]
  def executeCommand(command: Command): IO[Unit]
  def getCurrentState: IO[AppState]
  def getCurrentFocus: IO[Focus]
  def switchFocus(newFocus: Focus): IO[Unit]
  def getActiveBuffer: IO[Option[Buffer]]
  def getActivePane: IO[Option[EditorPane]]
  def awaitQuit: IO[Unit]
  def forceQuit(): IO[Unit]
  def intervalSaveStream: Stream[IO, Unit]
  def updateState(update: AppState => AppState): IO[Unit]
  def handleViewportResize(newSize: ViewportSize): IO[Unit]
  def advanceAnimationFrames(): IO[Unit]
  def advanceAnimationsOnTick(): IO[Boolean]
  def lspEffectStream: Stream[IO, LspEffect]

  // Buffer operations
  def createBuffer(content: String, filePath: Option[Path] = None): IO[BufferId]
  def createNewEmptyBuffer(): IO[BufferId]
  def updateBuffer(bufferId: BufferId, content: String): IO[Unit]
  def closeBuffer(bufferId: BufferId): IO[Unit]

  // Pane operations
  def createPane(bufferId: Option[BufferId] = None): IO[PaneId]
  def switchToPane(paneId: PaneId): IO[Unit]
  def closePane(paneId: PaneId): IO[Unit]
  def setBufferForPane(paneId: PaneId, bufferId: BufferId): IO[Unit]
  def setCursorPosition(paneId: PaneId, line: Int, column: Int): IO[Unit]
  def setViewport(paneId: PaneId, viewport: Viewport): IO[Unit]
  def setPaneProperties(paneId: PaneId, update: EditorPane => EditorPane): IO[Unit]

  // Peek operations
  def showPeek(content: PeekContent, at: CursorPosition): IO[Unit]
  def dismissPeek(): IO[Unit]
  def peekToPin(position: PanelPosition): IO[Unit]

  // Session persistence operations
  def saveSession(): IO[Unit]
  def loadSession(): IO[Option[AppState]]
  def sessionExists: IO[Boolean]
  def clearSession(): IO[Unit]

  // Panel operations
  def pinPanel(content: PanelContent, position: PanelPosition, size: Int): IO[Unit]
  def unpinPanel(position: PanelPosition): IO[Unit]
  def expandPinnedPanel(position: PanelPosition): IO[Unit]
  def collapseExpandedPanel(): IO[Unit]

  // Modal operations
  def showModal(modal: Modal): IO[Unit]
  def dismissModal(): IO[Unit]

  // File operations
  def setBufferFilePath(bufferId: BufferId, filePath: String): IO[Unit]
  def saveBuffer(bufferId: BufferId): IO[Unit]
  def saveBufferAs(bufferId: BufferId, filePath: String): IO[Unit]
  def markBufferSaved(bufferId: BufferId): IO[Unit]
  def checkUnsavedChanges(bufferId: Option[BufferId] = None): IO[Boolean]
  def forceCloseBuffer(bufferId: BufferId): IO[Unit]
  def getRecentFiles: IO[List[java.nio.file.Path]]

  // Tab / pane operations
  def createPaneAfter(afterPaneId: PaneId, bufferId: Option[BufferId] = None): IO[PaneId]
  def getTabOrder(): IO[List[PaneId]]
  def splitPaneHorizontal(paneId: PaneId, bufferId: Option[BufferId] = None): IO[PaneId]
  def splitPaneVertical(paneId: PaneId, bufferId: Option[BufferId] = None): IO[PaneId]

  // Panel operations
  def switchToPinnedPanel(position: PanelPosition): IO[Unit]
  def loadDirectoryTree(path: String, files: List[String]): IO[Unit]
  def selectFileInExplorer(filePath: String): IO[Unit]
  def resizePinnedPanel(position: PanelPosition, newSize: Int): IO[Unit]
  def dragFileToDirectory(sourceFile: String, targetDir: String): IO[Unit]

  // Scrolling operations
  def ensureCursorVisible(paneId: PaneId): IO[Unit]
  def smoothScrollTo(paneId: PaneId, targetLine: Int): IO[Unit]
  def progressSmoothScroll(paneId: PaneId, progress: Double): IO[Unit]
  def clickMinimap(paneId: PaneId, targetLine: Int): IO[Unit]

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
        onFontConfigChanged = onFontConfigChanged,
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

  private class StateManagerImpl(protected val runtime: StateManagerRuntime)(using Balance)
      extends StateManager,
        StateManagerBehavior:

    protected val balance: Balance = summon[Balance]

    def lspEffectStream: Stream[IO, LspEffect] =
      Stream
        .fromQueueUnterminated(lspQueue)
        .interruptWhen(Stream.eval(quitSignal.get).as(true))

    def executeCommand(command: Command): IO[Unit] =
      stateRef.get.flatMap(state => interpretCommand(command, state))

    // Session persistence operations
    def saveSession(): IO[Unit] =
      getCurrentState.flatMap { state =>
        sessionManager.saveSession(state, persistUnsavedBuffers = true) >>
          logger.info("[SESSION] Session saved")
      }.void

    def loadSession(): IO[Option[AppState]] =
      sessionManager.loadSession()

    def sessionExists: IO[Boolean] =
      sessionManager.sessionExists

    def clearSession(): IO[Unit] =
      sessionManager.clearSession()

    def awaitQuit: IO[Unit] = quitSignal.get

    def forceQuit(): IO[Unit] =
      stateRef.get.flatMap { state =>
        sessionPersistence
          .onAppClose(clearCloseActions(state))
          .handleErrorWith(error => logger.error(error)("[SESSION] Failed to save session during forced quit")) >>
          quitSignal.complete(()).attempt.void
      }

    def intervalSaveStream: Stream[IO, Unit] =
      policy.saveInterval match
        case None => Stream.empty
        case Some(interval) =>
          Stream
            .fixedRate[IO](interval)
            .interruptWhen(Stream.eval(quitSignal.get).as(true))
            .evalMap(_ => stateRef.get.flatMap(sessionPersistence.maybeSaveSession(_, SessionSaveTrigger.Interval)))
