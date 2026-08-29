package com.serenity.state.manager

import java.nio.file.{Files, Path}

import scala.annotation.unused

import cats.effect.*
import cats.effect.std.Semaphore
import com.serenity.animation.AnimationState
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
  def getBufferAnimations: IO[Map[BufferId, AnimationState]]

/** Applies an atomic transformation to application state. */
trait StateUpdater:
  def updateState(update: AppState => AppState): IO[Unit]
  def updateBufferAnimations(update: Map[BufferId, AnimationState] => Map[BufferId, AnimationState]): IO[Unit]

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

/** Manages editor panes, tabs, and splits. */
trait PaneManager:
  def getActivePane: IO[Option[EditorPane]]
  def handleViewportResize(newSize: ViewportSize): IO[Unit]
  def createPane(bufferId: Option[BufferId] = None): IO[PaneId]
  def switchToPane(paneId: PaneId): IO[Unit]
  def getTabOrder(): IO[List[PaneId]]

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
  def unpinPanel(target: PanelTarget): IO[Unit]
  def movePinnedPanel(surfaceId: SurfaceId, position: PanelPosition): IO[Unit]
  def expandPinnedPanel(target: PanelTarget): IO[Unit]
  def collapseExpandedPanel(): IO[Unit]
  def switchToPinnedPanel(target: PanelTarget): IO[Unit]
  def loadDirectoryTree(path: Path, files: List[String]): IO[Unit]
  def selectFileInExplorer(filePath: Path): IO[Unit]
  def resizePinnedPanel(target: PanelTarget, newSize: Int): IO[Unit]
  def dragFileToDirectory(sourceFile: Path, targetDir: Path): IO[Unit]

/** Manages modal surfaces. */
trait ModalService:
  def showModal(modal: Modal): IO[Unit]
  def dismissModal(): IO[Unit]

/** Manages buffer file paths and persistence. */
trait FileService:
  def setBufferFilePath(bufferId: BufferId, filePath: Path): IO[Unit]
  def saveBuffer(bufferId: BufferId): IO[Unit]
  def saveBufferAs(bufferId: BufferId, filePath: Path): IO[Unit]
  def markBufferSaved(bufferId: BufferId): IO[Unit]
  def checkUnsavedChanges(bufferId: Option[BufferId] = None): IO[Boolean]
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
    @unused parentLogger: Logger[IO],
    policy: SessionManager.SessionPolicy = SessionManager.SessionPolicy(),
    onFontConfigChanged: FontConfig => IO[Unit] = _ => IO.unit,
    deviceTextScaleProvider: IO[Double] = IO.pure(1.0),
    sessionRootOverride: Option[Path] = None,
    initialConfig: AppConfig = AppConfig.default,
    configPersistencePath: Option[Path] = None,
    uiPresetStore: UiPresetStore = UiPresetStore.default,
    windowSizeProvider: IO[Option[PreferredWindowSize]] = IO.pure(None),
    onPreferredWindowSizeChanged: PreferredWindowSize => IO[Unit] = _ => IO.unit,
    fileDialog: Option[FileDialog] = None,
    markdownPreviewWindow: com.serenity.ui.tui.MarkdownPreviewWindowAvailability =
      com.serenity.ui.tui.MarkdownPreviewWindowAvailability.Unavailable
  )(using Balance, LoggerFactory[IO]): IO[StateManager] =
    val themeManager = AppThemeManager.create
    for
      resolvedSessionRootOverride <- resolveSessionRootOverride(sessionRootOverride)
      stateRef                    <- Ref.of[IO, AppState](AppState.initial(initialConfig))
      undoRef                     <- Ref.of[IO, UndoState](UndoState(maxUndoDepth = policy.maxUndoDepth))
      mouseTargetCacheRef         <- Ref.of[IO, Option[MouseTargetCache]](None)
      documentAnalysisFiberRef    <- Ref.of[IO, Option[Fiber[IO, Throwable, Unit]]](None)
      bufferAnimationsRef         <- Ref.of[IO, Map[BufferId, AnimationState]](Map.empty)
      themeNamesRef <- themeManager.listAvailableThemes
        .handleErrorWith(_ => IO.pure(Nil))
        .flatMap(Ref.of[IO, List[String]])
      quitSignal           <- Deferred[IO, Unit]
      lspQueue             <- LspEffectQueue.create
      projectTaskFiberRef  <- Ref.of[IO, Option[ManagedProjectTask]](None)
      projectTaskSemaphore <- Semaphore[IO](1)
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
        projectTaskFiberRef = projectTaskFiberRef,
        projectTaskSemaphore = projectTaskSemaphore,
        mouseTargetCacheRef = mouseTargetCacheRef,
        documentAnalysisFiberRef = documentAnalysisFiberRef,
        bufferAnimationsRef = bufferAnimationsRef,
        onFontConfigChanged = onFontConfigChanged,
        deviceTextScaleProvider = deviceTextScaleProvider,
        configPersistencePath = configPersistencePath,
        uiPresetStore = uiPresetStore,
        windowSizeProvider = windowSizeProvider,
        onPreferredWindowSizeChanged = onPreferredWindowSizeChanged,
        fileDialog = fileDialog,
        markdownPreviewWindow = markdownPreviewWindow
      )
      operations <- StateManagerOperationBoundary.create(
        stateRef,
        documentAnalysisFiberRef,
        runtime.logger
      )
    yield new StateManagerImpl(runtime, operations)

  def describeCommandRunnerEvent(event: Event, runner: CommandRunner): String =
    val modePart =
      if runner.searchTerm.isEmpty then s"mode=browse category=${runner.activeCategory}"
      else s"mode=search category=${runner.activeCategory}"
    val selectedPart =
      runner.selectedItem match
        case Some(CommandSurfaceItem.CommandItem(command))    => s"selected=command:${command.name}"
        case Some(option: CommandSurfaceItem.OptionItem)      => s"selected=option:${option.id}"
        case Some(item: CommandSurfaceItem.InputItem)         => s"selected=input:${item.id}"
        case Some(item: CommandSurfaceItem.SettingSearchItem) => s"selected=setting:${item.targetItemId}"
        case Some(group: CommandSurfaceItem.GroupItem)        => s"selected=group:${group.id}"
        case None                                             => "selected=none"

    s"event=${structuralName(event)} $modePart $selectedPart"

  def describeCommandExecution(command: Command): String =
    s"command=${command.name} category=${command.category} intent=${structuralName(command.intent)}"

  /** Structural, non-sensitive name for a domain event or command intent: the chain of case names down to (but never
    * including) any leaf payload. Recursion is restricted to our own sealed hierarchies (`com.serenity` types) so it
    * can never descend into a `String`, `Path`, `List`, or other free-form user-supplied value - those always stop the
    * walk at their enclosing case name. This keeps rolling logs identifying *what* happened without ever capturing
    * typed characters, search text, comments, dictionary words/paths, preset names, or key bindings.
    */
  private def structuralName(value: Any): String =
    value match
      case p: Product if p.productArity == 1 =>
        p.productElement(0) match
          case nested: Product if nested.getClass.getName.startsWith("com.serenity") =>
            s"${p.productPrefix}(${structuralName(nested)})"
          case _ => p.productPrefix
      case p: Product => p.productPrefix
      case other      => other.getClass.getSimpleName

  private class StateManagerImpl(runtime: StateManagerRuntime, operations: StateManagerOperationBoundary)(using Balance)
      extends StateManager:

    private val composition = new StateManagerComposition(
      runtime.stateRef,
      runtime.undoRef,
      runtime.themeNamesRef,
      runtime.quitSignal,
      runtime.logger,
      runtime.policy,
      runtime.themeManager,
      runtime.lspQueue,
      runtime.projectTaskFiberRef,
      runtime.projectTaskSemaphore,
      runtime.mouseTargetCacheRef,
      runtime.documentAnalysisFiberRef,
      runtime.bufferAnimationsRef,
      runtime.onFontConfigChanged,
      runtime.deviceTextScaleProvider,
      runtime.configPersistencePath,
      runtime.uiPresetStore,
      runtime.windowSizeProvider,
      runtime.fileDialog,
      runtime.markdownPreviewWindow,
      runtime.fileManager,
      runtime.sessionManager,
      runtime.sessionPersistence,
      operations
    )

    export composition.*
