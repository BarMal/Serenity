package com.serenity.state.manager

import java.nio.file.{Files, Path}

import cats.effect.{Deferred, IO, Ref}
import com.serenity.animation.AnimationState
import com.serenity.command.{Command, CommandCategory, CommandIntent, SessionIntent}
import com.serenity.config.PreferredWindowSize
import com.serenity.rope.Balance
import com.serenity.session.SessionManager
import com.serenity.state.models.*
import com.serenity.state.undo.UndoState
import com.serenity.ui.fonts.FontLoader.FontConfig
import com.serenity.ui.layout.{PanelContent, PanelPosition, PanelTarget, ViewportSize}
import com.serenity.ui.presets.UiPresetStore
import com.serenity.ui.theme.config.AppThemeManager
import org.typelevel.log4cats.noop.NoOpLogger

/** `StateManager` operations only specs use (#1692), built on the public `StateManager` API; `seededStateManager` also
  * uses the `fromRuntime` construction seam.
  */
object StateManagerTestFacade:

  /** A state manager whose model starts from `seed` applied to the initial state, for specs that need a state validated
    * writes would reject -- a drifted id counter, focus on a missing surface. It is built through the same
    * `fromRuntime` seam other specs use to substitute infrastructure.
    */
  def seededStateManager(seed: AppState => AppState)(using Balance): IO[StateManager] =
    stateManagerOver(Model(seed(AppState.initial), UndoState(), Map.empty))

  private def stateManagerOver(model: Model)(using Balance): IO[StateManager] =
    for
      directory           <- IO.blocking(Files.createTempDirectory("seeded-state-manager"))
      modelRef            <- Ref.of[IO, Model](model)
      themeNamesRef       <- Ref.of[IO, List[String]](Nil)
      quitSignal          <- Deferred[IO, Unit]
      lspQueue            <- LspEffectQueue.create
      mouseTargetCacheRef <- Ref.of[IO, Option[MouseTargetCache]](None)
      runtime = StateManagerRuntime.create(
        modelRef = modelRef,
        themeNamesRef = themeNamesRef,
        quitSignal = quitSignal,
        logger = NoOpLogger.impl[IO],
        policy = SessionManager.SessionPolicy(),
        sessionRootOverride = Some(directory.resolve("session")),
        themeManager = AppThemeManager.create,
        lspQueue = lspQueue,
        mouseTargetCacheRef = mouseTargetCacheRef,
        onFontConfigChanged = (_: FontConfig) => IO.unit,
        deviceTextScaleProvider = IO.pure(1.0),
        configPersistencePath = None,
        uiPresetStore = UiPresetStore(directory.resolve("presets.json")),
        windowSizeProvider = IO.pure(None),
        onPreferredWindowSizeChanged = (_: PreferredWindowSize) => IO.unit,
        fileDialog = None
      )
      stateManager <- StateManager.fromRuntime(runtime)
    yield stateManager

  extension (stateManager: StateManager)

    def getBufferAnimations: IO[Map[BufferId, AnimationState]] =
      stateManager.getModel.map(_.bufferAnimations)

    /** A new state manager over this one's model with `update` applied to its buffer animations: nothing outside the
      * dispatcher writes buffer animations, so a spec that needs specific ones seeds them at construction.
      */
    def reseededWithBufferAnimations(
      update: Map[BufferId, AnimationState] => Map[BufferId, AnimationState]
    )(using Balance): IO[StateManager] =
      stateManager.getModel.flatMap(model =>
        stateManagerOver(model.copy(bufferAnimations = update(model.bufferAnimations)))
      )

    /** Commits through validation like any other write, but fails instead of silently keeping the previous state, so a
      * fixture that seeds an invalid state is caught where it is built. A spec that needs an invalid state seeds it at
      * construction instead.
      */
    def updateState(update: AppState => AppState): IO[Unit] =
      stateManager.getCurrentState.flatMap { current =>
        AppStateValidation.validationErrors(update(current)) match
          case Nil => stateManager.updateStateValidated(update)
          case errors =>
            IO.raiseError(new IllegalArgumentException(s"updateState would commit an invalid state: $errors"))
      }

    def setBufferFilePath(bufferId: BufferId, filePath: Path): IO[Unit] =
      stateManager.updateStateValidated(
        updateBuffer(bufferId)(buffer => buffer.copy(document = buffer.document.copy(filePath = Some(filePath))))
      )

    def markBufferSaved(bufferId: BufferId): IO[Unit] =
      stateManager.updateStateValidated(
        updateBuffer(bufferId)(buffer => buffer.copy(document = buffer.document.copy(isDirty = false)))
      )

    def checkUnsavedChanges(bufferId: Option[BufferId]): IO[Boolean] =
      stateManager.getCurrentState.map { state =>
        bufferId match
          case Some(id) => state.persisted.buffers.get(id).exists(_.hasUnsavedChanges)
          case None     => state.persisted.buffers.values.exists(_.hasUnsavedChanges)
      }

    def getRecentFiles: IO[List[Path]] =
      stateManager.getCurrentState.map(_.persisted.recentFiles)

    /** Runs an arbitrary `Command` directly, bypassing the palette's UI wiring (#1724) -- reaches the
      * `private[manager]` `StateManager.executeCommand` this facade shares the package with, so callers outside
      * `state.manager` need this extension rather than the trait member itself.
      */
    def executeCommand(command: Command): IO[Unit] =
      stateManager.executeCommand(command)

    def saveSession: IO[Unit] =
      stateManager.executeCommand(sessionCommand("save-session", SessionIntent.SaveSession))

    def clearSession: IO[Unit] =
      stateManager.executeCommand(sessionCommand("clear-session", SessionIntent.ClearSession))

    // #1724 deleted `paneManager`/`panelManager` (#1017 capability records): production drives every one of their
    // behaviors it still has a real caller for through `applyEvent`/`commandExecutor` instead (see the many
    // `ViewIntent`/`GlobalAppEvent` rewrites across the test suite). These specific methods never had a real
    // production caller beyond the deleted records themselves, so rather than inventing new `StateManager` API for
    // them, they are rebuilt here directly over the same capabilities (`StateManager.composition`, `private[manager]`
    // and reachable only from this package) the deleted records delegated to.

    def createPane(bufferId: Option[BufferId] = None): IO[PaneId] =
      stateManager.composition.editor.createPane(bufferId)

    def switchToPane(paneId: PaneId): IO[Unit] =
      stateManager.composition.editor.switchToPane(paneId)

    def getTabOrder(): IO[List[PaneId]] =
      stateManager.composition.editor.getTabOrder()

    def handleViewportResize(newSize: ViewportSize): IO[Unit] =
      stateManager.composition.viewport.handleViewportResize(newSize)

    def pinPanel(content: PanelContent, position: PanelPosition, size: Int): IO[Unit] =
      val composition = stateManager.composition
      composition.runSurfaceOperation(composition.surfaces.pinPanel(content, position, size))

    def pinOrUpdateTerminalPanel(text: String, position: PanelPosition, size: Int): IO[Unit] =
      val composition = stateManager.composition
      composition.runSurfaceOperation(composition.surfaces.pinOrUpdateTerminalPanel(text, position, size))

    def unpinPanel(target: PanelTarget): IO[Unit] =
      val composition = stateManager.composition
      composition.runSurfaceOperation(composition.surfaces.unpinPanel(target))

    def movePinnedPanel(surfaceId: SurfaceId, position: PanelPosition): IO[Unit] =
      val composition = stateManager.composition
      composition.runSurfaceOperation(composition.surfaces.movePinnedPanel(surfaceId, position))

    def expandPinnedPanel(target: PanelTarget): IO[Unit] =
      val composition = stateManager.composition
      composition.runSurfaceOperation(composition.surfaces.expandPinnedPanel(target))

    def collapseExpandedPanel(): IO[Unit] =
      val composition = stateManager.composition
      composition.runSurfaceOperation(composition.surfaces.collapseExpandedPanel())

    def switchToPinnedPanel(target: PanelTarget): IO[Unit] =
      val composition = stateManager.composition
      composition.runSurfaceOperation(composition.surfaces.switchToPinnedPanel(target))

    def loadDirectoryTree(path: Path, fileNames: List[String]): IO[Unit] =
      val composition = stateManager.composition
      composition.runSurfaceOperation(composition.surfaces.loadDirectoryTree(path, fileNames))

    def selectFileInExplorer(filePath: Path): IO[Unit] =
      val composition = stateManager.composition
      composition.runSurfaceOperation(composition.surfaces.selectFileInExplorer(filePath))

    def resizePinnedPanel(target: PanelTarget, newSize: Int): IO[Unit] =
      val composition = stateManager.composition
      composition.runSurfaceOperation(composition.surfaces.resizePinnedPanel(target, newSize))

    def dragFileToDirectory(sourceFile: Path, targetDir: Path): IO[Unit] =
      val composition = stateManager.composition
      composition.runSurfaceOperation(composition.surfaces.dragFileToDirectory(sourceFile, targetDir))

  private def updateBuffer(bufferId: BufferId)(change: Buffer => Buffer): AppState => AppState = state =>
    state.persisted.buffers.get(bufferId) match
      case Some(buffer) =>
        state.copy(persisted =
          state.persisted.copy(buffers = state.persisted.buffers.updated(bufferId, change(buffer)))
        )
      case None => state

  private def sessionCommand(name: String, intent: SessionIntent): Command =
    Command.typed(name, name, CommandIntent.Session(intent), CommandCategory.File)
