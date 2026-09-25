package com.serenity.state.manager

import java.nio.file.{Files, Path}

import cats.effect.{Deferred, IO, Ref}
import com.serenity.animation.AnimationState
import com.serenity.command.{Command, CommandCategory, CommandIntent, SessionIntent}
import com.serenity.config.PreferredWindowSize
import com.serenity.rope.Balance
import com.serenity.session.SessionManager
import com.serenity.state.core.EditorState
import com.serenity.state.models.*
import com.serenity.state.undo.UndoState
import com.serenity.ui.fonts.FontLoader.FontConfig
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

    def saveSession: IO[Unit] =
      stateManager.commandExecutor.executeCommand(sessionCommand("save-session", SessionIntent.SaveSession))

    def clearSession: IO[Unit] =
      stateManager.commandExecutor.executeCommand(sessionCommand("clear-session", SessionIntent.ClearSession))

    /** A new buffer with `content` (and, optionally, `filePath`), for specs that need one at a specific starting
      * state rather than through `FileIntent.NewFile`/file-open, which only ever produce an empty or disk-backed
      * buffer (#1724).
      */
    def createBuffer(content: String, filePath: Option[Path])(using Balance): IO[BufferId] =
      stateManager.getCurrentState.flatMap { state =>
        val creation = EditorTransitions.bufferCreated(state, content, filePath)
        stateManager.updateState(_ => creation.created).as(creation.bufferId)
      }

    def createNewEmptyBuffer(using Balance): IO[BufferId] =
      stateManager.getCurrentState.flatMap { state =>
        val (newState, bufferId) = EditorState.createNewEmptyBuffer(state)
        stateManager.updateState(_ => newState).as(bufferId)
      }

    /** Replaces `bufferId`'s content wholesale, for specs that need arbitrary starting content rather than driving it
      * in through keystroke events. Unlike production edits (which flow through the event pipeline's own
      * `LspDocumentSync`), this pure state write raises no LSP `didChange` -- nothing here asserts on one (#1724).
      */
    def updateBuffer(bufferId: BufferId, content: String)(using Balance): IO[Unit] =
      stateManager.getCurrentState.flatMap { state =>
        EditorTransitions.bufferContentReplaced(state, bufferId, content) match
          case Some(replacement) => stateManager.updateState(_ => replacement.state)
          case None               => IO.unit
      }

  private def updateBuffer(bufferId: BufferId)(change: Buffer => Buffer): AppState => AppState = state =>
    state.persisted.buffers.get(bufferId) match
      case Some(buffer) =>
        state.copy(persisted =
          state.persisted.copy(buffers = state.persisted.buffers.updated(bufferId, change(buffer)))
        )
      case None => state

  private def sessionCommand(name: String, intent: SessionIntent): Command =
    Command.typed(name, name, CommandIntent.Session(intent), CommandCategory.File)
