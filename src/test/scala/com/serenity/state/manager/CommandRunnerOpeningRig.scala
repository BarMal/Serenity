package com.serenity.state.manager

import cats.effect.{IO, Ref}
import com.serenity.command.CommandSurfaceItem
import com.serenity.project.ProjectPresence
import com.serenity.rope.Balance
import com.serenity.state.models.{AppState, CloseScope, SurfaceContent}
import com.serenity.state.undo.UndoState
import com.serenity.testkit.SharedDictionary
import com.serenity.ui.presets.UiPresetStore
import org.typelevel.log4cats.noop.NoOpLogger

/** A real event pipeline over a real operation boundary, for specs about what opening the command palette loads. */
final private[manager] class CommandRunnerOpeningRig(
    val pipeline: StateManagerEventPipeline,
    val operations: StateManagerOperationBoundary,
    modelRef: Ref[IO, Model]
):
  def state: IO[AppState] = ModelViews.appRef(modelRef).get

  def paletteItems: IO[List[CommandSurfaceItem]] =
    state.map(
      _.commandRunnerSurface
        .map(_.content)
        .collect { case SurfaceContent.CommandPalette(runner) => runner.visibleItems }
        .getOrElse(Nil)
    )

private[manager] object CommandRunnerOpeningRig:

  def create(
    uiPresetStore: UiPresetStore,
    detectProjectPresence: AppState => IO[ProjectPresence] = _ => IO.pure(ProjectPresence.Unchecked)
  )(using Balance): IO[CommandRunnerOpeningRig] =
    for
      modelRef <- Ref.of[IO, Model](Model(AppState.initial, UndoState()))
      cacheRef <- Ref.of[IO, Option[MouseTargetCache]](None)
      quiet = NoOpLogger.impl[IO]
      operations <- StateManagerOperationBoundary.create(modelRef, quiet, dictionaryCache = SharedDictionary.default)
      statePort = new EventStatePort:
        val logger              = quiet
        val mouseTargetCacheRef = cacheRef
        val authoritativeScene  = AuthoritativeUiScene()
      effectPort = EventEffectPort(interpretEffect = _ => IO.unit, interpretCommand = (_, _) => IO.unit)
      workflowPort = new EventWorkflowPort:
        def beginCloseAction(scope: CloseScope, state: AppState): IO[Unit] = IO.unit
      modelCommit = operations.modelCommit
      undoRecording = new UndoRecording(new UndoRecordingPort:
        def updateUndo(update: UndoState => UndoState): IO[Unit] = ModelViews.undoRef(modelRef).update(update)
        export modelCommit.updateValidated as updateModelValidated
        export modelCommit.updateValidatedPlaced as updateModelPlaced)
    yield CommandRunnerOpeningRig(
      new StateManagerEventPipeline(
        statePort,
        effectPort,
        workflowPort,
        uiPresetStore,
        update =>
          ModelViews
            .appRef(modelRef)
            .modify(state =>
              val config = update(state.persisted.config)
              (state.copy(persisted = state.persisted.copy(config = config)), config)
            ),
        (_, _) => IO.unit,
        operations,
        undoRecording,
        detectProjectPresence
      ),
      operations,
      modelRef
    )
