package com.serenity.state.manager

import cats.effect.{IO, Ref}
import cats.syntax.foldable.*
import com.serenity.state.models.AppState
import com.serenity.state.reducers.{AppEffect, UndoEffect}
import com.serenity.state.undo.UndoState
import com.serenity.ui.layout.WrappedLineCache

/** The one holder of the model `Ref` (#1697): capabilities read the model through it and change it only through its
  * writes. Every app-state write is validated by `StateManagerOperationBoundary.prepareCommit`, and a write that leaves
  * the app state the very instance already committed runs neither validation nor follow-up work (#1845).
  *
  * Transitions run inside `Ref.modify`, which may retry them, so they must be pure.
  */
final private[manager] class ModelCommit(
    modelRef: Ref[IO, Model],
    operations: StateManagerOperationBoundary,
    wrapCache: WrappedLineCache
):

  def model: IO[Model] = modelRef.get

  def currentState: IO[AppState] = modelRef.get.map(_.app)

  /** Commits the model `transition` returns (`None` leaves the model untouched). A rejected app state rejects the whole
    * transition: no part of the model changes.
    */
  def updateValidated(transition: Model => Option[Model]): IO[Unit] =
    commit(current => transition(current).map(next => (next, current.app)))

  /** Commits the model `transition` returns. A rejected app state restores `fallbackState` and leaves the other parts
    * of the model as they were.
    */
  def commitValidated(fallbackState: AppState)(transition: Model => Model): IO[Unit] =
    commit(current => Some((transition(current), fallbackState)))

  /** Commits `newState` as the app state, or restores `fallbackState` if it is rejected. */
  def commitState(newState: AppState, fallbackState: AppState): IO[Unit] =
    commitValidated(fallbackState)(_.copy(app = newState))

  /** Applies `result` if it is still current, then runs `onApplied` with the committed state and hands
    * `interpretEffect` every effect its transition emitted, in order, except the model-only ones (buffer animations,
    * undo bookkeeping): `ModelCommit.applyModelEffects` folds those into the model in this same write, the same pattern
    * `EventPipelineTransitions.committed` uses for every other message, so an effect result's state and the undo
    * boundary it declares (e.g. a project-task result re-pinning the Terminal panel) commit atomically -- a rejected
    * result, or a crash between computing and committing, can never leave one landed without the other. Runs on the
    * dispatcher: a lane job reaches it through `StateManagerOperationBoundary.dispatch`.
    */
  def applyResult(
    result: EffectResult,
    onApplied: AppState => IO[Unit],
    interpretEffect: AppEffect => IO[Unit] = _ => IO.unit
  ): IO[Unit] =
    modelRef.flatModify { current =>
      val reduced = EffectResult.reduce(current.app, result, wrapCache = wrapCache)
      if (reduced.state eq current.app) && reduced.effects.isEmpty then (current, IO.unit)
      else
        val next = ClosedBufferRetention.forgetting(
          current.app,
          ModelCommit.applyModelEffects(current.copy(app = reduced.state), reduced.effects)
        )
        StateManagerOperationBoundary.prepareCommit(next.app, current.app) match
          case Right(committed) =>
            (
              ModelCommit.settled(next, committed),
              operations.afterCommit(current.app, committed) >> onApplied(committed) >>
                reduced.effects.filterNot(ModelCommit.isModelEffect).traverse_(interpretEffect)
            )
          case Left(errors) => (current, operations.logRejectedCommit(errors))
    }

  // Undo history is not app state: `AppStateValidation` has nothing to check in them.
  def updateUndo(update: UndoState => UndoState): IO[Unit] =
    modelRef.update(current => current.copy(undo = update(current.undo)))

  private def commit(transition: Model => Option[(Model, AppState)]): IO[Unit] =
    modelRef.flatModify { current =>
      transition(current) match
        case None                                       => (current, IO.unit)
        case Some((next, _)) if next.app eq current.app => (next, IO.unit)
        case Some((transitioned, fallbackState)) =>
          val next = ClosedBufferRetention.forgetting(current.app, transitioned)
          StateManagerOperationBoundary.prepareCommit(next.app, fallbackState) match
            case Right(committedState) =>
              (ModelCommit.settled(next, committedState), operations.afterCommit(fallbackState, committedState))
            case Left(errors) =>
              (current.copy(app = fallbackState), operations.logRejectedCommit(errors))
    }

private[manager] object ModelCommit:

  /** `next` with `committed` as its app state, keeping undo history only for the buffers still open in it. */
  private def settled(next: Model, committed: AppState): Model =
    Model(committed, next.undo.forOpenBuffers(committed.persisted.buffers))

  /** Folds the reducer effects that only change the model itself -- undo bookkeeping -- into `model`, so they commit in
    * the same write as the state they came with. Every other effect is left to the effect interpreter, in order.
    */
  def applyModelEffects(model: Model, effects: List[AppEffect]): Model =
    effects.foldLeft(model) {
      case (current, AppEffect.Undo(UndoEffect.RecordBoundary(entry, grouping))) =>
        UndoRecording.recorded(current, entry, grouping)
      case (current, _) => current
    }

  def isModelEffect(effect: AppEffect): Boolean =
    effect match
      case AppEffect.Undo(_) => true
      case _                 => false
