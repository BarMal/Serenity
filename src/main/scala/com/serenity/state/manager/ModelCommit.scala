package com.serenity.state.manager

import cats.effect.{IO, Ref}
import cats.syntax.foldable.*
import com.serenity.animation.AnimationState
import com.serenity.state.models.{AppState, BufferId}
import com.serenity.state.reducers.{AppEffect, UndoEffect}
import com.serenity.state.undo.UndoState

/** The one holder of the model `Ref` (#1697): capabilities read the model through it and change it only through its
  * writes. Every app-state write is validated by `StateManagerOperationBoundary.prepareCommit` and runs the boundary's
  * follow-up work, except through [[updateUnvalidated]].
  *
  * Transitions run inside `Ref.modify`, which may retry them, so they must be pure.
  */
final private[manager] class ModelCommit(modelRef: Ref[IO, Model], operations: StateManagerOperationBoundary):

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

  /** Applies `result` if it is still current, then runs `onApplied` with the committed state and hands the effects its
    * transition emitted to `interpretEffect`, in order. Runs on the dispatcher: a lane job reaches it through
    * `StateManagerOperationBoundary.dispatch`.
    */
  def applyResult(
    result: EffectResult,
    onApplied: AppState => IO[Unit],
    interpretEffect: AppEffect => IO[Unit] = _ => IO.unit
  ): IO[Unit] =
    modelRef.flatModify { current =>
      val next = EffectResult.reduce(current.app, result)
      if next.state eq current.app then (current, IO.unit)
      else
        StateManagerOperationBoundary.prepareCommit(next.state, current.app) match
          case Right(committed) =>
            (
              current.copy(app = committed),
              operations.afterCommit(current.app, committed) >> onApplied(committed) >>
                next.effects.traverse_(interpretEffect)
            )
          case Left(errors) => (current, operations.logRejectedCommit(errors))
    }

  // Undo history and buffer animations are not app state: `AppStateValidation` has nothing to check in them.
  def updateUndo(update: UndoState => UndoState): IO[Unit] =
    modelRef.update(current => current.copy(undo = update(current.undo)))

  def updateBufferAnimations(update: Map[BufferId, AnimationState] => Map[BufferId, AnimationState]): IO[Unit] =
    modelRef.update(current => current.copy(bufferAnimations = update(current.bufferAnimations)))

  /** Writes `update`'s model without validation and returns it. Only for the render tick's animation advance, which
    * runs every frame and only moves animation progress forward, and for the test-seeding `StateUpdater.updateState`.
    */
  def updateUnvalidated(update: Model => Model): IO[Model] =
    modelRef.modify { current =>
      val next = update(current)
      (next, next)
    }

  private def commit(transition: Model => Option[(Model, AppState)]): IO[Unit] =
    modelRef.flatModify { current =>
      transition(current) match
        case None => (current, IO.unit)
        case Some((next, fallbackState)) =>
          StateManagerOperationBoundary.prepareCommit(next.app, fallbackState) match
            case Right(committedState) =>
              (next.copy(app = committedState), operations.afterCommit(fallbackState, committedState))
            case Left(errors) =>
              (current.copy(app = fallbackState), operations.logRejectedCommit(errors))
    }

private[manager] object ModelCommit:

  /** Folds the reducer effects that only change the model itself -- buffer animations and undo bookkeeping -- into
    * `model`, so they commit in the same write as the state they came with. Every other effect is left to the effect
    * interpreter, in order.
    */
  def applyModelEffects(model: Model, effects: List[AppEffect]): Model =
    effects.foldLeft(model) {
      case (current, AppEffect.Animation(effect)) =>
        current.copy(bufferAnimations = AnimationEffectHandler.applied(current.bufferAnimations, effect))
      case (current, AppEffect.Undo(UndoEffect.RecordBoundary(entry, groupable))) =>
        current.copy(undo = UndoRecording.recorded(current.undo, entry, groupable))
      case (current, _) => current
    }

  def isModelEffect(effect: AppEffect): Boolean =
    effect match
      case AppEffect.Animation(_) | AppEffect.Undo(_) => true
      case _                                          => false
