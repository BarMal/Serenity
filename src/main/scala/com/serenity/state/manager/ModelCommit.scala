package com.serenity.state.manager

import cats.effect.{IO, Ref}
import com.serenity.state.models.AppState
import com.serenity.state.reducers.{AppEffect, UndoEffect}

/** Validated writes that change more than one part of the [[Model]] in a single `Ref` write (#1697), held to the same
  * validation and follow-up work as `StateManagerOperationBoundary.validateAndUpdateState`.
  *
  * Transitions run inside `Ref.modify`, which may retry them, so they must be pure.
  */
final private[manager] class ModelCommit(modelRef: Ref[IO, Model], operations: StateManagerOperationBoundary):

  /** Commits the model `transition` returns (`None` leaves the model untouched). A rejected app state rejects the whole
    * transition: no part of the model changes.
    */
  def updateValidated(transition: Model => Option[Model]): IO[Unit] =
    commit(current => transition(current).map(next => (next, current.app)))

  /** Commits the model `transition` returns. A rejected app state restores `fallbackState` and leaves the other parts
    * of the model as they were -- the same fallback `validateAndUpdateState` restores.
    */
  def commitValidated(fallbackState: AppState)(transition: Model => Model): IO[Unit] =
    commit(current => Some((transition(current), fallbackState)))

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
