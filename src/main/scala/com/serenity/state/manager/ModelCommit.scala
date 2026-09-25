package com.serenity.state.manager

import cats.effect.{IO, Ref}
import cats.syntax.foldable.*
import com.serenity.animation.AnimationState
import com.serenity.state.models.{AppState, BufferId}
import com.serenity.state.reducers.{AppEffect, UndoEffect}
import com.serenity.state.undo.UndoState

/** The one holder of the model `Ref` (#1697): capabilities read the model through it and change it only through its
  * writes. Every app-state write is validated by `StateManagerOperationBoundary.prepareCommit`; [[advanceTick]], the
  * render tick's animation advance, is the one write that skips the boundary's `afterCommit` follow-up work (see its
  * doc), but it is validated the same as everything else.
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
      val reduced = EffectResult.reduce(current.app, result)
      if (reduced.state eq current.app) && reduced.effects.isEmpty then (current, IO.unit)
      else
        val next = ModelCommit.applyModelEffects(current.copy(app = reduced.state), reduced.effects)
        StateManagerOperationBoundary.prepareCommit(next.app, current.app) match
          case Right(committed) =>
            (
              next.copy(app = committed),
              operations.afterCommit(current.app, committed) >> onApplied(committed) >>
                reduced.effects.filterNot(ModelCommit.isModelEffect).traverse_(interpretEffect)
            )
          case Left(errors) => (current, operations.logRejectedCommit(errors))
    }

  // Undo history and buffer animations are not app state: `AppStateValidation` has nothing to check in them.
  def updateUndo(update: UndoState => UndoState): IO[Unit] =
    modelRef.update(current => current.copy(undo = update(current.undo)))

  def updateBufferAnimations(update: Map[BufferId, AnimationState] => Map[BufferId, AnimationState]): IO[Unit] =
    modelRef.update(current => current.copy(bufferAnimations = update(current.bufferAnimations)))

  /** Commits the render tick's animation advance (#1697): a message like any other, validated like any other, through
    * the same `StateManagerOperationBoundary.prepareCommit` every write uses -- so a surface `update` drops whose exit
    * animation finished (`AnimationChoreography`, `uiSurfaces.filterNot`) can never commit a dangling reference (e.g. a
    * workspace-tree node still naming it) unnoticed, the way the old unvalidated write could.
    *
    * Deliberately skips `afterCommit`'s follow-up work -- scheduling document analysis and logging a modal transition
    * -- unlike every other commit: `update` only ever advances animation progress (cursor glide, panel geometry,
    * surface fades, ...), so it can never change spell-check-relevant content or open/close a modal, and running that
    * work every frame would be pure waste for no observable effect. `update` is pure, geometry-preserving animation
    * math, so validation itself is cheap and exists as a correctness backstop, not because a well-behaved tick is
    * expected to fail it; a tick that would (a bug) is rejected and logged like any other invalid commit, leaving
    * animation progress where it was so the next frame retries.
    */
  def advanceTick(update: Model => Model): IO[Model] =
    modelRef.flatModify { current =>
      val next = update(current)
      if next.app eq current.app then (current, IO.pure(current))
      else
        StateManagerOperationBoundary.prepareCommit(next.app, current.app) match
          case Right(committed) =>
            val committedModel = next.copy(app = committed)
            (committedModel, IO.pure(committedModel))
          case Left(errors) => (current, operations.logRejectedCommit(errors).as(current))
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
