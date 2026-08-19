package com.serenity.state.reducers

import cats.data.{Chain, StateT, Writer}
import com.serenity.state.models.AppState

/** `Chain` rather than `List`: the log is appended to as transitions compose. */
type EffectLog[A] = Writer[Chain[AppEffect], A]

/** A state transition that may record effects: `ReducerResult` is this type run to completion.
  *
  * A type alias over stock Cats types, deliberately -- no instruction set, no interpreter. `Monad` is inherited and
  * law-checked in `TransitionLawSpec`.
  */
type Transition[A] = StateT[EffectLog, AppState, A]

object Transition:

  val get: Transition[AppState] = StateT.get

  def set(state: AppState): Transition[Unit] = StateT.set(state)

  def modify(f: AppState => AppState): Transition[Unit] = StateT.modify(f)

  def inspect[A](f: AppState => A): Transition[A] = StateT.inspect(f)

  def pure[A](a: A): Transition[A] = StateT.pure(a)

  val unit: Transition[Unit] = pure(())

  def emit(effect: AppEffect): Transition[Unit] =
    StateT.liftF(Writer.tell(Chain.one(effect)))

  def emitAll(effects: List[AppEffect]): Transition[Unit] =
    StateT.liftF(Writer.tell(Chain.fromSeq(effects)))

  /** `ReducerResult` stays the boundary until the reducers migrate in #993 and #994. */
  def run(initial: AppState)(transition: Transition[Unit]): ReducerResult =
    val (effects, (state, _)) = transition.run(initial).run
    ReducerResult(state, effects.toList)
