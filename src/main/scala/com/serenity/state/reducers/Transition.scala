package com.serenity.state.reducers

import cats.data.{Chain, StateT, Writer}
import com.serenity.state.models.AppState

/** The effect log a transition accumulates. `Chain` rather than `List` because the log is appended to as transitions
  * compose, and `Chain` concatenates in constant time where `List` does not.
  */
type EffectLog[A] = Writer[Chain[AppEffect], A]

/** A state transition that may record effects along the way.
  *
  * `ReducerResult(state, effects)` is precisely this type run to completion: a function from the state before to the
  * state after, plus the effects to interpret. Naming it as such is what lets reducer steps compose with `flatMap` and
  * be written as for-comprehensions, instead of each branch reassembling the pair by hand -- 297 `noEffects` wrappers
  * and 67 buffer write-backs across `src/main` at the time of writing.
  *
  * Deliberately a type alias over stock Cats types rather than a custom instruction set with an interpreter. The monad
  * is inherited, `Monad[StateT]` is law-checked in `TransitionLawSpec`, and nothing here needs a free monad to earn its
  * place.
  */
type Transition[A] = StateT[EffectLog, AppState, A]

object Transition:

  /** The state as it currently stands. */
  val get: Transition[AppState] = StateT.get

  def set(state: AppState): Transition[Unit] = StateT.set(state)

  /** Replace the state by applying a pure function to it. */
  def modify(f: AppState => AppState): Transition[Unit] = StateT.modify(f)

  /** Read a projection of the state without changing it. */
  def inspect[A](f: AppState => A): Transition[A] = StateT.inspect(f)

  def pure[A](a: A): Transition[A] = StateT.pure(a)

  val unit: Transition[Unit] = pure(())

  /** Record an effect for the interpreter to run once the transition completes. */
  def emit(effect: AppEffect): Transition[Unit] =
    StateT.liftF(Writer.tell(Chain.one(effect)))

  /** Record several effects, in order. */
  def emitAll(effects: List[AppEffect]): Transition[Unit] =
    StateT.liftF(Writer.tell(Chain.fromSeq(effects)))

  /** Run against a starting state, producing the boundary type reducers still return.
    *
    * `ReducerResult` stays the boundary deliberately: this issue lands the abstraction, and the reducers migrate to it
    * in #993 and #994, so no existing call site changes yet.
    */
  def run(initial: AppState)(transition: Transition[Unit]): ReducerResult =
    val (effects, (state, _)) = transition.run(initial).run
    ReducerResult(state, effects.toList)
