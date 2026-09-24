package com.serenity.state.manager

import cats.effect.{IO, Ref}
import com.serenity.state.models.AppState
import com.serenity.state.reducers.{ReducerResult, Transition}

/** The IO shell every mouse hit-testing handler shares around its pure [[Transition]]: read the live state once, run
  * the transition from it, commit the outcome once through the validated path.
  */
private[manager] object MouseTransition:

  def run[A](initial: AppState)(transition: Transition[A]): (ReducerResult, A) =
    val (effects, (state, value)) = transition.run(initial).run
    (ReducerResult(state, effects.toList), value)

  /** Runs from the live state rather than the dispatch snapshot the handler hit-tested against, so a write that landed
    * in between (e.g. a background diagnostics pass) is kept. A transition that returned its input untouched and
    * emitted nothing -- a miss, or a hover over what is already hovered -- skips the commit, and with it the
    * validation and document-analysis scheduling every commit runs.
    */
  def commit[A](
    stateRef: Ref[IO, AppState],
    applyReducerResult: (ReducerResult, AppState) => IO[Unit]
  )(transition: Transition[A]): IO[A] =
    stateRef.get.flatMap { current =>
      val (result, value) = run(current)(transition)
      val unchanged       = (result.state eq current) && result.effects.isEmpty
      (if unchanged then IO.unit else applyReducerResult(result, current)).as(value)
    }
