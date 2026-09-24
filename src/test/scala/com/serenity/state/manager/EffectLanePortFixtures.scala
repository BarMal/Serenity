package com.serenity.state.manager

import cats.effect.{IO, Ref, Resource}
import com.serenity.state.effects.{EffectLanes, Lane}
import com.serenity.state.models.AppState

/** [[EffectLanePort]] doubles for specs that build an effect interpreter on its own, without an operation boundary. */
private[manager] object EffectLanePortFixtures:

  /** Runs each job to completion on the caller's fiber and applies its result straight away: the synchronous shape a
    * spec asserting "what landed in state" right after the call needs.
    */
  def immediate(stateRef: Ref[IO, AppState]): EffectLanePort =
    new EffectLanePort:
      def submitEffect(lane: Lane.Keyed, job: IO[Unit]): IO[Unit] = job
      def dispatchEffectResult(result: EffectResult, onApplied: AppState => IO[Unit]): IO[Unit] =
        stateRef.updateAndGet(EffectResult.applyIfCurrent(_, result)).flatMap(onApplied)

  /** Real lanes, with results applied as the dispatcher would apply them -- for specs about ordering and supersession.
    */
  def laned(stateRef: Ref[IO, AppState]): Resource[IO, EffectLanePort] =
    EffectLanes.resource((_, _) => IO.unit).map { lanes =>
      new EffectLanePort:
        def submitEffect(lane: Lane.Keyed, job: IO[Unit]): IO[Unit] = lanes.submit(lane, job)
        def dispatchEffectResult(result: EffectResult, onApplied: AppState => IO[Unit]): IO[Unit] =
          stateRef.updateAndGet(EffectResult.applyIfCurrent(_, result)).flatMap(onApplied)
    }
