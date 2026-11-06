package com.serenity.state.manager

import scala.concurrent.duration.*

import cats.effect.IO
import com.serenity.state.effects.{EffectLanes, Lane}
import org.typelevel.log4cats.Logger

/** The first half of the quit step in [[StateManagerOperationBoundary.shutdownEffects]]. */
private[manager] object EffectsShutdown:

  /** How long a quit waits for queued and running Sequential work, so a write that hangs cannot wedge quitting. */
  val GracePeriod: FiniteDuration = 5.seconds

  /** Waits for everything already on the `Lane.Exclusive` barrier, for at most [[GracePeriod]]. */
  def drainPersistence(effectLanes: EffectLanes, logger: Logger[IO]): IO[Unit] =
    IO.deferred[Unit].flatMap { drained =>
      effectLanes.submit(Lane.Exclusive, drained.complete(()).void) >>
        drained.get.timeoutTo(
          GracePeriod,
          logger.warn(s"[EFFECTS] Pending persistence still running after $GracePeriod; abandoning it on quit")
        )
    }
