package com.serenity.state.models

import scala.concurrent.duration.*

/** When the latest two text-entry keystrokes arrived, on the monotonic clock. Undo grouping reads it to end a run left
  * alone for over [[EditClock.Pause]] (#1930). Kept in the runtime, stamped with the event, so undo history itself
  * stays a pure function of what was recorded.
  */
final case class EditClock(nowNanos: Long = 0L, previousNanos: Long = 0L):

  def observed(now: Long): EditClock = EditClock(now, nowNanos)

  def pausedBeforeLatest: Boolean = nowNanos - previousNanos > EditClock.Pause.toNanos

object EditClock:
  val Pause: FiniteDuration = 1.second
