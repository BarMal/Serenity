package com.serenity.app

import scala.concurrent.duration.*

import com.serenity.frontend.Frontend

/** When the caret blinks, as a pure function of the last user input: solid for [[Frontend.BlinkPauseDelay]], then one
  * toggle per blink interval on absolute deadlines. Tick `n` hides the caret when odd and shows it when even.
  */
private[serenity] object CaretBlinkSchedule:

  private def blinkStart(inputAt: FiniteDuration): FiniteDuration = inputAt + Frontend.BlinkPauseDelay

  def ticksElapsed(inputAt: FiniteDuration, now: FiniteDuration, interval: FiniteDuration): Long =
    val sinceStart = now - blinkStart(inputAt)
    if sinceStart < Duration.Zero then 0L else sinceStart.toNanos / interval.toNanos

  /** Zero when a tick later than `lastTick` is already due, as after a fast frame that swallowed one. */
  def waitUntilNextTick(
    inputAt: FiniteDuration,
    now: FiniteDuration,
    interval: FiniteDuration,
    lastTick: Long
  ): FiniteDuration =
    if ticksElapsed(inputAt, now, interval) > lastTick then Duration.Zero
    else (blinkStart(inputAt) + interval * (lastTick + 1) - now).max(Duration.Zero)

  def visibleAt(tick: Long): Boolean = tick % 2 == 0
