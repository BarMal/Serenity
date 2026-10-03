package com.serenity.state.models

import scala.concurrent.duration.*

/** A short window after each typed character during which the interface stays out of the way -- the floating status row
  * hides, so nothing near the caret moves while text is going in. Timed on the monotonic clock rather than counted in
  * animation ticks, so the window lasts the same wall time whatever frame rate the render loop actually reaches.
  */
final case class TypingActivity(quietUntilNanos: Option[Long] = None):
  def isActive: Boolean = quietUntilNanos.isDefined

  def observed(nowNanos: Long): TypingActivity =
    TypingActivity(Some(nowNanos + TypingActivity.QuietWindow.toNanos))

  def advance(nowNanos: Long): TypingActivity =
    if quietUntilNanos.exists(_ <= nowNanos) then TypingActivity.idle else this

object TypingActivity:
  val QuietWindow: FiniteDuration = 500.millis

  val idle: TypingActivity = TypingActivity()
