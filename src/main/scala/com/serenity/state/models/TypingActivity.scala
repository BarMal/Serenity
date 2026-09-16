package com.serenity.state.models

/** A short window after each typed character during which the interface stays out of the way -- the floating status row
  * hides, so nothing near the caret moves while text is going in. Decays one animation tick at a time, the same cadence
  * the window sitter and surface animations advance on, so the row reappears on the tick the burst ends.
  */
final case class TypingActivity(remainingTicks: Int = 0):
  def isActive: Boolean        = remainingTicks > 0
  def observed: TypingActivity = TypingActivity(TypingActivity.QuietTicks)
  def advance: TypingActivity  = if isActive then TypingActivity(remainingTicks - 1) else this

object TypingActivity:
  /** At the default 60 fps this is about half a second of quiet after the last keystroke. */
  val QuietTicks: Int = 30

  val idle: TypingActivity = TypingActivity()
