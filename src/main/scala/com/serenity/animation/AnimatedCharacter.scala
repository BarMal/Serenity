package com.serenity.animation

import com.googlecode.lanterna.TextColor

/** A character with animation state using list-based color step consumption */
case class AnimatedCharacter(
    char: Char,
    colorSteps: List[TextColor] // Pre-calculated interpolated color sequence
):

  /** Get the current color to render (head of color steps) */
  def currentColor: TextColor =
    colorSteps.headOption.getOrElse(TextColor.ANSI.WHITE)

  /** Check if animation is complete (no more color steps) */
  def isComplete: Boolean =
    colorSteps.isEmpty

  /** Advance to next animation step by consuming head color step */
  def advance(): AnimatedCharacter =
    if isComplete then this
    else copy(colorSteps = colorSteps.tail)

  /** Create completed character (empty color steps) */
  def complete(): AnimatedCharacter =
    copy(colorSteps = List.empty)

object AnimatedCharacter:

  /** Create a completed character (no animation) */
  def completed(char: Char, color: TextColor): AnimatedCharacter =
    AnimatedCharacter(
      char = char,
      colorSteps = List(color) // Single color step that will be consumed immediately
    )

  /** Create character from color interpolation */
  def fromInterpolation(
    char: Char,
    startColor: TextColor,
    endColor: TextColor,
    steps: Int
  ): AnimatedCharacter =
    val colorSequence = RgbInterpolator.interpolate(startColor, endColor, steps)
    AnimatedCharacter(
      char = char,
      colorSteps = colorSequence
    )

  /** Create a fade-in animation with specified timing */
  def createFadeAnimation(
    char: Char,
    startColor: TextColor,
    endColor: TextColor,
    durationMs: Int = 100,
    tickRateMs: Int = 16
  ): AnimatedCharacter =
    val steps = if durationMs <= 0 then 0 else math.max(1, durationMs / tickRateMs)
    if steps <= 0 then
      // Immediate completion for zero duration
      AnimatedCharacter(char, List.empty)
    else fromInterpolation(char, startColor, endColor, steps)

  /** Create character with single color (immediately completed) */
  def apply(char: Char, color: TextColor): AnimatedCharacter =
    AnimatedCharacter(char, List.empty)
