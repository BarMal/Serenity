package com.serenity.animation

import com.googlecode.lanterna.TextColor

case class AnimatedCell(
    content: Option[Char],
    foregroundSteps: List[TextColor],
    backgroundSteps: List[TextColor],
    cycling: Boolean = false
):

  def currentForeground: Option[TextColor] = foregroundSteps.headOption
  def currentBackground: Option[TextColor] = backgroundSteps.headOption

  def isComplete: Boolean =
    !cycling && foregroundSteps.isEmpty && backgroundSteps.isEmpty

  def advance(): AnimatedCell =
    if cycling then
      copy(
        foregroundSteps = if foregroundSteps.isEmpty then List.empty else foregroundSteps.tail :+ foregroundSteps.head,
        backgroundSteps = if backgroundSteps.isEmpty then List.empty else backgroundSteps.tail :+ backgroundSteps.head
      )
    else
      copy(
        foregroundSteps = if foregroundSteps.isEmpty then List.empty else foregroundSteps.tail,
        backgroundSteps = if backgroundSteps.isEmpty then List.empty else backgroundSteps.tail
      )

  def complete(): AnimatedCell =
    copy(foregroundSteps = List.empty, backgroundSteps = List.empty)

object AnimatedCell:

  def fromForegroundInterpolation(
    char: Char,
    startColor: TextColor,
    endColor: TextColor,
    steps: Int
  ): AnimatedCell =
    AnimatedCell(
      content = Some(char),
      foregroundSteps = RgbInterpolator.interpolate(startColor, endColor, steps),
      backgroundSteps = List.empty
    )

  def fromThemeTransition(
    oldForeground: TextColor,
    newForeground: TextColor,
    oldBackground: TextColor,
    newBackground: TextColor,
    steps: Int
  ): AnimatedCell =
    AnimatedCell(
      content = None,
      foregroundSteps = RgbInterpolator.interpolate(oldForeground, newForeground, steps),
      backgroundSteps = RgbInterpolator.interpolate(oldBackground, newBackground, steps)
    )

  def completed(char: Char, color: TextColor): AnimatedCell =
    AnimatedCell(
      content = Some(char),
      foregroundSteps = List(color),
      backgroundSteps = List.empty
    )

  def createFadeAnimation(
    char: Char,
    startColor: TextColor,
    endColor: TextColor,
    durationMs: Int = 100,
    tickRateMs: Int = 16
  ): AnimatedCell =
    val steps = if durationMs <= 0 then 0 else math.max(1, durationMs / tickRateMs)
    if steps <= 0 then AnimatedCell(Some(char), List.empty, List.empty)
    else fromForegroundInterpolation(char, startColor, endColor, steps)
