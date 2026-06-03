package com.serenity.animation

import java.awt.Color

case class AnimatedCell(
    content: Option[Char],
    foregroundSteps: List[Color],
    backgroundSteps: List[Color],
    cycling: Boolean = false
):

  def currentForeground: Option[Color] = foregroundSteps.headOption
  def currentBackground: Option[Color] = backgroundSteps.headOption

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
    startColor: Color,
    endColor: Color,
    steps: Int
  ): AnimatedCell =
    AnimatedCell(
      content = Some(char),
      foregroundSteps = RgbInterpolator.interpolateRgba(startColor, endColor, steps),
      backgroundSteps = List.empty
    )

  def fromThemeTransition(
    oldForeground: Color,
    newForeground: Color,
    oldBackground: Color,
    newBackground: Color,
    steps: Int
  ): AnimatedCell =
    AnimatedCell(
      content = None,
      foregroundSteps = RgbInterpolator.interpolateRgba(oldForeground, newForeground, steps),
      backgroundSteps = RgbInterpolator.interpolateRgba(oldBackground, newBackground, steps)
    )

  def completed(char: Char, color: Color): AnimatedCell =
    AnimatedCell(
      content = Some(char),
      foregroundSteps = List(color),
      backgroundSteps = List.empty
    )

  def createFadeAnimation(
    char: Char,
    startColor: Color,
    endColor: Color,
    durationMs: Int = 100,
    tickRateMs: Int = 16
  ): AnimatedCell =
    val steps = if durationMs <= 0 then 0 else math.max(1, durationMs / tickRateMs)
    if steps <= 0 then AnimatedCell(Some(char), List.empty, List.empty)
    else fromForegroundInterpolation(char, startColor, endColor, steps)
