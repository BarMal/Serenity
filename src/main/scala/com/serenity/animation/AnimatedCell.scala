package com.serenity.animation

import com.googlecode.lanterna.TextColor
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
    startColor: TextColor,
    endColor: TextColor,
    steps: Int
  ): AnimatedCell =
    AnimatedCell(
      content = Some(char),
      foregroundSteps = RgbInterpolator.interpolate(startColor, endColor, steps).map(_.toColor()),
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
      foregroundSteps = RgbInterpolator.interpolate(oldForeground, newForeground, steps).map(_.toColor()),
      backgroundSteps = RgbInterpolator.interpolate(oldBackground, newBackground, steps).map(_.toColor())
    )

  def completed(char: Char, color: TextColor): AnimatedCell =
    AnimatedCell(
      content = Some(char),
      foregroundSteps = List(color.toColor()),
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
