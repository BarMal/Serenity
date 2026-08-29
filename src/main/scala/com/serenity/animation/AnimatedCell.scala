package com.serenity.animation

import java.awt.Color

final case class ColorTimeline(
    startColor: Color,
    endColor: Color,
    steps: Int,
    delayFrames: Int = 0,
    currentFrame: Int = 0
):

  def currentColor: Option[Color] =
    if steps <= 0 then None
    else if currentFrame < delayFrames.max(0) then Some(startColor)
    else RgbInterpolator.interpolateRgbaAt(startColor, endColor, steps, currentFrame - delayFrames.max(0))

  def advance: ColorTimeline =
    copy(currentFrame = currentFrame + 1)

  def isComplete: Boolean =
    steps <= 0 || currentFrame >= delayFrames.max(0) + steps

final case class AnimatedCell(
    content: Option[Char],
    foregroundSteps: List[Color],
    backgroundSteps: List[Color],
    cycling: Boolean = false,
    foregroundAnimation: Option[ColorTimeline] = None,
    backgroundAnimation: Option[ColorTimeline] = None,
    owner: AnimationOwner = AnimationOwner.EditorText
):

  def currentForeground: Option[Color] =
    foregroundAnimation.flatMap(_.currentColor).orElse(foregroundSteps.headOption)

  def currentBackground: Option[Color] =
    backgroundAnimation.flatMap(_.currentColor).orElse(backgroundSteps.headOption)

  def isComplete: Boolean =
    !cycling &&
      foregroundSteps.isEmpty &&
      backgroundSteps.isEmpty &&
      foregroundAnimation.forall(_.isComplete) &&
      backgroundAnimation.forall(_.isComplete)

  def advance(): AnimatedCell =
    if cycling then
      copy(
        foregroundSteps = AnimatedCell.rotate(foregroundSteps),
        backgroundSteps = AnimatedCell.rotate(backgroundSteps)
      )
    else
      copy(
        foregroundSteps = foregroundSteps.drop(1),
        backgroundSteps = backgroundSteps.drop(1),
        foregroundAnimation = foregroundAnimation.map(_.advance),
        backgroundAnimation = backgroundAnimation.map(_.advance)
      )

  def complete(): AnimatedCell =
    copy(
      foregroundSteps = List.empty,
      backgroundSteps = List.empty,
      foregroundAnimation = None,
      backgroundAnimation = None
    )

object AnimatedCell:

  /** Moves the head element to the tail; an empty list is returned unchanged. */
  private def rotate(steps: List[Color]): List[Color] =
    steps match
      case Nil          => List.empty
      case head :: tail => tail :+ head

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

  def parametricForeground(
    char: Char,
    startColor: Color,
    endColor: Color,
    steps: Int,
    delayFrames: Int = 0
  ): AnimatedCell =
    AnimatedCell(
      content = Some(char),
      foregroundSteps = List.empty,
      backgroundSteps = List.empty,
      foregroundAnimation = Option.when(steps > 0)(
        ColorTimeline(startColor, endColor, steps, delayFrames.max(0))
      )
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
