package com.serenity.animation

import java.awt.Color

/** A single buffer cell's colour animation. Both foreground and background are `Option[Tween[Color]]` (issue #1574) --
  * the same tweened-value primitive every other animated value in this codebase uses, generalised over `Color` via
  * `Interpolator[Color]` -- rather than the `ColorTimeline`/step-list pair this type carried before: `ColorTimeline` is
  * retired now that `Tween` covers everything it did (including its `delayFrames` stagger), and the step-list mechanism
  * (`foregroundSteps`/`backgroundSteps`/`cycling`/`rotate`) had no caller left to serve once `Tween` took over every
  * construction site that used it.
  */
final case class AnimatedCell(
    content: Option[Char],
    foregroundAnimation: Option[Tween[Color]] = None,
    backgroundAnimation: Option[Tween[Color]] = None,
    owner: AnimationOwner = AnimationOwner.EditorText
):

  def currentForeground: Option[Color] =
    foregroundAnimation.map(_.currentValue)

  def currentBackground: Option[Color] =
    backgroundAnimation.map(_.currentValue)

  def isComplete: Boolean =
    foregroundAnimation.forall(_.isComplete) && backgroundAnimation.forall(_.isComplete)

  def advance(): AnimatedCell =
    copy(
      foregroundAnimation = foregroundAnimation.map(_.advance),
      backgroundAnimation = backgroundAnimation.map(_.advance)
    )

  def complete(): AnimatedCell =
    copy(foregroundAnimation = None, backgroundAnimation = None)

object AnimatedCell:

  def fromThemeTransition(
    oldForeground: Color,
    newForeground: Color,
    oldBackground: Color,
    newBackground: Color,
    steps: Int
  ): AnimatedCell =
    AnimatedCell(
      content = None,
      foregroundAnimation = Option.when(steps > 0)(Tween(oldForeground, newForeground, EasingCurve.Linear, steps)),
      backgroundAnimation = Option.when(steps > 0)(Tween(oldBackground, newBackground, EasingCurve.Linear, steps))
    )

  def completed(char: Char, color: Color): AnimatedCell =
    AnimatedCell(
      content = Some(char),
      foregroundAnimation = Some(Tween(color, color, EasingCurve.Linear, steps = 1))
    )

  def parametricForeground(
    char: Char,
    startColor: Color,
    endColor: Color,
    steps: Int,
    delayFrames: Int = 0,
    curve: EasingCurve = EasingCurve.Linear
  ): AnimatedCell =
    AnimatedCell(
      content = Some(char),
      foregroundAnimation = Option.when(steps > 0)(
        Tween(start = startColor, end = endColor, curve = curve, steps = steps, delayFrames = delayFrames.max(0))
      )
    )
