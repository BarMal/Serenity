package com.serenity.animation

import java.awt.Color

import com.serenity.ui.layout.{LayoutRect, PixelPoint, PixelRect}

/** Typeclass for values a [[Tween]] can interpolate between. `t` is already curve-adjusted ("eased progress"), so an
  * instance only ever has to do the linear part -- `start + (end - start) * t` in whatever shape `A` needs.
  */
trait Interpolator[A]:
  def lerp(start: A, end: A, t: Double): A

object Interpolator:

  given Interpolator[Double] with
    def lerp(start: Double, end: Double, t: Double): Double = start + (end - start) * t

  /** Reuses `Interpolator[Double]`'s math rather than repeating it -- `Float` only exists here for callers whose domain
    * value (alpha, blur radius) is naturally a `Float`; the interpolation itself is identical.
    */
  given Interpolator[Float] with
    def lerp(start: Float, end: Float, t: Double): Float =
      summon[Interpolator[Double]].lerp(start.toDouble, end.toDouble, t).toFloat

  private def lerpInt(start: Int, end: Int, t: Double): Int =
    math.round(summon[Interpolator[Double]].lerp(start.toDouble, end.toDouble, t)).toInt

  /** `com.serenity.ui.layout.LayoutRect` is a cell-grid rectangle -- the "`Rect`-like type" #1083 asks for, reused
    * rather than inventing a new geometry type. Each field is lerped independently and rounded to the nearest whole
    * cell, since a fractional cell position doesn't mean anything on this grid.
    */
  given Interpolator[LayoutRect] with

    def lerp(start: LayoutRect, end: LayoutRect, t: Double): LayoutRect =
      LayoutRect(
        x = lerpInt(start.x, end.x, t),
        y = lerpInt(start.y, end.y, t),
        width = lerpInt(start.width, end.width, t),
        height = lerpInt(start.height, end.height, t)
      )

  /** `com.serenity.ui.layout.PixelRect`: the same shape as `LayoutRect`, one pixel-space level down -- lerped and
    * rounded the same way, to the nearest whole pixel.
    */
  given Interpolator[PixelRect] with

    def lerp(start: PixelRect, end: PixelRect, t: Double): PixelRect =
      PixelRect(
        xPx = lerpInt(start.xPx, end.xPx, t),
        yPx = lerpInt(start.yPx, end.yPx, t),
        widthPx = lerpInt(start.widthPx, end.widthPx, t),
        heightPx = lerpInt(start.heightPx, end.heightPx, t)
      )

  /** `com.serenity.ui.layout.PixelPoint`: caret glide's (issue #1085 phase 2) tweened value -- a single pixel position
    * rather than a rect, lerped and rounded the same way as `PixelRect`'s corner.
    */
  given Interpolator[PixelPoint] with

    def lerp(start: PixelPoint, end: PixelPoint, t: Double): PixelPoint =
      PixelPoint(
        xPx = lerpInt(start.xPx, end.xPx, t),
        yPx = lerpInt(start.yPx, end.yPx, t)
      )

  /** `java.awt.Color` (issue #1574): each RGBA channel lerped independently via `Interpolator[Double]`, rounded and
    * clamped to `[0, 255]` -- the same math `RgbInterpolator.interpolateComponent` used before that type was retired in
    * favour of this generic primitive (issue #1574), so no visible colour output changed.
    */
  given Interpolator[Color] with

    def lerp(start: Color, end: Color, t: Double): Color =
      def component(from: Int, to: Int): Int =
        math.round(summon[Interpolator[Double]].lerp(from.toDouble, to.toDouble, t)).toInt.max(0).min(255)

      new Color(
        component(start.getRed, end.getRed),
        component(start.getGreen, end.getGreen),
        component(start.getBlue, end.getBlue),
        component(start.getAlpha, end.getAlpha)
      )

/** A generic, curve-aware, tick-driven tween (issue #1083) -- the same shape as `ScalarTimeline`/`ColorTimeline`
  * (`progress`/`advance`/`isComplete`, one discrete step per render tick) generalised over any `A` with an
  * [[Interpolator]] instance, and curve-aware the way [[ColorTimeline]] never became (see `AnimationConfig`, item 3).
  *
  * Retiring `ScalarTimeline` (issue #1338's ad hoc scalar primitive) in favour of this is `Tween[Double]`'s whole
  * reason for existing here rather than as a narrower parametrisation of `ScalarTimeline` itself: `retarget` below is
  * what a plain generic `ScalarTimeline[A]` would still be missing.
  */
final case class Tween[A](
    start: A,
    end: A,
    curve: EasingCurve,
    steps: Int,
    currentFrame: Int = 0,
    delayFrames: Int = 0
):

  /** Frames the tween sits at `start` before interpolation begins -- the per-row stagger `ColorTimeline` used to
    * provide (issue #1574 folds that mechanism in here so every `Tween[A]`, not just colour, can stagger).
    */
  private def delay: Int = delayFrames.max(0)

  def progress: Double =
    if steps <= 0 then 1.0
    else math.min(1.0, math.max(0.0, (currentFrame - delay).toDouble / steps.toDouble))

  def easedProgress: Double = curve(progress)

  def currentValue(using interpolator: Interpolator[A]): A =
    if currentFrame < delay then start else interpolator.lerp(start, end, easedProgress)

  def advance: Tween[A] =
    if isComplete then this else copy(currentFrame = currentFrame + 1)

  def isComplete: Boolean =
    steps <= 0 || currentFrame >= delay + steps

  /** Frames still to run, delay included -- exactly the length of the step-list the retired `ColorTimeline`/
    * `RgbInterpolator` pair used to hand out before issue #1574, kept for callers (`AnimationChoreography`'s
    * reverse-fade continuation) that need to know how far into an in-flight tween a cell has got.
    */
  def remainingFrames: Int =
    if steps <= 0 then 0 else math.max(0, delay + steps - currentFrame)

  /** Continues smoothly from wherever this tween currently is, rather than resetting to `start` -- the "in-flight
    * retarget" #1083 asks for, and what fixes the column-transition jump-cut on rapid re-triggering
    * (`ColumnTransitionState`).
    *
    * The new leg runs `currentValue -> newEnd` over `steps - currentFrame` steps -- the steps this tween had left to
    * run, not its original `steps` count -- so a retarget partway through keeps roughly the same per-step cadence (and
    * so the same visual speed) instead of stretching a short remaining distance over a full-length duration, which
    * would visibly slow the motion down right when it should look uninterrupted. A tween that had already finished has
    * no "steps left" to inherit, so it starts a fresh full-length leg from its own `end` instead.
    */
  def retarget(newEnd: A)(using interpolator: Interpolator[A]): Tween[A] =
    if isComplete then Tween(start = end, end = newEnd, curve = curve, steps = steps)
    else Tween(start = currentValue, end = newEnd, curve = curve, steps = math.max(1, steps - currentFrame))
