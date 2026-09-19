package com.serenity.animation

/** A pure remapping of a tween's normalized linear progress `t ∈ [0,1]` to an eased progress, also in `[0,1]` (issues
  * #1082/#1083). Applied before interpolation -- `Tween.easedProgress` -- so the same `start`/`end`/`Interpolator[A]`
  * produce a different-feeling motion depending only on which curve is chosen.
  *
  * Every curve must satisfy `curve(0) == 0` and `curve(1) == 1`: a tween's first and last frame must still land exactly
  * on `start` and `end`, whatever shape the curve takes in between.
  */
sealed trait EasingCurve:
  def apply(t: Double): Double

object EasingCurve:

  /** No remapping: constant velocity throughout. The default for every existing colour animation, so adding `curve` to
    * `AnimationConfig` changes nothing until a caller opts into a different one.
    */
  case object Linear extends EasingCurve:
    def apply(t: Double): Double = t

  /** Standard cubic ease-in (`t^3`): starts slow, accelerates into the end. */
  case object EaseIn extends EasingCurve:
    def apply(t: Double): Double = t * t * t

  /** Standard cubic ease-out (`1-(1-t)^3`): starts fast, decelerates into the end. */
  case object EaseOut extends EasingCurve:

    def apply(t: Double): Double =
      val inverse = 1.0 - t
      1.0 - inverse * inverse * inverse

  /** Standard piecewise cubic ease-in-out: the first half is `EaseIn` compressed into `[0, 0.5]`, the second half is
    * `EaseOut` compressed into `[0.5, 1]`, meeting at the midpoint. A cubic rather than the sine-based
    * `-(cos(pi*t)-1)/2` convention some libraries use -- both are visually similar "slow-fast-slow" curves, but the
    * cubic keeps this module's easing family (`EaseIn`/`EaseOut`/`EaseInOut`) built from the same cubic primitive
    * rather than mixing trigonometric and polynomial curves for no behavioural difference.
    */
  case object EaseInOut extends EasingCurve:

    def apply(t: Double): Double =
      if t < 0.5 then 4.0 * t * t * t
      else
        val overshoot = -2.0 * t + 2.0
        1.0 - overshoot * overshoot * overshoot / 2.0

  /** A general CSS-`cubic-bezier()`-compatible curve: the cubic Bezier from `(0,0)` through control points `(p1x, p1y)`
    * and `(p2x,p2y)` to `(1,1)`. `p1y`/`p2y` may lie outside `[0,1]` (CSS allows this, for an overshoot/ "back" feel)
    * -- only the x control points are expected to keep the curve a function of x (monotonically increasing x(t)), which
    * is what every CSS `cubic-bezier()` preset satisfies.
    *
    * `apply(x)` has to invert the curve's own parametrization: the Bezier is defined as `(x(u), y(u))` for a parameter
    * `u ∈ [0,1]`, so producing "the eased value at input x" means first solving `x(u) = x` for `u`, then evaluating
    * `y(u)`. Solved by Newton-Raphson (fast, and exact for the well-behaved, monotonic-in-x curves this type is meant
    * for), falling back to bisection on the rare input where Newton's derivative-based step misbehaves (a flat or
    * near-flat `dx/du`) -- the same two-stage approach browser engines use for `cubic-bezier()` (e.g. WebKit's
    * `UnitBezier`).
    */
  final case class CubicBezier(p1x: Double, p1y: Double, p2x: Double, p2y: Double) extends EasingCurve:

    private def bezierComponent(a: Double, b: Double, u: Double): Double =
      val oneMinusU = 1.0 - u
      3.0 * oneMinusU * oneMinusU * u * a + 3.0 * oneMinusU * u * u * b + u * u * u

    private def bezierComponentDerivative(a: Double, b: Double, u: Double): Double =
      val oneMinusU = 1.0 - u
      3.0 * oneMinusU * oneMinusU * a + 6.0 * oneMinusU * u * (b - a) + 3.0 * u * u * (1.0 - b)

    private def xAt(u: Double): Double  = bezierComponent(p1x, p2x, u)
    private def yAt(u: Double): Double  = bezierComponent(p1y, p2y, u)
    private def dxAt(u: Double): Double = bezierComponentDerivative(p1x, p2x, u)

    private def solveU(targetX: Double): Double =
      val NewtonIterations    = 8
      val NewtonMinSlope      = 1e-6
      val BisectionIterations = 30
      val BisectionTolerance  = 1e-7

      @annotation.tailrec
      def newton(u: Double, remaining: Int): Option[Double] =
        if remaining == 0 then Some(u)
        else
          val slope = dxAt(u)
          if math.abs(slope) < NewtonMinSlope then None
          else
            val nextU = u - (xAt(u) - targetX) / slope
            if math.abs(xAt(nextU) - targetX) < 1e-7 then Some(nextU) else newton(nextU, remaining - 1)

      @annotation.tailrec
      def bisect(lowU: Double, highU: Double, remaining: Int): Double =
        val midU = (lowU + highU) / 2.0
        if remaining == 0 then midU
        else
          val midX = xAt(midU)
          if math.abs(midX - targetX) < BisectionTolerance then midU
          else if midX < targetX then bisect(midU, highU, remaining - 1)
          else bisect(lowU, midU, remaining - 1)

      newton(targetX, NewtonIterations)
        .filter(u => u >= 0.0 && u <= 1.0)
        .getOrElse(bisect(0.0, 1.0, BisectionIterations))

    def apply(t: Double): Double =
      val clamped = t.max(0.0).min(1.0)
      if clamped == 0.0 || clamped == 1.0 then clamped
      else yAt(solveU(clamped))
