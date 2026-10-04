package com.serenity.input

enum WheelAxis:
  case Vertical, Horizontal

/** Wheel rotation received on each axis but not yet worth a whole notch, carried into the next wheel event. */
final case class WheelScrollState private[input] (vertical: Double, horizontal: Double)

object WheelScrollState:
  val empty: WheelScrollState = WheelScrollState(0.0, 0.0)

/** Turns AWT's precise (fractional) wheel rotation into whole notches. A high-resolution trackpad reports a gesture as
  * many events whose integer rotation is 0, so only the running sum per axis can ever reach a notch (issue #1796). A
  * classic wheel reports whole notches and passes through unchanged.
  *
  * Pure state transitions, wrapped by `SwingInputHandler` over an `AtomicReference` the same way it wraps
  * [[ModifierTapDetector]].
  */
object WheelScrollAccumulator:

  final case class Step(state: WheelScrollState, notches: Int)

  // Ten 0.1 deltas sum to a hair under 1.0 in floating point; a sum this close to a whole notch counts as reaching it.
  private val WholeNotchTolerance = 1e-9

  def accumulate(state: WheelScrollState, axis: WheelAxis, preciseRotation: Double): Step =
    val delta   = if preciseRotation.isFinite then preciseRotation else 0.0
    val total   = carriedTowards(remainder(state, axis), delta) + delta
    val nearest = math.rint(total)
    val (whole, leftover) =
      if math.abs(total - nearest) < WholeNotchTolerance then (nearest, 0.0)
      else
        val truncated = math.signum(total) * math.floor(math.abs(total))
        (truncated, total - truncated)
    Step(withRemainder(state, axis, leftover), whole.toInt)

  // Reversing direction starts a fresh count: leftover rotation the other way would swallow the start of the reversal.
  private def carriedTowards(carried: Double, delta: Double): Double =
    if carried * delta < 0 then 0.0 else carried

  private def remainder(state: WheelScrollState, axis: WheelAxis): Double =
    axis match
      case WheelAxis.Vertical   => state.vertical
      case WheelAxis.Horizontal => state.horizontal

  private def withRemainder(state: WheelScrollState, axis: WheelAxis, value: Double): WheelScrollState =
    axis match
      case WheelAxis.Vertical   => state.copy(vertical = value)
      case WheelAxis.Horizontal => state.copy(horizontal = value)
