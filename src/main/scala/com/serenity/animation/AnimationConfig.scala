package com.serenity.animation

import scala.concurrent.duration.FiniteDuration

/** Animation configuration for list-based color interpolation system */
final case class AnimationConfig(
    steps: Int,
    totalDuration: FiniteDuration
):
  /** Calculate the tick rate in milliseconds based on total duration and steps */
  def tickRateMs: Int = (totalDuration.toMillis / steps.max(1)).toInt.max(1)

  /** Get duration in milliseconds */
  def durationMs: Int = totalDuration.toMillis.toInt

  /** Check if animation is disabled (zero duration or steps) */
  def isDisabled: Boolean = steps <= 0 || totalDuration.toMillis <= 0

  /** Derive a runtime animation duration from a global speed multiplier. */
  def scaledBy(speedScale: Double): Option[AnimationConfig] =
    val normalizedScale = speedScale.max(0.0)
    if isDisabled || normalizedScale <= 0.0 then None
    else
      Some(
        copy(
          steps = math.max(1, math.round(steps.toDouble * normalizedScale).toInt),
          totalDuration = scala.concurrent.duration.Duration.fromNanos(
            math.round(totalDuration.toNanos.toDouble * normalizedScale)
          )
        )
      )

object AnimationConfig:
  /** No animation - characters appear immediately with final color */
  val none: Option[AnimationConfig] = None

  /** Expressive surface transition on Serenity's shared 80/160/240 ms timing scale. */
  val quick: Option[AnimationConfig] = Some(Enabled.quick)

  /** Smooth surface transition. */
  val smooth: Option[AnimationConfig] = Some(Enabled.smooth)

  /** Subtle transition for low-distraction surface changes. */
  val subtle: Option[AnimationConfig] = Some(Enabled.subtle)

  /** Fast fade-in optimized for 16ms tick rate */
  val fast: Option[AnimationConfig] = Some(Enabled.fast)

  /** Create custom animation configuration */
  def custom(durationMs: Int, tickRateMs: Int = 16): Option[AnimationConfig] =
    Some(Enabled.custom(durationMs, tickRateMs))

  /** Concrete, always-present values for the built-in presets -- for callers that need a guaranteed animation (not a
    * "may be disabled" `Option[AnimationConfig]`) without an unsafe `.get`.
    */
  object Enabled:

    val quick: AnimationConfig = AnimationConfig(
      steps = 15,
      totalDuration = scala.concurrent.duration.Duration.fromNanos(240_000_000)
    )

    val smooth: AnimationConfig = AnimationConfig(
      steps = 12,
      totalDuration = scala.concurrent.duration.Duration.fromNanos(160_000_000)
    )

    val subtle: AnimationConfig = AnimationConfig(
      steps = 5,
      totalDuration = scala.concurrent.duration.Duration.fromNanos(80_000_000)
    )

    val fast: AnimationConfig = AnimationConfig(
      steps = 4,
      totalDuration = scala.concurrent.duration.Duration.fromNanos(64_000_000) // 64ms = 4 × 16ms
    )

    def custom(durationMs: Int, tickRateMs: Int = 16): AnimationConfig =
      AnimationConfig(
        steps = math.max(1, durationMs / tickRateMs),
        totalDuration = scala.concurrent.duration.Duration.fromNanos(durationMs * 1_000_000L)
      )
