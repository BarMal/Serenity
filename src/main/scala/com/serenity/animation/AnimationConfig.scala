package com.serenity.animation

import scala.concurrent.duration.FiniteDuration

/** Animation configuration for list-based color interpolation system */
case class AnimationConfig(
    steps: Int,
    totalDuration: FiniteDuration
):
  /** Calculate the tick rate in milliseconds based on total duration and steps */
  def tickRateMs: Int = (totalDuration.toMillis / steps.max(1)).toInt.max(1)

  /** Get duration in milliseconds */
  def durationMs: Int = totalDuration.toMillis.toInt

  /** Check if animation is disabled (zero duration or steps) */
  def isDisabled: Boolean = steps <= 0 || totalDuration.toMillis <= 0

object AnimationConfig:
  /** No animation - characters appear immediately with final color */
  val none: Option[AnimationConfig] = None

  /** Quick fade-in with very visible duration for debugging */
  val quick: Option[AnimationConfig] = Some(
    AnimationConfig(
      steps = 20,
      totalDuration = scala.concurrent.duration.Duration.fromNanos(1_000_000_000) // 1000ms → 50ms per step
    )
  )

  /** Smooth fade-in with 12 steps over 200ms (for slower, more visible transitions) */
  val smooth: Option[AnimationConfig] = Some(
    AnimationConfig(
      steps = 12,
      totalDuration = scala.concurrent.duration.Duration.fromNanos(200_000_000) // 200ms → ~16ms per step
    )
  )

  /** Subtle fade-in with 3 steps over 50ms (minimal animation) */
  val subtle: Option[AnimationConfig] = Some(
    AnimationConfig(
      steps = 3,
      totalDuration = scala.concurrent.duration.Duration.fromNanos(50_000_000) // 50ms → ~16ms per step
    )
  )

  /** Fast fade-in optimized for 16ms tick rate */
  val fast: Option[AnimationConfig] = Some(
    AnimationConfig(
      steps = 4,
      totalDuration = scala.concurrent.duration.Duration.fromNanos(64_000_000) // 64ms = 4 × 16ms
    )
  )

  /** Create custom animation configuration */
  def custom(durationMs: Int, tickRateMs: Int = 16): Option[AnimationConfig] =
    val steps = math.max(1, durationMs / tickRateMs)
    Some(
      AnimationConfig(
        steps = steps,
        totalDuration = scala.concurrent.duration.Duration.fromNanos(durationMs * 1_000_000L)
      )
    )
