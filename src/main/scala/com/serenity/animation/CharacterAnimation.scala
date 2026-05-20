package com.serenity.animation

import scala.concurrent.duration.FiniteDuration

/** Configuration for character animation behavior */
case class AnimationConfig(
  opacitySteps: Int,
  totalDuration: FiniteDuration,
  animationType: AnimationType
):
  /** Calculate the opacity for a given step (0 to opacitySteps-1) */
  def opacityForStep(step: Int): Double =
    if opacitySteps <= 1 then 1.0
    else math.min(1.0, (step + 1).toDouble / opacitySteps)
  
  /** Calculate step duration in milliseconds */
  def stepDurationMs: Long =
    if opacitySteps <= 0 then 0
    else totalDuration.toMillis / opacitySteps

object AnimationConfig:
  /** No animation - instant full opacity */
  val none: Option[AnimationConfig] = None
  
  /** Quick fade-in with 3 steps over 150ms */
  val quick: Option[AnimationConfig] = Some(AnimationConfig(
    opacitySteps = 3,
    totalDuration = scala.concurrent.duration.Duration.fromNanos(150_000_000), // 150ms
    animationType = AnimationType.FadeIn
  ))
  
  /** Smooth fade-in with 5 steps over 200ms */
  val smooth: Option[AnimationConfig] = Some(AnimationConfig(
    opacitySteps = 5,
    totalDuration = scala.concurrent.duration.Duration.fromNanos(200_000_000), // 200ms
    animationType = AnimationType.FadeIn
  ))
  
  /** Subtle fade-in with 2 steps over 100ms */
  val subtle: Option[AnimationConfig] = Some(AnimationConfig(
    opacitySteps = 2,
    totalDuration = scala.concurrent.duration.Duration.fromNanos(100_000_000), // 100ms
    animationType = AnimationType.FadeIn
  ))

enum AnimationType:
  case FadeIn
  case Slide
  case Scale

/** Represents a character being animated at a specific screen position */
case class AnimatingCharacter(
  char: Char,
  screenX: Int,
  screenY: Int,
  startTimeMs: Long,
  config: AnimationConfig
):
  /** Calculate current opacity based on elapsed time */
  def currentOpacity(currentTimeMs: Long): Double =
    val elapsedMs = currentTimeMs - startTimeMs
    if elapsedMs >= config.totalDuration.toMillis then
      1.0 // Animation complete, fully opaque
    else
      val currentStep = (elapsedMs / config.stepDurationMs).toInt
      config.opacityForStep(currentStep)
  
  /** Check if animation is complete */
  def isComplete(currentTimeMs: Long): Boolean =
    (currentTimeMs - startTimeMs) >= config.totalDuration.toMillis

/** Manages the state of all character animations */
case class AnimationState(
  activeAnimations: Map[CharacterPosition, AnimatingCharacter] = Map.empty
):
  /** Add a new character animation */
  def addAnimation(char: Char, x: Int, y: Int, config: AnimationConfig, currentTimeMs: Long): AnimationState =
    val position = CharacterPosition(x, y)
    val animation = AnimatingCharacter(char, x, y, currentTimeMs, config)
    copy(activeAnimations = activeAnimations + (position -> animation))
  
  /** Remove completed animations and return updated state */
  def cleanupCompleted(currentTimeMs: Long): AnimationState =
    val stillActive = activeAnimations.filter { case (_, animation) => 
      !animation.isComplete(currentTimeMs)
    }
    copy(activeAnimations = stillActive)
  
  /** Get animation for a specific screen position, if any */
  def getAnimation(x: Int, y: Int): Option[AnimatingCharacter] =
    activeAnimations.get(CharacterPosition(x, y))
  
  /** Check if any animations are active */
  def hasActiveAnimations: Boolean = activeAnimations.nonEmpty

case class CharacterPosition(x: Int, y: Int)

object AnimationState:
  /** Empty animation state with no active animations */
  val empty: AnimationState = AnimationState()