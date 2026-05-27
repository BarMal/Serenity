package com.serenity.animation

import com.googlecode.lanterna.TextColor

/** Buffer coordinate key for character animations (column, line) */
case class CharacterKey(column: Int, line: Int)

/** Manages animations for all characters using buffer coordinates as keys */
case class AnimationState(
    animations: Map[CharacterKey, AnimatedCharacter] = Map.empty
):

  /** Add a new character animation at the given buffer position (column, line) */
  def addCharacterAnimation(
    char: Char,
    x: Int,
    y: Int,
    backgroundColor: TextColor,
    foregroundColor: TextColor,
    steps: Int
  ): AnimationState =
    val key = CharacterKey(x, y)
    val animatedChar = AnimatedCharacter.fromInterpolation(
      char = char,
      startColor = backgroundColor,
      endColor = foregroundColor,
      steps = steps
    )
    copy(animations = animations + (key -> animatedChar))

  /** Add a completed character (no animation) at the given buffer position (column, line) */
  def addCompletedCharacter(char: Char, x: Int, y: Int, color: TextColor): AnimationState =
    val key           = CharacterKey(x, y)
    val completedChar = AnimatedCharacter.completed(char, color)
    copy(animations = animations + (key -> completedChar))

  /** Advance all animations by one step using list consumption */
  def advanceAnimations(): AnimationState =
    val advancedAnimations = animations.view.mapValues(_.advance()).toMap
    copy(animations = advancedAnimations)

  /** Advance all animations and automatically clean up completed ones */
  def advanceAllAnimations(): AnimationState =
    advanceAnimations().cleanupCompleted()

  /** Mark all animations as completed (for theme changes) */
  def onThemeChange(): AnimationState =
    val completedAnimations = animations.view.mapValues(_.complete()).toMap
    copy(animations = completedAnimations)

  /** Remove all completed animations from state */
  def cleanupCompleted(): AnimationState =
    val activeAnimations = animations.filter((_, char) => !char.isComplete)
    copy(animations = activeAnimations)

  /** Clear all animations */
  def clearAll(): AnimationState =
    copy(animations = Map.empty)

  /** Get the color for a character at the given buffer position (column, line), if any */
  def getCharacterColor(x: Int, y: Int): Option[TextColor] =
    animations.get(CharacterKey(x, y)).map(_.currentColor)

  /** Get the animated character at the given buffer position (column, line), if any */
  def getCharacter(x: Int, y: Int): Option[AnimatedCharacter] =
    animations.get(CharacterKey(x, y))

  /** Check if there are any active animations */
  def hasActiveAnimations: Boolean =
    animations.values.exists(!_.isComplete)

  /** Get count of active (non-completed) animations */
  def activeAnimationCount: Int =
    animations.values.count(!_.isComplete)

  /** Get all animation positions (as buffer coordinates) */
  def allPositions: Set[CharacterKey] =
    animations.keySet

object AnimationState:
  /** Empty animation state with no active animations */
  val empty: AnimationState = AnimationState()
