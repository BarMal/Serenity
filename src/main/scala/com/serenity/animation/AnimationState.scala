package com.serenity.animation

import java.awt.Color

/** Buffer coordinate key for character animations (column, line) */
case class CharacterKey(column: Int, line: Int)

/** Manages animations for all characters using buffer coordinates as keys */
case class AnimationState(
    animations: Map[CharacterKey, AnimatedCell] = Map.empty
):

  def addCharacterAnimation(
    char: Char,
    x: Int,
    y: Int,
    startColor: Color,
    endColor: Color,
    steps: Int
  ): AnimationState =
    val key  = CharacterKey(x, y)
    val cell = AnimatedCell.fromForegroundInterpolation(char, startColor, endColor, steps)
    copy(animations = animations + (key -> cell))

  def addCompletedCharacter(char: Char, x: Int, y: Int, color: Color): AnimationState =
    val key  = CharacterKey(x, y)
    val cell = AnimatedCell.completed(char, color)
    copy(animations = animations + (key -> cell))

  /** Merge a pre-built map of cells into this state, overwriting any existing entries */
  def mergeAnimations(incoming: Map[CharacterKey, AnimatedCell]): AnimationState =
    copy(animations = animations ++ incoming)

  /** Advance all animations by one step */
  def advanceAnimations(): AnimationState =
    copy(animations = animations.view.mapValues(_.advance()).toMap)

  /** Advance all animations and automatically clean up completed ones */
  def advanceAllAnimations(): AnimationState =
    advanceAnimations().cleanupCompleted()

  /** Mark all animations as completed (snap to end state) */
  def onThemeChange(): AnimationState =
    copy(animations = animations.view.mapValues(_.complete()).toMap)

  /** Remove all completed animations from state */
  def cleanupCompleted(): AnimationState =
    copy(animations = animations.filter((_, cell) => !cell.isComplete))

  /** Clear all animations */
  def clearAll(): AnimationState =
    copy(animations = Map.empty)

  /** Get the animated cell at the given buffer position, if any */
  def getCell(x: Int, y: Int): Option[AnimatedCell] =
    animations.get(CharacterKey(x, y))

  /** Get the current animated foreground color at a buffer position, if an active animation exists */
  def getCharacterColor(x: Int, y: Int): Option[Color] =
    getCell(x, y).flatMap(_.currentForeground)

  /** Get all animated cells for a given buffer line, keyed by column */
  def getLineAnimations(line: Int): Map[Int, AnimatedCell] =
    animations
      .filter((key, _) => key.line == line)
      .map((key, cell) => key.column -> cell)

  /** Check if there are any active (non-complete) animations */
  def hasActiveAnimations: Boolean =
    animations.values.exists(!_.isComplete)

  /** Count of active (non-completed) animations */
  def activeAnimationCount: Int =
    animations.values.count(!_.isComplete)

  /** All animation positions as buffer coordinates */
  def allPositions: Set[CharacterKey] =
    animations.keySet

object AnimationState:
  val empty: AnimationState = AnimationState()
