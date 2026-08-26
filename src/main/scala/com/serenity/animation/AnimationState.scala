package com.serenity.animation

import java.awt.Color

import com.serenity.rope.Rope

/** Buffer coordinate key for character animations (column, line) */
final case class CharacterKey(column: Int, line: Int)

/** One text edit's effect on buffer offsets: `[start, end)` replaced by `insertedText`. `start == end` is a pure
  * insertion. Deliberately narrower than the reducer's own edit-tracking type (no cursor-ownership index) -- this is
  * the shape [[AnimationState.remapThroughEdits]] needs to cross the reducer/presentation-layer boundary as an
  * `AppEffect` payload, not a reducer-internal detail.
  */
final case class TextEdit(start: Int, end: Int, insertedText: String)

/** Manages animations for all characters using buffer coordinates as keys */
final case class AnimationState(
    animations: Map[CharacterKey, AnimatedCell] = Map.empty
):

  private lazy val activeCount: Int =
    animations.values.count(!_.isComplete)

  private lazy val animationsByLine: Map[Int, Map[Int, AnimatedCell]] =
    animations
      .groupMap { case (key, _) => key.line } { case (key, cell) => key.column -> cell }
      .view
      .mapValues(_.toMap)
      .toMap

  def addCharacterAnimation(
    char: Char,
    x: Int,
    y: Int,
    startColor: Color,
    endColor: Color,
    steps: Int
  ): AnimationState =
    val key  = CharacterKey(x, y)
    val cell = AnimatedCell.parametricForeground(char, startColor, endColor, steps)
    copy(animations = animations + (key -> cell))

  def addCompletedCharacter(char: Char, x: Int, y: Int, color: Color): AnimationState =
    val key  = CharacterKey(x, y)
    val cell = AnimatedCell.completed(char, color)
    copy(animations = animations + (key -> cell))

  /** Merge a pre-built map of cells into this state, overwriting any existing entries */
  def mergeAnimations(incoming: Map[CharacterKey, AnimatedCell]): AnimationState =
    if incoming.isEmpty then this
    else copy(animations = animations ++ incoming)

  /** Merge UI transitions without replacing an active editor-text animation at the same cell. */
  def mergeUiTransitionAnimations(incoming: Map[CharacterKey, AnimatedCell]): AnimationState =
    val nonConflicting = incoming.filter {
      case (key, _) =>
        animations.get(key).forall(cell => cell.owner != AnimationOwner.EditorText || cell.isComplete)
    }
    mergeAnimations(nonConflicting)

  /** Advance all animations by one step. Cells for which `isRelevant` returns false (e.g. scrolled out of the current
    * viewport) are left untouched rather than advanced -- they simply resume from where they left off once relevant
    * again, instead of paying per-tick interpolation/allocation cost while nothing is rendering them.
    */
  def advanceAnimations(isRelevant: CharacterKey => Boolean = _ => true): AnimationState =
    if !hasActiveAnimations then this
    else
      copy(animations = animations.map { (key, cell) =>
        if cell.isComplete || !isRelevant(key) then key -> cell else key -> cell.advance()
      })

  /** Advance all animations and automatically clean up completed ones. */
  def advanceAllAnimations(isRelevant: CharacterKey => Boolean = _ => true): AnimationState =
    if !hasActiveAnimations then cleanupCompleted()
    else advanceAnimations(isRelevant).cleanupCompleted()

  /** Mark all animations as completed (snap to end state) */
  def onThemeChange(): AnimationState =
    if animations.isEmpty then this
    else copy(animations = animations.view.mapValues(_.complete()).toMap)

  /** Remove all completed animations from state */
  def cleanupCompleted(): AnimationState =
    if animations.isEmpty then this
    else if activeAnimationCount == animations.size then this
    else if activeAnimationCount == 0 then AnimationState.empty
    else copy(animations = animations.filter((_, cell) => !cell.isComplete))

  /** Clear all animations */
  def clearAll(): AnimationState =
    if animations.isEmpty then this else AnimationState.empty

  /** Remove animations owned by one independent motion family. */
  def clear(owner: AnimationOwner): AnimationState =
    val retained = animations.filter((_, cell) => cell.owner != owner)
    if retained.size == animations.size then this else AnimationState(retained)

  /** Get the animated cell at the given buffer position, if any */
  def getCell(x: Int, y: Int): Option[AnimatedCell] =
    animations.get(CharacterKey(x, y))

  /** Get the current animated foreground color at a buffer position, if an active animation exists */
  def getCharacterColor(x: Int, y: Int): Option[Color] =
    getCell(x, y).flatMap(_.currentForeground)

  /** Get all animated cells for a given buffer line, keyed by column */
  def getLineAnimations(line: Int): Map[Int, AnimatedCell] =
    animationsByLine.getOrElse(line, Map.empty)

  /** Check if there are any active (non-complete) animations */
  def hasActiveAnimations: Boolean =
    activeCount > 0

  /** Count of active (non-completed) animations */
  def activeAnimationCount: Int =
    activeCount

  /** All animation positions as buffer coordinates */
  def allPositions: Set[CharacterKey] =
    animations.keySet

  /** Remap every animation's key through a set of text edits, so an edit before an animating character moves its key
    * rather than silently leaving the animation attached to whatever character now occupies that `(line, column)` cell.
    * A character an edit deletes has its animation dropped rather than remapped: keeping it would attach the animation
    * to a different character at the same offset. `initialContent`/`updatedContent` are the buffer's rope before and
    * after the edits, used only to convert keys to and from offsets.
    */
  def remapThroughEdits(initialContent: Rope, updatedContent: Rope, edits: List[TextEdit]): AnimationState =
    if animations.isEmpty || edits.isEmpty then this
    else
      val sortedEdits = edits.sortBy(edit => (edit.start, edit.end))
      val remapped = animations.flatMap { (key, cell) =>
        val offset = initialContent.lineColumnToOffset(key.line, key.column)
        if sortedEdits.exists(edit => offset >= edit.start && offset < edit.end) then None
        else
          val nextOffset     = AnimationState.remapOffset(offset, sortedEdits)
          val (line, column) = updatedContent.offsetToLineColumn(nextOffset)
          Some(CharacterKey(column, line) -> cell)
      }
      AnimationState(remapped)

object AnimationState:
  val empty: AnimationState = AnimationState()

  /** A single character position's offset, tracked through a sequence of edits the same way a zero-width boundary that
    * always moves with an insertion at its own position would be.
    */
  private def remapOffset(offset: Int, edits: List[TextEdit]): Int =
    val (_, remapped) = edits.foldLeft((0, offset)) {
      case ((deltaSoFar, currentOffset), edit) =>
        val removedLength     = edit.end - edit.start
        val insertedLength    = edit.insertedText.length
        val editDelta         = insertedLength - removedLength
        val remappedEditStart = edit.start + deltaSoFar
        val isInsertion       = edit.start == edit.end
        val nextOffset =
          if isInsertion then if offset >= edit.start then currentOffset + insertedLength else currentOffset
          else if offset < edit.start then currentOffset
          else if offset > edit.end then currentOffset + editDelta
          else remappedEditStart

        (deltaSoFar + editDelta, nextOffset)
    }

    remapped.max(0)
