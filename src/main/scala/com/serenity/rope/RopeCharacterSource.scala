package com.serenity.rope

import com.serenity.text.TextEditing

/** Adapts a [[Rope]] to `TextEditing.CharacterSource` so the word/grapheme-boundary scanners in `TextEditing` can
  * operate directly on rope content without materialising a `String`.
  */
final case class RopeCharacterSource(rope: Rope) extends TextEditing.CharacterSource:
  override def length: Int = rope.weight

  override def charAt(index: Int): Char = rope.index(index).getOrElse('\u0000')

/** Rope-native word/grapheme-boundary helpers, sharing one `RopeCharacterSource` adapter and one call into
  * `TextEditing` per operation instead of each call site building the adapter itself.
  */
extension (rope: Rope)
  def previousWordBoundary(offset: Int): Int =
    TextEditing.previousWordBoundary(RopeCharacterSource(rope), offset)

  def nextWordBoundary(offset: Int): Int =
    TextEditing.nextWordBoundary(RopeCharacterSource(rope), offset)

  def previousGraphemeBoundary(offset: Int): Int =
    TextEditing.previousGraphemeBoundary(RopeCharacterSource(rope), offset)

  def nextGraphemeBoundary(offset: Int): Int =
    TextEditing.nextGraphemeBoundary(RopeCharacterSource(rope), offset)

  def graphemeBoundaryBeforeOrAt(offset: Int): Int =
    TextEditing.graphemeBoundaryBeforeOrAt(RopeCharacterSource(rope), offset)

  def graphemeBoundaryAfterOrAt(offset: Int): Int =
    TextEditing.graphemeBoundaryAfterOrAt(RopeCharacterSource(rope), offset)

  def isWholeGraphemeRange(start: Int, end: Int): Boolean =
    TextEditing.isWholeGraphemeRange(RopeCharacterSource(rope), start, end)
