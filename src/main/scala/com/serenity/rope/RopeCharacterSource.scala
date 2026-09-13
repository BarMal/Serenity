package com.serenity.rope

import java.util.concurrent.atomic.AtomicReference

import com.serenity.text.TextEditing

/** Adapts a [[Rope]] to `TextEditing.CharacterSource` so the word/grapheme-boundary scanners in `TextEditing` can
  * operate directly on rope content without materialising a `String`.
  *
  * `charAt` caches the one leaf chunk covering its most recent access. `TextEditing`'s word/grapheme boundary scanners
  * only ever step by one character at a time, forward or backward, so almost every call lands back in the cached chunk
  * (an O(1) `String#charAt`) instead of re-descending the rope from the root via `Rope.index` (#1458, where a
  * k-character scan cost O(k log n)) -- only a genuine chunk crossing pays the O(log n) `chunksInRange` walk.
  */
final case class RopeCharacterSource(rope: Rope) extends TextEditing.CharacterSource:
  // Plain `AtomicReference`, not `Ref[IO, _]`: `charAt` is a pure synchronous scan (no `IO`, no fiber ever shares
  // this instance), so there is no async boundary for `Ref` to protect and no reason to lift this into `IO`.
  private val cachedChunk = new AtomicReference[Option[(Int, String)]](None)

  override def length: Int = rope.weight

  override def charAt(index: Int): Char =
    cachedChunk.get() match
      case Some((chunkOffset, chunk)) if index >= chunkOffset && index < chunkOffset + chunk.length =>
        chunk.charAt(index - chunkOffset)
      case _ =>
        rope.chunksInRange(index, index + 1).nextOption() match
          case Some(entry @ (chunkOffset, chunk)) =>
            cachedChunk.set(Some(entry))
            chunk.charAt(index - chunkOffset)
          case None => '\u0000'

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
