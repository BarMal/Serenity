package com.serenity.text

import java.text.CharacterIterator
import java.util.concurrent.atomic.AtomicInteger

import com.ibm.icu.text.BreakIterator

object TextEditing:

  /** Minimal indexed character access for word-boundary scanning without requiring a String. */
  trait CharacterSource:
    def length: Int
    def charAt(index: Int): Char

  private enum CharacterClass:
    case Whitespace, Word, Punctuation

  def deleteWordBackward(text: String): String =
    val boundary = previousWordBoundary(text, text.length)
    text.substring(0, boundary) + text.substring(text.length)

  /** Every real call site passes the full field text with no separate cursor position -- these are single-line fields
    * (goto-line, find/replace, command palette search, file-dialog filename/path) whose cursor is implicitly pinned to
    * the end, mirroring how the sibling single-character `deleteForwardFromActiveField` is a no-op for the same fields.
    * There is nothing after an end-pinned cursor to delete, so this is correctly a no-op today; it exists for symmetry
    * with `deleteWordBackward` and to be ready if a field ever gains real interior-cursor tracking.
    */
  def deleteWordForward(text: String): String =
    val boundary = nextWordBoundary(text, text.length)
    text.substring(0, boundary) + text.substring(text.length)

  def previousWordBoundary(text: String, cursor: Int): Int =
    previousWordBoundary(StringCharacterSource(text), cursor)

  def previousWordBoundary(source: CharacterSource, cursor: Int): Int =
    val idx = clamp(cursor, source.length)
    val segmentEnd =
      if idx > 0 && characterClass(source.charAt(idx - 1)) == CharacterClass.Whitespace then
        scanBackwardClassStart(source, idx, CharacterClass.Whitespace)
      else idx

    if segmentEnd <= 0 then 0
    else scanBackwardClassStart(source, segmentEnd, characterClass(source.charAt(segmentEnd - 1)))

  def nextWordBoundary(text: String, cursor: Int): Int =
    nextWordBoundary(StringCharacterSource(text), cursor)

  def nextWordBoundary(source: CharacterSource, cursor: Int): Int =
    val length = source.length
    val idx    = clamp(cursor, length)
    val segmentStart =
      if idx < length && characterClass(source.charAt(idx)) == CharacterClass.Whitespace then
        scanForwardClassEnd(source, idx, CharacterClass.Whitespace)
      else idx

    if segmentStart >= length then length
    else
      val segmentEnd = scanForwardClassEnd(source, segmentStart, characterClass(source.charAt(segmentStart)))
      if segmentEnd < length && characterClass(source.charAt(segmentEnd)) == CharacterClass.Whitespace then
        scanForwardClassEnd(source, segmentEnd, CharacterClass.Whitespace)
      else segmentEnd

  def previousGraphemeBoundary(text: String, cursor: Int): Int =
    previousGraphemeBoundary(StringCharacterSource(text), cursor)

  /** Rewinds to the grapheme-cluster boundary before `cursor`, per Unicode UAX#29 extended grapheme clusters
    * (`BreakIterator.getCharacterInstance`) -- correctly joining Hangul jamo, Indic conjuncts, ZWJ-joined emoji
    * sequences (gated on Extended_Pictographic, unlike a bare unconditional ZWJ join) and regional-indicator flag
    * pairs, none of which a codepoint-class-only walk can get right on its own (#1277).
    */
  def previousGraphemeBoundary(source: CharacterSource, cursor: Int): Int =
    val idx = clamp(cursor, source.length)
    if idx <= 0 then 0
    else graphemeBreakIterator(source).preceding(idx)

  def nextGraphemeBoundary(text: String, cursor: Int): Int =
    nextGraphemeBoundary(StringCharacterSource(text), cursor)

  def nextGraphemeBoundary(source: CharacterSource, cursor: Int): Int =
    val idx = clamp(cursor, source.length)
    if idx >= source.length then source.length
    else graphemeBreakIterator(source).following(idx)

  def graphemeBoundaryBeforeOrAt(text: String, cursor: Int): Int =
    graphemeBoundaryBeforeOrAt(StringCharacterSource(text), cursor)

  def graphemeBoundaryBeforeOrAt(source: CharacterSource, cursor: Int): Int =
    val idx = clamp(cursor, source.length)
    if idx <= 0 then 0
    else
      val previous = previousGraphemeBoundary(source, idx)
      if nextGraphemeBoundary(source, previous) == idx then idx
      else previous

  def graphemeBoundaryAfterOrAt(text: String, cursor: Int): Int =
    graphemeBoundaryAfterOrAt(StringCharacterSource(text), cursor)

  def graphemeBoundaryAfterOrAt(source: CharacterSource, cursor: Int): Int =
    val idx = clamp(cursor, source.length)
    if idx >= source.length then source.length
    else
      val next = nextGraphemeBoundary(source, idx)
      if previousGraphemeBoundary(source, next) == idx then idx
      else next

  def isWholeGraphemeRange(text: String, start: Int, end: Int): Boolean =
    isWholeGraphemeRange(StringCharacterSource(text), start, end)

  def isWholeGraphemeRange(source: CharacterSource, start: Int, end: Int): Boolean =
    val normalizedStart = clamp(start, source.length)
    val normalizedEnd   = clamp(end, source.length)
    normalizedStart <= normalizedEnd &&
    graphemeBoundaryAfterOrAt(source, normalizedStart) == normalizedStart &&
    graphemeBoundaryBeforeOrAt(source, normalizedEnd) == normalizedEnd

  private def clamp(cursor: Int, length: Int): Int =
    math.max(0, math.min(cursor, length))

  private def characterClass(char: Char): CharacterClass =
    if char.isWhitespace then CharacterClass.Whitespace
    else
      Character.getType(char) match
        case Character.UPPERCASE_LETTER | Character.LOWERCASE_LETTER | Character.TITLECASE_LETTER |
            Character.MODIFIER_LETTER | Character.OTHER_LETTER | Character.DECIMAL_DIGIT_NUMBER |
            Character.LETTER_NUMBER | Character.OTHER_NUMBER | Character.NON_SPACING_MARK |
            Character.COMBINING_SPACING_MARK =>
          CharacterClass.Word
        case _ =>
          CharacterClass.Punctuation

  /** One `BreakIterator.getCharacterInstance` per thread, reused across calls via `setText` rather than constructed
    * fresh each time. `BreakIterator` is stateful and not thread-safe, so a single shared instance isn't safe -- hence
    * `ThreadLocal` -- but constructing `getCharacterInstance()` itself is expensive enough (cloning the rule engine)
    * that doing it on every grapheme-boundary query measurably regressed find/replace over large documents, which
    * resolves thousands of matches by calling `isWholeGraphemeRange` (and so this) per match.
    */
  private val threadLocalBreakIterator: ThreadLocal[BreakIterator] =
    ThreadLocal.withInitial(() => BreakIterator.getCharacterInstance())

  /** `source` adapted through [[CharacterSourceIterator]] rather than a materialised `String` -- confirmed against
    * `RopeCharacterSource` (#1277 step 3 spike) to work directly against a rope-backed source, so a large line's
    * grapheme scan never copies the whole line into a `String`.
    */
  private def graphemeBreakIterator(source: CharacterSource): BreakIterator =
    val boundary = threadLocalBreakIterator.get()
    boundary.setText(CharacterSourceIterator(source))
    boundary

  @annotation.tailrec
  private def scanBackwardClassStart(source: CharacterSource, idx: Int, targetClass: CharacterClass): Int =
    if idx > 0 && characterClass(source.charAt(idx - 1)) == targetClass then
      scanBackwardClassStart(source, idx - 1, targetClass)
    else idx

  @annotation.tailrec
  private def scanForwardClassEnd(source: CharacterSource, idx: Int, targetClass: CharacterClass): Int =
    if idx < source.length && characterClass(source.charAt(idx)) == targetClass then
      scanForwardClassEnd(source, idx + 1, targetClass)
    else idx

  final private case class StringCharacterSource(text: String) extends CharacterSource:
    override def length: Int =
      text.length

    override def charAt(index: Int): Char =
      text.charAt(index)

  /** Adapts a `CharacterSource` to `java.text.CharacterIterator` so ICU4J's `BreakIterator` can walk it directly -- a
    * rope included -- without ever materialising a `String` (#1277 step 3). The current position is genuinely mutable
    * state intrinsic to `CharacterIterator`'s contract (`first`/`next`/`setIndex` etc. all move and return from one
    * cursor), so it is held in an `AtomicInteger` rather than a `var`, per this file's no-`var` convention.
    */
  final private class CharacterSourceIterator(source: CharacterSource) extends CharacterIterator:
    private val position = AtomicInteger(0)

    private def charOrDone(index: Int): Char =
      if index < 0 || index >= source.length then CharacterIterator.DONE else source.charAt(index)

    override def first(): Char =
      position.set(0)
      charOrDone(0)

    override def last(): Char =
      val lastIndex = math.max(0, source.length - 1)
      position.set(lastIndex)
      if source.length == 0 then CharacterIterator.DONE else charOrDone(lastIndex)

    override def current(): Char =
      charOrDone(position.get())

    override def next(): Char =
      val advanced = position.incrementAndGet()
      if advanced >= source.length then
        position.set(source.length)
        CharacterIterator.DONE
      else charOrDone(advanced)

    override def previous(): Char =
      if position.get() <= 0 then CharacterIterator.DONE
      else charOrDone(position.decrementAndGet())

    override def setIndex(index: Int): Char =
      position.set(index)
      charOrDone(index)

    override def getBeginIndex: Int =
      0

    override def getEndIndex: Int =
      source.length

    override def getIndex: Int =
      position.get()

    override def clone(): AnyRef =
      val copy = CharacterSourceIterator(source)
      copy.position.set(position.get())
      copy
