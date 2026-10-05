package com.serenity.text

import java.text.CharacterIterator
import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}

import com.ibm.icu.lang.{UCharacter, UProperty}
import com.ibm.icu.text.BreakIterator
import com.ibm.icu.util.ULocale

object TextEditing:

  /** Minimal indexed character access for word-boundary scanning without requiring a String. */
  trait CharacterSource:
    def length: Int
    def charAt(index: Int): Char

    /** What `BreakIteratorCache.forSource` compares (by reference) to decide whether the cached `BreakIterator`'s
      * `setText` can be skipped. Defaults to the source itself, which is correct for a source callers reuse the same
      * instance of across calls (e.g. `RopeCharacterSource`). A wrapper type reconstructed fresh on every call --
      * `StringCharacterSource`, built anew by every `String`-based convenience overload in this file -- overrides this
      * to return the identity it actually wraps, so a caller looping the `String` overload over one unchanged `String`
      * (`TextLayoutSnapshot.graphemeBoundaryOffsets`, `CharacterRenderer.computeGraphemeSpans`) still gets cache hits.
      */
    def identityAnchor: AnyRef = this

  /** `Pictograph` runs are never merged: each emoji sequence or flag is its own stop. */
  private[text] enum CharacterClass:
    case Whitespace, Word, Punctuation, Pictograph

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
    text

  def previousWordBoundary(text: String, cursor: Int): Int =
    previousWordBoundary(StringCharacterSource(text), cursor)

  /** Word motion keeps the stops of a class scan (letters and digits, punctuation, whitespace) and lets UAX#29 word
    * boundaries split a run of word characters further, which is what finds the dictionary words inside a spaceless CJK
    * run. Latin text has no boundary inside such a run, so its stops are the class scan's own.
    */
  def previousWordBoundary(source: CharacterSource, cursor: Int): Int =
    val runEnd = scanBackwardWhitespaceStart(source, clamp(cursor, source.length))
    if runEnd <= 0 then 0
    else
      val segmentStart = wordBreakIterator(source).preceding(runEnd)
      val runClass     = characterClassBefore(source, runEnd)
      val runStart     = scanBackwardClassStart(source, runEnd, runClass)
      if characterClassAt(source, segmentStart) == CharacterClass.Pictograph then segmentStart
      else if runClass == CharacterClass.Word then math.max(runStart, segmentStart)
      else runStart

  def nextWordBoundary(text: String, cursor: Int): Int =
    nextWordBoundary(StringCharacterSource(text), cursor)

  def nextWordBoundary(source: CharacterSource, cursor: Int): Int =
    val start = scanForwardWhitespaceEnd(source, clamp(cursor, source.length))
    if start >= source.length then source.length
    else scanForwardWhitespaceEnd(source, runEndAfter(source, start))

  /** Where the run of one class starting at `start` ends, ignoring the whitespace that follows it. */
  private[text] def runEndAfter(source: CharacterSource, start: Int): Int =
    val segmentEnd = wordBreakIterator(source).following(start)
    characterClassAt(source, start) match
      case CharacterClass.Pictograph | CharacterClass.Whitespace => segmentEnd
      case CharacterClass.Word        => math.min(segmentEnd, scanForwardClassEnd(source, start, CharacterClass.Word))
      case CharacterClass.Punctuation => scanForwardClassEnd(source, start, CharacterClass.Punctuation)

  def previousSubWordBoundary(text: String, cursor: Int): Int =
    previousSubWordBoundary(StringCharacterSource(text), cursor)

  def previousSubWordBoundary(source: CharacterSource, cursor: Int): Int =
    SubWordMotion.previous(source, cursor)

  def nextSubWordBoundary(text: String, cursor: Int): Int =
    nextSubWordBoundary(StringCharacterSource(text), cursor)

  def nextSubWordBoundary(source: CharacterSource, cursor: Int): Int =
    SubWordMotion.next(source, cursor)

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

  private[text] def characterClassAt(source: CharacterSource, index: Int): CharacterClass =
    characterClass(codePointAt(source, index))

  private[text] def characterClassBefore(source: CharacterSource, index: Int): CharacterClass =
    characterClass(codePointBefore(source, index))

  private def characterClass(codePoint: Int): CharacterClass =
    if Character.isWhitespace(codePoint) then CharacterClass.Whitespace
    else if isPictograph(codePoint) then CharacterClass.Pictograph
    else
      Character.getType(codePoint) match
        case Character.UPPERCASE_LETTER | Character.LOWERCASE_LETTER | Character.TITLECASE_LETTER |
            Character.MODIFIER_LETTER | Character.OTHER_LETTER | Character.DECIMAL_DIGIT_NUMBER |
            Character.LETTER_NUMBER | Character.OTHER_NUMBER | Character.NON_SPACING_MARK |
            Character.COMBINING_SPACING_MARK =>
          CharacterClass.Word
        case _ =>
          CharacterClass.Punctuation

  private def isPictograph(codePoint: Int): Boolean =
    UCharacter.hasBinaryProperty(codePoint, UProperty.EXTENDED_PICTOGRAPHIC) ||
      UCharacter.hasBinaryProperty(codePoint, UProperty.REGIONAL_INDICATOR)

  private[text] def codePointAt(source: CharacterSource, index: Int): Int =
    val high = source.charAt(index)
    if Character.isHighSurrogate(high) && index + 1 < source.length && Character.isLowSurrogate(
          source.charAt(index + 1)
        )
    then Character.toCodePoint(high, source.charAt(index + 1))
    else high.toInt

  private[text] def codePointBefore(source: CharacterSource, index: Int): Int =
    val low = source.charAt(index - 1)
    if Character.isLowSurrogate(low) && index >= 2 && Character.isHighSurrogate(source.charAt(index - 2)) then
      Character.toCodePoint(source.charAt(index - 2), low)
    else low.toInt

  /** One `BreakIterator` (grapheme unless told otherwise) per thread, paired with the identity anchor
    * ([[CharacterSource.identityAnchor]]) of the `CharacterSource` it was last `setText` onto. `BreakIterator` is
    * stateful and not thread-safe, so a single shared instance isn't safe -- hence `ThreadLocal` -- but constructing
    * `getCharacterInstance()` itself is expensive enough (cloning the rule engine) that doing it on every
    * grapheme-boundary query measurably regressed find/replace over large documents. `preceding` and `following` are
    * meant to be called repeatedly against one `setText` per ICU4J's own usage pattern, so comparing by identity anchor
    * rather than the `CharacterSource` instance itself (compared by reference, not content) lets repeat calls skip
    * `setText` entirely both when a caller reuses the exact same `CharacterSource` instance across calls (e.g.
    * `FindSearch.results` scanning many matches over one document) and when a caller instead loops the `String`-based
    * overloads over one unchanged `String` (`TextLayoutSnapshot.graphemeBoundaryOffsets`,
    * `CharacterRenderer.computeGraphemeSpans`), where a fresh `StringCharacterSource` wrapper is built every call but
    * its `identityAnchor` -- the wrapped `String` -- is not.
    */
  // `private[text]`, not fully `private`: `CharacterSourceIdentityAnchorSpec` (same package) instantiates this
  // directly to whitebox-test the setText-skip decision against the real ICU4J `BreakIterator`.
  final private[text] class BreakIteratorCache(val iterator: BreakIterator = BreakIterator.getCharacterInstance()):
    private val lastAnchor: AtomicReference[Option[AnyRef]] = new AtomicReference(None)

    def forSource(source: CharacterSource): BreakIterator =
      val anchor   = source.identityAnchor
      val previous = lastAnchor.getAndSet(Some(anchor))
      if !previous.exists(_.eq(anchor)) then iterator.setText(CharacterSourceIterator(source))
      iterator

  private val threadLocalBreakIterator: ThreadLocal[BreakIteratorCache] =
    ThreadLocal.withInitial(() => BreakIteratorCache())

  // Its own cache, not a shared one: word motion asks grapheme questions in between, and sharing would re-set the text
  // on every switch.
  private val threadLocalWordBreakIterator: ThreadLocal[BreakIteratorCache] =
    ThreadLocal.withInitial(() => BreakIteratorCache(BreakIterator.getWordInstance(ULocale.ROOT)))

  /** `source` adapted through [[CharacterSourceIterator]] rather than a materialised `String` -- confirmed against
    * `RopeCharacterSource` (#1277 step 3 spike) to work directly against a rope-backed source, so a large line's
    * grapheme scan never copies the whole line into a `String`.
    */
  private[text] def graphemeBreakIterator(source: CharacterSource): BreakIterator =
    threadLocalBreakIterator.get().forSource(source)

  private[text] def wordBreakIterator(source: CharacterSource): BreakIterator =
    threadLocalWordBreakIterator.get().forSource(source)

  @annotation.tailrec
  private[text] def scanBackwardClassStart(source: CharacterSource, index: Int, targetClass: CharacterClass): Int =
    if index > 0 && characterClassBefore(source, index) == targetClass then
      scanBackwardClassStart(source, index - Character.charCount(codePointBefore(source, index)), targetClass)
    else index

  @annotation.tailrec
  private[text] def scanForwardClassEnd(source: CharacterSource, index: Int, targetClass: CharacterClass): Int =
    if index < source.length && characterClassAt(source, index) == targetClass then
      scanForwardClassEnd(source, index + Character.charCount(codePointAt(source, index)), targetClass)
    else index

  private[text] def scanBackwardWhitespaceStart(source: CharacterSource, index: Int): Int =
    scanBackwardClassStart(source, index, CharacterClass.Whitespace)

  private[text] def scanForwardWhitespaceEnd(source: CharacterSource, index: Int): Int =
    scanForwardClassEnd(source, index, CharacterClass.Whitespace)

  // `private[text]`, not fully `private`: `CharacterSourceIdentityAnchorSpec` (same package) instantiates this
  // directly to whitebox-test the identity-anchor cache-skip mechanism against the real ICU4J `BreakIterator`.
  final private[text] case class StringCharacterSource(text: String) extends CharacterSource:
    override def length: Int =
      text.length

    override def charAt(index: Int): Char =
      text.charAt(index)

    override def identityAnchor: AnyRef =
      text

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
