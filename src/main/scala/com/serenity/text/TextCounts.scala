package com.serenity.text

import scala.annotation.tailrec

import com.ibm.icu.text.BreakIterator

/** Words and characters as a writer counts them (#1949).
  *
  * A word is a UAX#29 word segment of letters or numbers, except that ideographs and kana count one per character, the
  * usual unit for CJK writing goals and the only definition that stays exact when text is split into rope leaves.
  * Letter segments joined by a lone hyphen count once, so "well-known" stays one word as it was under whitespace
  * counting. Punctuation, symbols and emoji are not words. A character is a grapheme cluster, so an emoji ZWJ sequence
  * is one character.
  */
final case class TextCounts(words: Int, characters: Int, nonWhitespaceCharacters: Int):

  def +(that: TextCounts): TextCounts =
    TextCounts(
      words + that.words,
      characters + that.characters,
      nonWhitespaceCharacters + that.nonWhitespaceCharacters
    )

  def -(that: TextCounts): TextCounts =
    TextCounts(
      words - that.words,
      characters - that.characters,
      nonWhitespaceCharacters - that.nonWhitespaceCharacters
    )

object TextCounts:

  private enum SegmentKind:
    case Word, PerCharacter, Hyphen, Other

  private val hyphens: Set[Char] = Set('-', '\u2010', '\u2011')

  /** The placeholders of a rich document's soft breaks (U+FFFC) and blocks (U+2064): not characters of prose. */
  private val InlineAtomPlaceholders: Set[Int] = Set(0xfffc, 0x2064)

  // Separate from `TextEditing`'s iterators: counting sets arbitrary strings, which would evict their cached text.
  private val threadLocalWords: ThreadLocal[BreakIterator] =
    ThreadLocal.withInitial(() => BreakIterator.getWordInstance())

  private val threadLocalCharacters: ThreadLocal[BreakIterator] =
    ThreadLocal.withInitial(() => BreakIterator.getCharacterInstance())

  // One cell per thread, so counting costs a thread-local read and an increment, and no thread sees another's runs.
  private val threadLocalSegmentations: ThreadLocal[Array[Long]] =
    ThreadLocal.withInitial(() => new Array[Long](1))

  /** Runs `body` and returns its result with how many texts the calling thread segmented meanwhile; lets specs prove
    * that building or editing a rope segments nothing. Work other threads do concurrently is not counted, and neither
    * is work `body` hands to another thread.
    */
  private[serenity] def countSegmentations[A](body: => A): (A, Long) =
    val runs   = threadLocalSegmentations.get()
    val before = runs(0)
    val result = body
    (result, runs(0) - before)

  def of(text: String): TextCounts =
    threadLocalSegmentations.get()(0) += 1
    val characters = threadLocalCharacters.get()
    characters.setText(text)
    val words = threadLocalWords.get()
    words.setText(text)
    val (characterCount, nonWhitespaceCount) = countCharacters(characters, text)
    TextCounts(countWords(words, characters, text), characterCount, nonWhitespaceCount)

  private def countCharacters(characters: BreakIterator, text: String): (Int, Int) =
    @tailrec
    def loop(start: Int, count: Int, nonWhitespace: Int): (Int, Int) =
      val end = characters.following(start)
      if end == BreakIterator.DONE then (count, nonWhitespace)
      else
        val codePoint = text.codePointAt(start)
        val visible   = if Character.isWhitespace(codePoint) || InlineAtomPlaceholders.contains(codePoint) then 0 else 1
        loop(end, count + 1, nonWhitespace + visible)

    loop(0, 0, 0)

  private def countWords(words: BreakIterator, characters: BreakIterator, text: String): Int =
    @tailrec
    def loop(start: Int, previous: SegmentKind, beforePrevious: SegmentKind, count: Int): Int =
      val end = words.following(start)
      if end == BreakIterator.DONE then count
      else
        val kind = segmentKind(words.getRuleStatus, text, start, end)
        val added = kind match
          case SegmentKind.Word =>
            if previous == SegmentKind.Hyphen && beforePrevious == SegmentKind.Word then 0 else 1
          case SegmentKind.PerCharacter               => graphemesBetween(characters, start, end)
          case SegmentKind.Hyphen | SegmentKind.Other => 0
        loop(end, kind, previous, count + added)

    loop(0, SegmentKind.Other, SegmentKind.Other, 0)

  private def segmentKind(status: Int, text: String, start: Int, end: Int): SegmentKind =
    if status >= BreakIterator.WORD_KANA && status < BreakIterator.WORD_IDEO_LIMIT then SegmentKind.PerCharacter
    else if status >= BreakIterator.WORD_NONE_LIMIT && status < BreakIterator.WORD_LETTER_LIMIT then SegmentKind.Word
    else if end - start == 1 && hyphens.contains(text.charAt(start)) then SegmentKind.Hyphen
    else SegmentKind.Other

  private def graphemesBetween(characters: BreakIterator, start: Int, end: Int): Int =
    @tailrec
    def loop(offset: Int, count: Int): Int =
      if offset >= end then count
      else loop(characters.following(offset), count + 1)

    loop(start, 0)

/** What a rope node needs to combine its children's counts without rescanning them.
  *
  * Word and grapheme rules look only a few characters either side of a position, so the counts of two joined texts
  * differ from the sum of their counts only near the join. Recounting `SeamWidth` characters either side measures that
  * difference exactly; the same artificial start cuts into both the seam and the stored edge, so it cancels out. Two
  * things are not local and can be off by about one per join: dictionary segmentation of Thai, Lao, Khmer and Myanmar,
  * and runs of more than `SeamWidth / 4` regional-indicator flags.
  */
final case class TextSummary(
    counts: TextCounts,
    head: String,
    headCounts: TextCounts,
    tail: String,
    tailCounts: TextCounts
)

object TextSummary:

  val SeamWidth: Int = 32

  def of(text: String): TextSummary =
    val counts = TextCounts.of(text)
    if text.length <= SeamWidth then TextSummary(counts, text, counts, text, counts)
    else
      val head = text.take(SeamWidth)
      val tail = text.takeRight(SeamWidth)
      TextSummary(counts, head, TextCounts.of(head), tail, TextCounts.of(tail))

  def join(left: TextSummary, right: TextSummary): TextSummary =
    if left.head.isEmpty then right
    else if right.head.isEmpty then left
    else
      val seam = TextCounts.of(left.tail + right.head) - left.tailCounts - right.headCounts
      val (head, headCounts) =
        if left.head.length == SeamWidth then (left.head, left.headCounts)
        else withCounts((left.head + right.head).take(SeamWidth))
      val (tail, tailCounts) =
        if right.tail.length == SeamWidth then (right.tail, right.tailCounts)
        else withCounts((left.tail + right.tail).takeRight(SeamWidth))
      TextSummary(left.counts + right.counts + seam, head, headCounts, tail, tailCounts)

  private def withCounts(edge: String): (String, TextCounts) =
    (edge, TextCounts.of(edge))
