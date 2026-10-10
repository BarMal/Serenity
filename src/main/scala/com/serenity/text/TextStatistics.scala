package com.serenity.text

import java.util.concurrent.atomic.AtomicReference

import com.serenity.rope.Rope

/** Word/character counts and an estimated reading time for a span of prose.
  *
  * Words and characters are defined by [[TextCounts]]: `characterCount` counts grapheme clusters, whitespace included,
  * and `characterCountExcludingWhitespace` leaves the whitespace ones out. `of` only reads the root node's
  * incrementally maintained summary (see `Rope.textSummary`) rather than rescanning the whole buffer.
  */
final case class TextStatistics(
    wordCount: Int,
    characterCount: Int,
    characterCountExcludingWhitespace: Int
):

  /** Whole minutes, rounded up, at `TextStatistics.wordsPerMinute`. Zero only for zero words -- any non-empty text
    * reads as at least one minute rather than "0 min read".
    */
  def readingTimeMinutes: Int =
    if wordCount <= 0 then 0 else math.ceil(wordCount.toDouble / TextStatistics.wordsPerMinute).toInt

object TextStatistics:

  /** Average adult silent-reading speed, in words per minute -- the figure commonly used by reading-time estimators
    * (e.g. Medium's).
    */
  val wordsPerMinute: Int = 200

  val empty: TextStatistics = TextStatistics(0, 0, 0)

  /** O(1): reads the fields `Rope` already maintains incrementally as edits land, rather than walking the buffer. */
  def of(content: Rope): TextStatistics =
    TextStatistics(
      wordCount = content.wordCount,
      characterCount = content.characterCount,
      characterCountExcludingWhitespace = content.nonWhitespaceCount
    )

  /** For a bounded, already-materialised span of text -- a selection, typically -- where the span itself (not the whole
    * document) sets the cost of counting.
    */
  def ofString(text: String): TextStatistics =
    val counts = TextCounts.of(text)
    TextStatistics(
      wordCount = counts.words,
      characterCount = counts.characters,
      characterCountExcludingWhitespace = counts.nonWhitespaceCharacters
    )

  final private case class RangeEntry(content: Rope, start: Int, end: Int, statistics: TextStatistics)

  private val lastRange = new AtomicReference[Option[RangeEntry]](None)

  /** Counts `content[start, end)` by slicing the rope, which reuses the cached summaries of every subtree the range
    * covers, so no selection text is copied or rescanned. The last answer is kept: a status line asks again on every
    * frame, usually about the very same rope and selection.
    */
  def ofRange(content: Rope, start: Int, end: Int): TextStatistics =
    lastRange.get.filter(entry => (entry.content eq content) && entry.start == start && entry.end == end) match
      case Some(entry) => entry.statistics
      case None =>
        val statistics = of(content.slice(start, end))
        lastRange.set(Some(RangeEntry(content, start, end, statistics)))
        statistics
