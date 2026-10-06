package com.serenity.text

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
