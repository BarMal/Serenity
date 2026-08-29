package com.serenity

import com.serenity.rope.{Balance, Rope}
import com.serenity.text.TextStatistics
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Pure word/character-counting specs for #1203 -- the acceptance criteria's first bullet: empty buffer, single word,
  * multi-paragraph text, unicode/CJK content, punctuation-only content.
  */
class TextStatisticsSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  "TextStatistics.of" should "report all-zero counts for an empty buffer" in {
    val stats = TextStatistics.of(Rope.empty)

    stats.wordCount shouldBe 0
    stats.characterCount shouldBe 0
    stats.characterCountExcludingWhitespace shouldBe 0
    stats.readingTimeMinutes shouldBe 0
  }

  it should "count a single word" in {
    val stats = TextStatistics.of(Rope("hello"))

    stats.wordCount shouldBe 1
    stats.characterCount shouldBe 5
    stats.characterCountExcludingWhitespace shouldBe 5
    stats.readingTimeMinutes shouldBe 1
  }

  it should "count words and characters across multi-paragraph text" in {
    val text  = "The quick brown fox jumps over the lazy dog.\n\nIt ran away, fast."
    val stats = TextStatistics.of(Rope(text))

    stats.wordCount shouldBe 13
    stats.characterCount shouldBe text.length
    stats.characterCountExcludingWhitespace shouldBe text.count(!_.isWhitespace)
  }

  it should "count unicode and CJK content by maximal non-whitespace run" in {
    val stats = TextStatistics.of(Rope("café naïve 你好世界 emoji🎉test"))

    stats.wordCount shouldBe 4
  }

  it should "count punctuation-only content as words" in {
    val stats = TextStatistics.of(Rope("--- ... ???"))

    stats.wordCount shouldBe 3
    stats.characterCountExcludingWhitespace shouldBe 9
  }

  it should "estimate reading time at the standard 200 words per minute, rounded up" in {
    val words = (1 to 450).map(i => s"word$i").mkString(" ")

    TextStatistics.of(Rope(words)).readingTimeMinutes shouldBe 3 // ceil(450 / 200.0)
  }

  "TextStatistics.ofString" should "agree with TextStatistics.of on the same text" in {
    val text = "one two three, four-five six\nseven"

    TextStatistics.ofString(text) shouldBe TextStatistics.of(Rope(text))
  }

  it should "report all-zero counts for an empty string" in {
    TextStatistics.ofString("") shouldBe TextStatistics.empty
  }
