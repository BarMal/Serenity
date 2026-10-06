package com.serenity.rope

import com.serenity.text.TextCounts
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RopeSummaryLazinessSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val megabyte = "lorem ipsum dolor sit amet\n" * 40000

  "Rope" should "segment no text while it is built, edited, searched or measured" in {
    val (_, segmentations) = TextCounts.countSegmentations {
      val rope   = Rope(megabyte)
      val edited = rope.insert(500000, "word ").flatMap(_.delete(10, 20)).getOrElse(rope)
      edited.weight should be > 0
      edited.newlineCount should be > 0
    }

    segmentations shouldBe 0L
  }

  it should "segment only on demand, and reuse the summary until the rope is edited" in {
    val rope                 = Rope(megabyte)
    val (words, _)           = TextCounts.countSegmentations(rope.wordCount)
    val (_, reused)          = TextCounts.countSegmentations(rope.wordCount shouldBe words)
    val (editedWords, extra) = TextCounts.countSegmentations(rope.insert(500000, "extra ").getOrElse(rope).wordCount)

    reused shouldBe 0L
    editedWords shouldBe words + 1
    extra should be < 200L
  }
