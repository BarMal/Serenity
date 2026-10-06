package com.serenity.rope

import com.serenity.text.TextCounts
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RopeSummaryLazinessSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val megabyte = "lorem ipsum dolor sit amet\n" * 40000

  "Rope" should "segment no text while it is built, edited, searched or measured" in {
    val before = TextCounts.segmentationCount

    val rope   = Rope(megabyte)
    val edited = rope.insert(500000, "word ").flatMap(_.delete(10, 20)).getOrElse(rope)
    edited.weight should be > 0
    edited.newlineCount should be > 0

    TextCounts.segmentationCount shouldBe before
  }

  it should "segment only on demand, and reuse the summary until the rope is edited" in {
    val rope            = Rope(megabyte)
    val words           = rope.wordCount
    val afterFirstQuery = TextCounts.segmentationCount

    rope.wordCount shouldBe words
    TextCounts.segmentationCount shouldBe afterFirstQuery

    val edited = rope.insert(500000, "extra ").getOrElse(rope)
    edited.wordCount shouldBe words + 1
    TextCounts.segmentationCount - afterFirstQuery should be < 200L
  }
