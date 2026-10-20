package com.serenity.text

import com.serenity.rope.{Balance, Rope, RopeBuilder}
import com.serenity.testkit.Generators
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class TextStatisticsRangeSpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  given Balance = Balance.default

  private val tokens: Gen[String] = Gen.oneOf(
    Gen.alphaNumChar.map(_.toString),
    Gen.oneOf(" ", "  ", "\n", ".", ",", "-", "'", "well-known", "café"),
    Gen.oneOf("我", "東京", "ひらがな")
  )

  private val ropeWithRange: Gen[(Rope, Int, Int)] =
    for
      text  <- Gen.listOf(tokens).map(_.mkString)
      rope  <- Generators.ropeOfShape(text)
      start <- Gen.choose(0, text.length)
      end   <- Gen.choose(start, text.length)
    yield (rope, start, end)

  "Statistics of a range" should "equal counting the materialised selection, whatever the leaf splits" in
    forAll(ropeWithRange) { (rope, start, end) =>
      TextStatistics.ofRange(rope, start, end) shouldBe TextStatistics.ofString(rope.sliceString(start, end))
    }

  it should "count a selection that splits a word at a leaf boundary as the whole word" in {
    val rope = RopeBuilder.fromLeaves(List("alp", "ha beta"))

    TextStatistics.ofRange(rope, 0, 5).wordCount shouldBe 1
    TextStatistics.ofRange(rope, 1, 7).wordCount shouldBe 2
  }

  it should "be remembered by a memo for the same rope and selection" in {
    val rope = Rope("one two three four")
    val memo = RangeStatisticsMemo()

    val first = memo.of(rope, 0, 13)

    memo.of(rope, 0, 13) should be theSameInstanceAs first
    memo.computations shouldBe 1L
  }

  it should "be recomputed for the same selection of an edited rope" in {
    val rope   = Rope("one two three")
    val edited = rope.insert(3, " and").getOrElse(rope)

    val memo = RangeStatisticsMemo()

    memo.of(rope, 0, 7).wordCount shouldBe 2
    memo.of(edited, 0, 7).wordCount shouldBe 2
    memo.of(edited, 0, 11).wordCount shouldBe 3
    memo.computations shouldBe 3L
  }
