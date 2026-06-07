package com.serenity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import com.serenity.rope.{Balance, Rope}

class UnderscoreStorageSpec extends AnyFlatSpec with Matchers:

  "Rope" should "correctly store and retrieve underscore characters" in {
    given Balance = Balance.default

    val textWithUnderscores = "test_with_underscores"
    val rope                = Rope(textWithUnderscores)

    rope.collect() shouldBe textWithUnderscores

    // Verify character by character
    rope.index(4) shouldBe Some('_') // first underscore
    rope.index(9) shouldBe Some('_') // second underscore
  }

  it should "correctly insert underscore characters" in {
    given Balance = Balance.default

    val originalText = "test"
    val rope         = Rope(originalText)

    val updatedRope = rope.insert(4, "_added")
    updatedRope.collect() shouldBe "test_added"
    updatedRope.index(4) shouldBe Some('_')
  }

  it should "correctly retrieve lines containing underscores" in {
    given Balance = Balance.default

    val textWithUnderscores = "first_line\nsecond_line_with_more"
    val rope                = Rope(textWithUnderscores)

    val firstLine = rope.getLine(0)
    firstLine shouldBe Some("first_line")

    val secondLine = rope.getLine(1)
    secondLine shouldBe Some("second_line_with_more")
  }
