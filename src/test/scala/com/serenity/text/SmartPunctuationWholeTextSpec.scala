package com.serenity.text

import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class SmartPunctuationWholeTextSpec extends AnyPropSpec with ScalaCheckPropertyChecks with Matchers:

  private val asciiProse: Gen[String] =
    Gen.listOf(Gen.oneOf("a", "b", " ", "'", "\"", "-", ".", "`", "\n", "(", "|")).map(_.mkString)

  private val anyProse: Gen[String] =
    Gen.listOf(Gen.oneOf("a", " ", "'", "\"", "-", ".", "`", "\n", "“", "”", "‘", "’", "–", "—", "…")).map(_.mkString)

  property("educate matches what typing the text would have produced") {
    SmartPunctuation.educate("\"Wait--no...\" she said. 'It's---'") shouldBe
      "“Wait–no…” she said. ‘It’s—‘"
  }

  property("educate leaves code spans and rule rows alone") {
    SmartPunctuation.educate("use `\"x\" -- y` here\n---\n|--|") shouldBe "use `\"x\" -- y` here\n---\n|--|"
  }

  property("educate is idempotent") {
    forAll(anyProse)(text =>
      SmartPunctuation.educate(SmartPunctuation.educate(text)) shouldBe SmartPunctuation.educate(text)
    )
  }

  property("straighten undoes educate on ASCII text") {
    forAll(asciiProse)(text => SmartPunctuation.straighten(SmartPunctuation.educate(text)) shouldBe text)
  }

  property("educateTagged leaves verbatim characters alone and tags a replacement with its trigger") {
    val tagged = "a-".toVector.map(_ -> 1) ++ "-\"".toVector.map(_ -> 2) ++ "\"".toVector.map(_ -> 3)

    SmartPunctuation.educateTagged(tagged, _ == 3) shouldBe Vector('a' -> 1, '–' -> 2, '“' -> 2, '"' -> 3)
  }
