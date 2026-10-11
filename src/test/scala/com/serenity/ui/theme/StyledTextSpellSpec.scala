package com.serenity.ui.theme

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class StyledTextSpellSpec extends AnyFlatSpec with Matchers:

  private def segments(contents: String*): List[StyledText] = contents.map(StyledText(_)).toList

  private val cases = List(
    (segments("foo", "bar"), "foobar"),
    (segments("foo", "bar"), "foobaz"),
    (segments("foo", "bar"), "foo"),
    (segments("foo", "bar"), "foobarbaz"),
    (segments("", "foo", ""), "foo"),
    (segments(), ""),
    (segments(), "x"),
    (segments("x"), ""),
    (segments("ab", "c"), "abd"),
    (segments("á", "b"), "áb")
  )

  "StyledText.spell" should "agree with comparing the concatenated contents" in
    cases.foreach { (segs, text) =>
      withClue(s"$segs against '$text': ") {
        StyledText.spell(segs, text) shouldBe (segs.map(_.content).mkString == text)
      }
    }
