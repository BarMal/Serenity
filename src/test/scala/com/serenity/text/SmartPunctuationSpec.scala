package com.serenity.text

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SmartPunctuationSpec extends AnyFlatSpec with Matchers:

  "replacementFor" should "leave a lone hyphen typed with no preceding hyphen unchanged" in {
    SmartPunctuation.replacementFor('-', "a") shouldBe None
  }

  it should "turn a second consecutive hyphen into an en dash, replacing the first" in {
    SmartPunctuation.replacementFor('-', "a-") shouldBe Some((1, "–"))
  }

  it should "turn a hyphen typed after an en dash into an em dash, so three hyphens make one em dash" in {
    SmartPunctuation.replacementFor('-', "a–") shouldBe Some((1, "—"))
  }

  it should "leave hyphens literal on a line holding only hyphens so far, a thematic break or front-matter fence" in {
    SmartPunctuation.replacementFor('-', "-") shouldBe None
    SmartPunctuation.replacementFor('-', "--") shouldBe None
  }

  it should "leave hyphens literal in a Markdown table delimiter row" in {
    SmartPunctuation.replacementFor('-', "|-") shouldBe None
    SmartPunctuation.replacementFor('-', "| :--- | -") shouldBe None
  }

  it should "leave a lone period typed with no preceding periods unchanged" in {
    SmartPunctuation.replacementFor('.', "a") shouldBe None
  }

  it should "leave a second consecutive period unchanged" in {
    SmartPunctuation.replacementFor('.', "a.") shouldBe None
  }

  it should "turn a third consecutive period into a true ellipsis, replacing the first two" in {
    SmartPunctuation.replacementFor('.', "a..") shouldBe Some((2, "…"))
  }

  it should "open a double quote at the start of the buffer" in {
    SmartPunctuation.replacementFor('"', "") shouldBe Some((0, "“"))
  }

  it should "open a double quote after whitespace" in {
    SmartPunctuation.replacementFor('"', "say ") shouldBe Some((0, "“"))
  }

  it should "open a double quote after an opening bracket" in {
    SmartPunctuation.replacementFor('"', "(") shouldBe Some((0, "“"))
  }

  it should "close a double quote after a letter" in {
    SmartPunctuation.replacementFor('"', "hello") shouldBe Some((0, "”"))
  }

  it should "open a single quote at the start of the buffer" in {
    SmartPunctuation.replacementFor('\'', "") shouldBe Some((0, "‘"))
  }

  it should "close a single quote after a letter, matching apostrophe use" in {
    SmartPunctuation.replacementFor('\'', "don") shouldBe Some((0, "’"))
  }

  it should "leave characters with no smart-punctuation rule unchanged" in {
    SmartPunctuation.replacementFor('x', "a") shouldBe None
  }

  "withinInlineCode" should "report a caret after an unclosed backtick as inside a code span" in {
    SmartPunctuation.withinInlineCode("call `a-") shouldBe true
  }

  it should "report a caret after a closed code span as outside it" in {
    SmartPunctuation.withinInlineCode("call `a--` then") shouldBe false
  }
