package com.serenity.text

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SmartPunctuationSpec extends AnyFlatSpec with Matchers:

  "replacementFor" should "leave a lone hyphen typed with no preceding hyphen unchanged" in {
    SmartPunctuation.replacementFor('-', "a") shouldBe None
  }

  it should "turn a second consecutive hyphen into an em dash, replacing the first" in {
    SmartPunctuation.replacementFor('-', "a-") shouldBe Some((1, "—"))
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
