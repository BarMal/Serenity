package com.serenity

import com.serenity.text.TextEditing
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TextEditingSpec extends AnyFlatSpec with Matchers:

  "TextEditing" should "delete accented Latin words as a single word segment" in {
    TextEditing.deleteWordBackward("cafe\u0301 naive") shouldBe "cafe\u0301 "
    TextEditing.deleteWordBackward("cafe\u0301 naive ") shouldBe "cafe\u0301 "
  }

  it should "delete non-Latin words as a single word segment" in {
    TextEditing.deleteWordBackward("hello \u041F\u0440\u0438\u0432\u0435\u0442") shouldBe "hello "
    TextEditing.deleteWordBackward("hello \u6771\u4EAC") shouldBe "hello "
  }

  it should "treat punctuation as a separate word-navigation segment" in {
    TextEditing.deleteWordBackward("hello,") shouldBe "hello"
    TextEditing.deleteWordBackward("hello, ") shouldBe "hello"

    TextEditing.nextWordBoundary("hello, world", 0) shouldBe 5
    TextEditing.nextWordBoundary("hello, world", 5) shouldBe 7
    TextEditing.nextWordBoundary("hello, world", 7) shouldBe 12
  }

  it should "step over surrogate-pair emoji as one grapheme" in {
    val text = "a🙂b"

    TextEditing.nextGraphemeBoundary(text, 1) shouldBe 3
    TextEditing.previousGraphemeBoundary(text, 3) shouldBe 1
  }

  it should "step over emoji skin-tone modifier sequences as one grapheme" in {
    val text = "a\uD83D\uDC4D\uD83C\uDFFDb"

    TextEditing.nextGraphemeBoundary(text, 1) shouldBe 5
    TextEditing.previousGraphemeBoundary(text, 5) shouldBe 1
  }

  it should "step over combining-mark accents with their base character" in {
    val text = "cafe\u0301!"

    TextEditing.nextGraphemeBoundary(text, 3) shouldBe 5
    TextEditing.previousGraphemeBoundary(text, 5) shouldBe 3
  }

  it should "snap split grapheme offsets outward to stable boundaries" in {
    val emoji  = "a\uD83D\uDE42b"
    val accent = "cafe\u0301!"

    TextEditing.graphemeBoundaryBeforeOrAt(emoji, 2) shouldBe 1
    TextEditing.graphemeBoundaryAfterOrAt(emoji, 2) shouldBe 3
    TextEditing.graphemeBoundaryBeforeOrAt(accent, 4) shouldBe 3
    TextEditing.graphemeBoundaryAfterOrAt(accent, 4) shouldBe 5
  }
