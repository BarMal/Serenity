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
