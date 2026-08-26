package com.serenity.ui.tui

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CharWidthSpec extends AnyFlatSpec with Matchers:

  "CharWidth.of" should "report ASCII as narrow" in {
    CharWidth.of('a'.toInt) shouldBe 1
    CharWidth.of(' '.toInt) shouldBe 1
  }

  it should "report CJK unified ideographs as wide" in {
    CharWidth.of(0x4e2d) shouldBe 2 // 中
  }

  it should "report Hangul syllables as wide" in {
    CharWidth.of(0xac00) shouldBe 2 // 가
  }

  it should "report fullwidth forms as wide" in {
    CharWidth.of(0xff21) shouldBe 2 // fullwidth 'A'
  }

  it should "report common emoji as wide" in {
    CharWidth.of(0x1f600) shouldBe 2 // grinning face
  }

  it should "report Latin-1 supplement punctuation as narrow" in {
    CharWidth.of(0x00e9) shouldBe 1 // e-acute
  }
