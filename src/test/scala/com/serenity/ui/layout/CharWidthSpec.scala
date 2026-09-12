package com.serenity.ui.layout

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

  it should "report transport and map symbols as wide" in {
    CharWidth.of(0x1f680) shouldBe 2 // 🚀, the block's first codepoint
    CharWidth.of(0x1f6a2) shouldBe 2 // 🚢
    // U+1F6FF itself is unassigned in this Unicode version (ICU4J reports it Neutral, not Wide) -- the old
    // hand-rolled range table asserted its whole 0x1f680-0x1f6ff block as wide by construction, which this test
    // originally mirrored without checking the real per-codepoint property. U+1F6FC is the last codepoint in the
    // block that is both assigned and actually Wide under UAX#11.
    CharWidth.of(0x1f6fc) shouldBe 2 // 🛼, the block's last assigned Wide codepoint
  }

  it should "report supplemental symbols extended-A as wide" in {
    CharWidth.of(0x1fa79) shouldBe 2 // 🩹
  }

  it should "report Latin-1 supplement punctuation as narrow" in {
    CharWidth.of(0x00e9) shouldBe 1 // e-acute
  }

  it should "report ornamental dingbats, which sit between two wide blocks, as narrow" in {
    CharWidth.of(0x1f650) shouldBe 1
  }

  // The following pin the ICU4J East Asian Width property (UProperty.EAST_ASIAN_WIDTH) as the source of truth,
  // closing the gaps the hand-rolled range table's doc comment named explicitly: the wide singletons scattered
  // outside the wide blocks, and Hangul jamo (which the old table already special-cased but is worth re-pinning
  // against the real property now that it drives the answer).

  it should "report the hourglass singleton (U+231A) as wide, unlike the old hand-rolled table" in {
    CharWidth.of(0x231a) shouldBe 2
  }

  it should "report the mahjong tile singleton (U+1F004) as wide, unlike the old hand-rolled table" in {
    CharWidth.of(0x1f004) shouldBe 2
  }

  it should "report the playing card singleton (U+1F0CF) as wide, unlike the old hand-rolled table" in {
    CharWidth.of(0x1f0cf) shouldBe 2
  }

  it should "report Hangul jamo choseong/jungseong as wide per EAW, matching the old table's Hangul Jamo block" in {
    CharWidth.of(0x1100) shouldBe 2 // choseong kiyeok
  }

  it should "report Latin-1 e-acute (Ambiguous under EAW) as narrow, matching terminal-default behavior" in {
    CharWidth.of(0x00e9) shouldBe 1
  }

  it should "report halfwidth forms as narrow" in {
    CharWidth.of(0xff61) shouldBe 1 // halfwidth ideographic full stop
  }
