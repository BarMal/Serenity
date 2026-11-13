package com.serenity.ui.layout

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DisplayColumnSpec extends AnyFlatSpec with Matchers:

  private val flag   = "🇺🇸"
  private val family = "👨‍👩‍👧"

  "DisplayColumn.of" should "be the column itself on plain ASCII" in {
    DisplayColumn.of("hello", 3, 4) shouldBe 3
  }

  it should "be zero on an empty line" in {
    DisplayColumn.of("", 0, 4) shouldBe 0
  }

  it should "count an emoji cluster at its two cells, however many UTF-16 units it takes" in {
    DisplayColumn.of(flag + "x", 4, 4) shouldBe 2
    DisplayColumn.of(family + "x", 8, 4) shouldBe 2
    DisplayColumn.of("❤️x", 2, 4) shouldBe 2
  }

  it should "count a base and its combining mark as one cell" in {
    DisplayColumn.of("éx", 2, 4) shouldBe 1
  }

  it should "count a wide character as two cells" in {
    DisplayColumn.of("你好", 1, 4) shouldBe 2
  }

  it should "take a tab to the next tab stop, from wherever it stands" in {
    DisplayColumn.of("\tx", 1, 4) shouldBe 4
    DisplayColumn.of("ab\tc", 3, 4) shouldBe 4
    DisplayColumn.of("abcd\tc", 5, 4) shouldBe 8
  }

  it should "stop a column inside a cluster before the cluster" in {
    DisplayColumn.of("a" + flag, 3, 4) shouldBe 1
    DisplayColumn.of("a" + family, 5, 4) shouldBe 1
  }

  it should "count a column past the end as the whole line and a negative one as zero" in {
    DisplayColumn.of("a你", 50, 4) shouldBe 3
    DisplayColumn.of("abc", -2, 4) shouldBe 0
  }

  it should "give a tab size under one the width of one cell a tab" in {
    DisplayColumn.of("\tx", 2, 0) shouldBe 2
  }
