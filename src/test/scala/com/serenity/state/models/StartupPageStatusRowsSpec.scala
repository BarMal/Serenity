package com.serenity.state.models

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A status message on the start page keeps two cells clear on each side of the width it is given. */
class StartupPageStatusRowsSpec extends AnyFlatSpec with Matchers:

  "StartupPage.statusRows" should "return a message that fits on one row exactly as it was written" in {
    StartupPage.statusRows("Skia is unavailable", 40) shouldBe List("Skia is unavailable")
    StartupPage.statusRows("  two  spaces   kept", 40) shouldBe List("  two  spaces   kept")
  }

  it should "wrap to the width less two cells on each side" in {
    StartupPage.statusRows("aaaa bbbb cccc", 14) shouldBe List("aaaa bbbb", "cccc")
    StartupPage.statusRows("aaaa bbbb cccc", 12) shouldBe List("aaaa", "bbbb", "cccc")
  }

  it should "wrap at one cell when the width leaves nothing" in {
    StartupPage.statusRows("ab", 4) shouldBe List("a", "b")
    StartupPage.statusRows("ab", 0) shouldBe List("a", "b")
  }

  it should "break a word longer than the row" in {
    StartupPage.statusRows("abcdefghij", 8) shouldBe List("abcd", "efgh", "ij")
  }

  it should "measure wide characters in cells" in {
    StartupPage.statusRows("日本語日本語", 10) shouldBe List("日本語", "日本語")
  }
