package com.serenity.config

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A gutter is the space a side of the text area keeps free: a share of the width, a count of cells, a count of the
  * prose font's `0` advances, or (left and right) whatever the wrap point leaves spare.
  */
class GutterSpec extends AnyFlatSpec with Matchers:

  "Reading a gutter" should "take a bare number or N% as a percentage, as the setting always has" in {
    Gutter.parse("12.5", allowAuto = true) shouldBe Some(Gutter.Percent(0.125))
    Gutter.parse("20%", allowAuto = true) shouldBe Some(Gutter.Percent(0.20))
    Gutter.parse(" 7.5 % ", allowAuto = false) shouldBe Some(Gutter.Percent(0.075))
  }

  it should "take Ncells, N cells and Nch as counts, in any case" in {
    Gutter.parse("12cells", allowAuto = true) shouldBe Some(Gutter.Cells(12))
    Gutter.parse("12 Cells", allowAuto = true) shouldBe Some(Gutter.Cells(12))
    Gutter.parse("4ch", allowAuto = true) shouldBe Some(Gutter.Ch(4))
    Gutter.parse("4 CH", allowAuto = true) shouldBe Some(Gutter.Ch(4))
  }

  it should "take auto only where a side is allowed to share spare width" in {
    Gutter.parse("auto", allowAuto = true) shouldBe Some(Gutter.Auto)
    Gutter.parse("AUTO", allowAuto = true) shouldBe Some(Gutter.Auto)
    Gutter.parse("auto", allowAuto = false) shouldBe None
  }

  it should "refuse a count of ch on a vertical side, where there is no advance to count" in {
    Gutter.parse("4ch", allowAuto = false, allowCh = false) shouldBe None
    Gutter.parse("4cells", allowAuto = false, allowCh = false) shouldBe Some(Gutter.Cells(4))
  }

  it should "refuse what is not a gutter: a negative, a share past the cap, a fraction of a cell, or words" in
    List("-1", "-3cells", "46", "46%", "2.5cells", "1.5ch", "wide", "", "10 px", "ch", "%").foreach { text =>
      withClue(s"'$text': ")(Gutter.parse(text, allowAuto = true) shouldBe None)
    }

  it should "accept the cap itself" in {
    Gutter.parse("45", allowAuto = true) shouldBe Some(Gutter.Percent(0.45))
  }

  "Writing a gutter" should "keep a percentage a bare number, so a file that never used units does not change" in {
    Gutter.render(Gutter.Percent(0.125)) shouldBe "12.5"
    Gutter.render(Gutter.Percent(0.0)) shouldBe "0.0"
    Gutter.render(Gutter.Percent(0.173)) shouldBe "17.3"
  }

  it should "spell the other units out" in {
    Gutter.render(Gutter.Cells(12)) shouldBe "12cells"
    Gutter.render(Gutter.Ch(4)) shouldBe "4ch"
    Gutter.render(Gutter.Auto) shouldBe "auto"
  }

  it should "read back what it wrote" in
    List(Gutter.Percent(0.125), Gutter.Percent(0.0), Gutter.Cells(0), Gutter.Cells(30), Gutter.Ch(66), Gutter.Auto)
      .foreach(gutter => Gutter.parse(Gutter.render(gutter), allowAuto = true) shouldBe Some(gutter))

  "Insets" should "build from percentages for the specs and callers that only ever had those" in {
    TextAreaInsets.percent(left = 0.1, right = 0.2, top = 0.05, bottom = 0.0) shouldBe
      TextAreaInsets(Gutter.Percent(0.1), Gutter.Percent(0.2), Gutter.Percent(0.05), Gutter.Percent(0.0))
  }

  it should "default every side to no gutter" in {
    TextAreaInsets() shouldBe TextAreaInsets.percent()
  }

  it should "cap each percentage at 45% and a side pair's percentages at 80% together, leaving counts alone" in {
    val capped = TextAreaInsets.percent(left = 0.9, right = 0.9, top = 0.6, bottom = 0.1).normalized

    capped.left shouldBe Gutter.Percent(0.45 * (0.8 / 0.9))
    capped.right shouldBe Gutter.Percent(0.45 * (0.8 / 0.9))
    capped.top shouldBe Gutter.Percent(0.45)
    capped.bottom shouldBe Gutter.Percent(0.1)

    val counts = TextAreaInsets(Gutter.Cells(60), Gutter.Auto, Gutter.Cells(3), Gutter.Percent(0.9)).normalized
    counts.left shouldBe Gutter.Cells(60)
    counts.right shouldBe Gutter.Auto
    counts.top shouldBe Gutter.Cells(3)
    counts.bottom shouldBe Gutter.Percent(0.45)
  }

  it should "let the other side of a pair keep its whole percentage beside a count" in {
    val insets =
      TextAreaInsets(Gutter.Cells(10), Gutter.Percent(0.9), Gutter.Percent(0.0), Gutter.Percent(0.0)).normalized

    insets.right shouldBe Gutter.Percent(0.45)
  }
