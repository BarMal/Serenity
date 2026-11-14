package com.serenity.config

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The wrap column is where a line wraps inside the text area: the area's edge, a count of cells, or a count of the
  * prose font's `0` advances (#2570).
  */
class WrapColumnSpec extends AnyFlatSpec with Matchers:

  "Reading a wrap column" should "take edge, unlimited and off as the text area's edge, in any case" in
    List("edge", "Edge", " UNLIMITED ", "off").foreach { text =>
      withClue(s"'$text': ")(WrapColumn.parse(text) shouldBe Some(WrapColumn.Edge))
    }

  it should "take a bare number, Ncells and N cells as cells" in {
    WrapColumn.parse("80") shouldBe Some(WrapColumn.Cells(80))
    WrapColumn.parse("80cells") shouldBe Some(WrapColumn.Cells(80))
    WrapColumn.parse("80 Cells") shouldBe Some(WrapColumn.Cells(80))
  }

  it should "take Nch as a count of the prose font's advance" in {
    WrapColumn.parse("66ch") shouldBe Some(WrapColumn.Ch(66))
    WrapColumn.parse("66 CH") shouldBe Some(WrapColumn.Ch(66))
  }

  it should "keep cells in 1..400 and ch in 20..200, refusing what is outside rather than clamping it" in {
    WrapColumn.parse("1") shouldBe Some(WrapColumn.Cells(1))
    WrapColumn.parse("400") shouldBe Some(WrapColumn.Cells(400))
    WrapColumn.parse("0") shouldBe None
    WrapColumn.parse("401") shouldBe None
    WrapColumn.parse("20ch") shouldBe Some(WrapColumn.Ch(20))
    WrapColumn.parse("200ch") shouldBe Some(WrapColumn.Ch(200))
    WrapColumn.parse("19ch") shouldBe None
    WrapColumn.parse("201ch") shouldBe None
  }

  it should "refuse what is not a width: a negative, a fraction, words or units it does not have" in
    List("-1", "-5cells", "6.5", "6.5ch", "wide", "", "10px", "ch", "cells").foreach { text =>
      withClue(s"'$text': ")(WrapColumn.parse(text) shouldBe None)
    }

  "Writing a wrap column" should "spell the edge, a bare cell count and a ch count, and read back what it wrote" in {
    WrapColumn.render(WrapColumn.Edge) shouldBe "edge"
    WrapColumn.render(WrapColumn.Cells(80)) shouldBe "80"
    WrapColumn.render(WrapColumn.Ch(66)) shouldBe "66ch"
    List(WrapColumn.Edge, WrapColumn.Cells(1), WrapColumn.Cells(400), WrapColumn.Ch(20), WrapColumn.Ch(200)).foreach {
      wrapColumn => WrapColumn.parse(WrapColumn.render(wrapColumn)) shouldBe Some(wrapColumn)
    }
  }

  "The wrap column setting" should "be editor.wrap_column, default to the edge and be on the surface config" in {
    val field = ConfigRegistry.find("editor.wrap_column").getOrElse(fail("missing editor.wrap_column"))

    AppConfig.default.surfaceConfig.wrapColumn shouldBe WrapColumn.Edge
    field.setting(AppConfig.default)._2.config.unwrapped shouldBe "edge"
    field.read(AppConfig.default, "66ch").map(_.surfaceConfig.wrapColumn) shouldBe Some(WrapColumn.Ch(66))
    field.read(AppConfig.default, "80").map(field.setting(_)._2.config.unwrapped) shouldBe Some(80)
    field.read(AppConfig.default, "wide") shouldBe None
    AppConfig.default.withWrapColumn(WrapColumn.Cells(72)).surfaceConfig.wrapColumn shouldBe WrapColumn.Cells(72)
  }

  it should "no longer have the two settings it replaces as fields of their own" in {
    ConfigRegistry.writtenKeys should not contain "typography.prose.measure"
    ConfigRegistry.writtenKeys should not contain "editor.column_target_width"
  }
