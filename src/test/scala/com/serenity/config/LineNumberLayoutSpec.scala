package com.serenity.config

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LineNumberLayoutSpec extends AnyFlatSpec with Matchers:

  "LineNumberLayout" should "default to left placement with no margins and no padding" in {
    val layout = LineNumberLayout()
    layout.side shouldBe LineNumberSide.Left
    layout.marginLeft shouldBe 0
    layout.marginRight shouldBe 0
    layout.padding shouldBe 0
  }

  it should "clamp negative margins and padding to zero" in {
    val normalized = LineNumberLayout(marginLeft = -5, marginRight = -1, padding = -3).normalized
    normalized.marginLeft shouldBe 0
    normalized.marginRight shouldBe 0
    normalized.padding shouldBe 0
  }

  it should "clamp margins and padding to the maximum cell bound" in {
    val huge       = LineNumberLayout.MaxCells + 100
    val normalized = LineNumberLayout(marginLeft = huge, marginRight = huge, padding = huge).normalized
    normalized.marginLeft shouldBe LineNumberLayout.MaxCells
    normalized.marginRight shouldBe LineNumberLayout.MaxCells
    normalized.padding shouldBe LineNumberLayout.MaxCells
  }

  it should "leave in-range values untouched when normalized" in {
    val layout = LineNumberLayout(side = LineNumberSide.Both, marginLeft = 2, marginRight = 3, padding = 1)
    layout.normalized shouldBe layout
  }

  "LineNumberSide" should "report which edges carry a counter" in {
    LineNumberSide.Left.showsLeft shouldBe true
    LineNumberSide.Left.showsRight shouldBe false
    LineNumberSide.Right.showsLeft shouldBe false
    LineNumberSide.Right.showsRight shouldBe true
    LineNumberSide.Both.showsLeft shouldBe true
    LineNumberSide.Both.showsRight shouldBe true
  }

  "SurfaceConfig" should "default to a left-placed line-number layout" in {
    SurfaceConfig().lineNumberLayout shouldBe LineNumberLayout()
  }

  it should "normalize its line-number layout" in {
    val config = SurfaceConfig(lineNumberLayout = LineNumberLayout(marginLeft = -4, padding = -2))
    config.normalized.lineNumberLayout shouldBe LineNumberLayout()
  }

  "AppConfig" should "expose and update the line-number layout, clamping on the way in" in {
    val base = AppConfig()
    base.lineNumberLayout shouldBe LineNumberLayout()

    val updated = base.withLineNumberLayout(LineNumberLayout(side = LineNumberSide.Both, marginLeft = -1, padding = 2))
    updated.lineNumberLayout shouldBe LineNumberLayout(side = LineNumberSide.Both, marginLeft = 0, padding = 2)
  }
