package com.serenity.config

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LineNumberLayoutSpec extends AnyFlatSpec with Matchers:

  "LineNumberLayout" should "default to left placement with margin/padding unset" in {
    val layout = LineNumberLayout()
    layout.side shouldBe LineNumberSide.Left
    layout.marginLeft shouldBe None
    layout.marginRight shouldBe 0
    layout.padding shouldBe None
  }

  it should "clamp negative margins and padding to zero" in {
    val normalized = LineNumberLayout(marginLeft = Some(-5), marginRight = -1, padding = Some(-3)).normalized
    normalized.marginLeft shouldBe Some(0)
    normalized.marginRight shouldBe 0
    normalized.padding shouldBe Some(0)
  }

  it should "clamp margins and padding to the maximum cell bound" in {
    val huge       = LineNumberLayout.MaxCells + 100
    val normalized = LineNumberLayout(marginLeft = Some(huge), marginRight = huge, padding = Some(huge)).normalized
    normalized.marginLeft shouldBe Some(LineNumberLayout.MaxCells)
    normalized.marginRight shouldBe LineNumberLayout.MaxCells
    normalized.padding shouldBe Some(LineNumberLayout.MaxCells)
  }

  it should "leave in-range values untouched when normalized" in {
    val layout = LineNumberLayout(side = LineNumberSide.Both, marginLeft = Some(2), marginRight = 3, padding = Some(1))
    layout.normalized shouldBe layout
  }

  it should "leave an unset margin/padding unset when normalized" in {
    val layout = LineNumberLayout(side = LineNumberSide.Both)
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
    val config = SurfaceConfig(lineNumberLayout = LineNumberLayout(marginLeft = Some(-4), padding = Some(-2)))
    config.normalized.lineNumberLayout shouldBe LineNumberLayout(marginLeft = Some(0), padding = Some(0))
  }

  "AppConfig" should "expose and update the line-number layout, clamping on the way in" in {
    val base = AppConfig()
    base.lineNumberLayout shouldBe LineNumberLayout()

    val updated =
      base.withLineNumberLayout(LineNumberLayout(side = LineNumberSide.Both, marginLeft = Some(-1), padding = Some(2)))
    updated.lineNumberLayout shouldBe LineNumberLayout(
      side = LineNumberSide.Both,
      marginLeft = Some(0),
      padding = Some(2)
    )
  }
