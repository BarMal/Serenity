package com.serenity.ui.layout

import com.serenity.config.InterfaceDensity
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SpacingScaleSpec extends AnyFlatSpec with Matchers:

  private val baseline = SpacingScale.forUi(uiFontSizePx = 12.0, density = InterfaceDensity.Comfortable)

  "SpacingScale" should "resolve None to no space at all" in {
    baseline.px(SpacingStep.None) shouldBe 0
  }

  it should "order its steps strictly, so no two steps collapse onto the same pixel value" in {
    val resolved = SpacingStep.values.toList.map(baseline.px)
    resolved shouldBe resolved.sorted
    resolved.distinct.size shouldBe resolved.size
  }

  it should "never resolve a non-zero step to zero pixels, even at the smallest usable font" in {
    val tiny = SpacingScale.forUi(uiFontSizePx = 6.0, density = InterfaceDensity.Compact)
    SpacingStep.values.toList.filterNot(_ == SpacingStep.None).foreach { step =>
      withClue(s"$step: ") {
        tiny.px(step) should be >= 1
      }
    }
  }

  it should "scale with the UI font size, so chrome stays proportionate as type grows" in {
    val small = SpacingScale.forUi(uiFontSizePx = 12.0, density = InterfaceDensity.Comfortable)
    val large = SpacingScale.forUi(uiFontSizePx = 24.0, density = InterfaceDensity.Comfortable)
    large.px(SpacingStep.Md) should be > small.px(SpacingStep.Md)
  }

  it should "give a denser interface less space than a roomier one at the same font size" in {
    val compact    = SpacingScale.forUi(uiFontSizePx = 12.0, density = InterfaceDensity.Compact)
    val comfotable = SpacingScale.forUi(uiFontSizePx = 12.0, density = InterfaceDensity.Comfortable)
    val spacious   = SpacingScale.forUi(uiFontSizePx = 12.0, density = InterfaceDensity.Spacious)

    compact.px(SpacingStep.Md) should be < comfotable.px(SpacingStep.Md)
    comfotable.px(SpacingStep.Md) should be < spacious.px(SpacingStep.Md)
  }

  it should "survive a nonsensical font size rather than producing negative or infinite space" in {
    val degenerate = SpacingScale.forUi(uiFontSizePx = 0.0, density = InterfaceDensity.Comfortable)
    SpacingStep.values.toList.foreach(step => degenerate.px(step) should be >= 0)

    val nonFinite = SpacingScale.forUi(uiFontSizePx = Double.NaN, density = InterfaceDensity.Comfortable)
    SpacingStep.values.toList.foreach(step => nonFinite.px(step) should be >= 0)
  }

  "The terminal spacing scale" should "spend exactly one cell on any space at all" in {
    SpacingScale.terminal.px(SpacingStep.None) shouldBe 0
    SpacingStep.values.toList.filterNot(_ == SpacingStep.None).foreach { step =>
      withClue(s"$step: ") {
        SpacingScale.terminal.px(step) shouldBe 1
      }
    }
  }
