package com.serenity

import java.awt.Color

import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Reference values below are the worked examples from the WCAG 2.x relative luminance definition (Rec.709-weighted,
  * sRGB gamma-linearized): https://www.w3.org/TR/WCAG21/#dfn-relative-luminance
  */
class ThemeLuminanceSpec extends AnyFlatSpec with Matchers:

  "luminance" should "be 0 for black and 1 for white" in {
    Theme.luminance(Color.BLACK) shouldBe 0.0 +- 1e-9
    Theme.luminance(Color.WHITE) shouldBe 1.0 +- 1e-9
  }

  it should "equal each Rec.709 coefficient for the pure primary of that channel" in {
    Theme.luminance(new Color(255, 0, 0)) shouldBe 0.2126 +- 1e-4
    Theme.luminance(new Color(0, 255, 0)) shouldBe 0.7152 +- 1e-4
    Theme.luminance(new Color(0, 0, 255)) shouldBe 0.0722 +- 1e-4
  }

  it should "gamma-linearize mid-gray to roughly 0.216, not the naive 0.5 average" in {
    Theme.luminance(new Color(128, 128, 128)) shouldBe 0.2159 +- 1e-3
  }

  "EqualContrastLuminanceThreshold" should "be the luminance whose WCAG contrast ratio to black equals its ratio to white" in {
    val threshold       = Theme.EqualContrastLuminanceThreshold
    val contrastToBlack = (threshold + 0.05) / 0.05
    val contrastToWhite = 1.05 / (threshold + 0.05)
    contrastToBlack shouldBe contrastToWhite +- 1e-9
    threshold shouldBe 0.1791 +- 1e-4
  }
end ThemeLuminanceSpec
