package com.serenity

import java.awt.Color

import com.serenity.ui.theme.config.ColorParser
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #RRGGBBAA hex parsing -- needed so a theme's `background` field (a plain hex string, see `UiColors.background`) can
  * carry the alpha-0 transparency sentinel, not just the pre-existing `UiTokenConfig.alpha` surface-opacity field.
  */
class ColorParserSpec extends AnyFlatSpec with Matchers:

  "ColorParser.parseColor" should "parse a 6-digit hex color as fully opaque" in {
    ColorParser.parseColor("#112233") shouldBe Right(new Color(0x11, 0x22, 0x33, 255))
  }

  it should "parse an 8-digit hex color with an explicit alpha channel" in {
    ColorParser.parseColor("#11223344") shouldBe Right(new Color(0x11, 0x22, 0x33, 0x44))
  }

  it should "parse an 8-digit hex color with alpha 00 as fully transparent" in {
    val result = ColorParser.parseColor("#00000000")
    result shouldBe Right(new Color(0, 0, 0, 0))
    result.toOption.get.getAlpha shouldBe 0
  }

  it should "reject a hex color of an invalid length" in {
    ColorParser.parseColor("#1122") shouldBe a[Left[?, ?]]
  }

  it should "reject non-hex-digit characters in an 8-digit color" in {
    ColorParser.parseColor("#GGGGGGGG") shouldBe a[Left[?, ?]]
  }
end ColorParserSpec
