package com.serenity

import java.awt.Color

import com.serenity.ui.theme.ColorFormat
import com.serenity.ui.theme.ColorFormat.withAlpha
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ColorFormatSpec extends AnyFlatSpec with Matchers:

  "toHex" should "format an opaque color as uppercase #RRGGBB regardless of the withAlpha flag" in {
    val color = new Color(0x12, 0x34, 0x56)
    ColorFormat.toHex(color, withAlpha = false) shouldBe "#123456"
    ColorFormat.toHex(color, withAlpha = true) shouldBe "#123456"
  }

  it should "zero-pad single-digit channels" in {
    ColorFormat.toHex(new Color(0, 5, 255), withAlpha = false) shouldBe "#0005FF"
  }

  it should "append an uppercase alpha byte only when withAlpha is true and the color is non-opaque" in {
    val translucent = new Color(0xcc, 0x66, 0x33, 0x80)
    ColorFormat.toHex(translucent, withAlpha = true) shouldBe "#CC663380"
    ColorFormat.toHex(translucent, withAlpha = false) shouldBe "#CC6633"
  }

  it should "omit the alpha suffix for a fully-opaque color even when withAlpha is true" in {
    val opaque = new Color(0x33, 0x66, 0x99, 255)
    ColorFormat.toHex(opaque, withAlpha = true) shouldBe "#336699"
  }

  it should "round-trip through java.awt.Color parsing" in {
    val color = new Color(0x1a, 0x2b, 0x3c)
    val hex   = ColorFormat.toHex(color, withAlpha = false)
    Color.decode(hex) shouldBe color
  }

  "the withAlpha extension" should "replace only the alpha channel, keeping RGB intact" in {
    val color  = new Color(10, 20, 30, 200)
    val result = color.withAlpha(50)
    result.getRed shouldBe 10
    result.getGreen shouldBe 20
    result.getBlue shouldBe 30
    result.getAlpha shouldBe 50
  }

  it should "return the same value when the requested alpha already matches" in {
    val color = new Color(10, 20, 30, 200)
    color.withAlpha(200) shouldBe color
  }

  it should "support zeroing alpha out entirely" in {
    val color = new Color(1, 2, 3, 255)
    color.withAlpha(0).getAlpha shouldBe 0
  }
end ColorFormatSpec
