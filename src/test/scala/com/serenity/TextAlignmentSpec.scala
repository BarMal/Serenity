package com.serenity

import java.awt.Font

import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TextAlignmentSpec extends AnyFlatSpec with Matchers:

  private val font       = Font(Font.SANS_SERIF, Font.PLAIN, 12)
  private val frc        = TextLayoutSnapshot.defaultFontRenderContext()
  private val lineHeight = 16
  private val ascent     = 12

  "TextAlignment" should "place text at the left edge of a text area" in {
    val area = TextAreaPx(xPx = 20.0f, yPx = 30, widthPx = 200.0f, heightPx = 80)

    val placement = TextAlignment.placeLine(
      text = "Aligned",
      area = area,
      font = font,
      lineHeightPx = lineHeight,
      ascentPx = ascent,
      horizontal = TextHorizontalAlignment.Left,
      vertical = TextVerticalAlignment.Top,
      fontRenderContext = frc
    )

    placement.xPx shouldBe area.xPx +- 0.001f
    placement.yPx shouldBe area.yPx
    placement.lineHeightPx shouldBe lineHeight
    placement.ascentPx shouldBe ascent
  }

  it should "place text at the centered x position of a text area" in {
    val text = "Wide text"
    val area = TextAreaPx(xPx = 10.0f, yPx = 4, widthPx = 240.0f, heightPx = 48)

    val placement = TextAlignment.placeLine(
      text = text,
      area = area,
      font = font,
      lineHeightPx = lineHeight,
      ascentPx = ascent,
      horizontal = TextHorizontalAlignment.Center,
      vertical = TextVerticalAlignment.Top,
      fontRenderContext = frc
    )

    val textWidth = TextAlignment.measureTextWidth(text, font, frc)
    placement.xPx shouldBe (area.xPx + (area.widthPx - textWidth) / 2.0f) +- 0.001f
  }

  it should "place text at the right edge of a text area" in {
    val text = "Right"
    val area = TextAreaPx(xPx = 7.0f, yPx = 3, widthPx = 180.0f, heightPx = 48)

    val placement = TextAlignment.placeLine(
      text = text,
      area = area,
      font = font,
      lineHeightPx = lineHeight,
      ascentPx = ascent,
      horizontal = TextHorizontalAlignment.Right,
      vertical = TextVerticalAlignment.Top,
      fontRenderContext = frc
    )

    val textWidth = TextAlignment.measureTextWidth(text, font, frc)
    placement.xPx shouldBe (area.xPx + area.widthPx - textWidth) +- 0.001f
  }

  it should "place text vertically at top, middle, and bottom positions" in {
    val area = TextAreaPx(xPx = 0.0f, yPx = 10, widthPx = 200.0f, heightPx = 70)

    val top = TextAlignment.placeLine(
      "Text",
      area,
      font,
      lineHeight,
      ascent,
      TextHorizontalAlignment.Left,
      TextVerticalAlignment.Top,
      frc
    )
    val middle = TextAlignment.placeLine(
      "Text",
      area,
      font,
      lineHeight,
      ascent,
      TextHorizontalAlignment.Left,
      TextVerticalAlignment.Middle,
      frc
    )
    val bottom = TextAlignment.placeLine(
      "Text",
      area,
      font,
      lineHeight,
      ascent,
      TextHorizontalAlignment.Left,
      TextVerticalAlignment.Bottom,
      frc
    )

    top.yPx shouldBe 10
    middle.yPx shouldBe 37
    bottom.yPx shouldBe 64
  }

  it should "clamp oversized text to the text area origin" in {
    val area = TextAreaPx(xPx = 12.0f, yPx = 9, widthPx = 1.0f, heightPx = 1)

    val placement = TextAlignment.placeLine(
      text = "A very long line",
      area = area,
      font = font,
      lineHeightPx = lineHeight,
      ascentPx = ascent,
      horizontal = TextHorizontalAlignment.Right,
      vertical = TextVerticalAlignment.Bottom,
      fontRenderContext = frc
    )

    placement.xPx shouldBe area.xPx +- 0.001f
    placement.yPx shouldBe area.yPx
  }
