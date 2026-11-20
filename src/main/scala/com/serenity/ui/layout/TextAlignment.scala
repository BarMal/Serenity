package com.serenity.ui.layout

import java.awt.Font

import com.serenity.ui.renderer.FontSpec

enum TextHorizontalAlignment:
  case Left
  case Center
  case Right

enum TextVerticalAlignment:
  case Top
  case Middle
  case Bottom

final case class TextAreaPx(xPx: Float, yPx: Int, widthPx: Float, heightPx: Int)

final case class AlignedTextPlacement(
    xPx: Float,
    yPx: Int,
    widthPx: Float,
    lineHeightPx: Int,
    ascentPx: Int
)

object TextAlignment:

  /** How far `text` advances in `font`, as the owner's measurer measures what the surface draws. */
  def measureTextWidth(text: String, font: Font, measurer: TextMeasurer): Float =
    measurer.glyphWidthPx(FontSpec.fromAwt(font), text)

  def placeLine(
    text: String,
    area: TextAreaPx,
    font: Font,
    lineHeightPx: Int,
    ascentPx: Int,
    horizontal: TextHorizontalAlignment,
    vertical: TextVerticalAlignment,
    measurer: TextMeasurer
  ): AlignedTextPlacement =
    val safeLineHeight = math.max(1, lineHeightPx)
    val safeAscent     = math.max(1, ascentPx)
    val textWidth      = measureTextWidth(text, font, measurer)

    val alignedX = horizontal match
      case TextHorizontalAlignment.Left =>
        area.xPx
      case TextHorizontalAlignment.Center =>
        area.xPx + (area.widthPx - textWidth) / 2.0f
      case TextHorizontalAlignment.Right =>
        area.xPx + area.widthPx - textWidth

    val alignedY = vertical match
      case TextVerticalAlignment.Top =>
        area.yPx
      case TextVerticalAlignment.Middle =>
        area.yPx + math.round((area.heightPx - safeLineHeight).toFloat / 2.0f)
      case TextVerticalAlignment.Bottom =>
        area.yPx + area.heightPx - safeLineHeight

    AlignedTextPlacement(
      xPx = math.max(area.xPx, alignedX),
      yPx = math.max(area.yPx, alignedY),
      widthPx = textWidth,
      lineHeightPx = safeLineHeight,
      ascentPx = safeAscent
    )
