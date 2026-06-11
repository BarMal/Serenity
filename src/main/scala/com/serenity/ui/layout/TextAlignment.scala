package com.serenity.ui.layout

import java.awt.Font
import java.awt.font.FontRenderContext

enum TextHorizontalAlignment:
  case Left
  case Center
  case Right

enum TextVerticalAlignment:
  case Top
  case Middle
  case Bottom

case class TextAreaPx(xPx: Float, yPx: Int, widthPx: Float, heightPx: Int)

case class AlignedTextPlacement(
    xPx: Float,
    yPx: Int,
    widthPx: Float,
    lineHeightPx: Int,
    ascentPx: Int
)

object TextAlignment:

  def measureTextWidth(
    text: String,
    font: Font,
    fontRenderContext: FontRenderContext = TextLayoutSnapshot.defaultFontRenderContext()
  ): Float =
    font.getStringBounds(text, fontRenderContext).getWidth.toFloat

  def placeLine(
    text: String,
    area: TextAreaPx,
    font: Font,
    lineHeightPx: Int,
    ascentPx: Int,
    horizontal: TextHorizontalAlignment,
    vertical: TextVerticalAlignment,
    fontRenderContext: FontRenderContext = TextLayoutSnapshot.defaultFontRenderContext()
  ): AlignedTextPlacement =
    val safeLineHeight = math.max(1, lineHeightPx)
    val safeAscent     = math.max(1, ascentPx)
    val textWidth      = measureTextWidth(text, font, fontRenderContext)

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
