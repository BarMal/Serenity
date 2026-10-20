package com.serenity.ui.layout

import java.awt.Font
import java.awt.font.FontRenderContext

import com.serenity.ui.renderer.FontFirstUse

/** The advance width of a drop cap glyph, measured once per `(font, render context, text)` by the owner of this
  * instance. The font is already sized via `RichTextStyling.dropCapGlyphStyle`/`TextStyle.styledFont`.
  */
final private[serenity] class DropCapWidths:

  final private case class GlyphKey(font: Font, frc: FontRenderContext, text: String)

  private val memo = MeasurementMemo[GlyphKey, Float](maxEntries = 256) { key =>
    FontFirstUse.settle(key.font, key.frc)
    key.font.getStringBounds(key.text, key.frc).getWidth.toFloat
  }

  /** How many glyph widths were measured rather than served from the memo. */
  def measurements: Long = memo.measurements

  /** The width of `glyphText` at `glyphFont`, for a single character/grapheme; 0 for an empty glyph. */
  def apply(glyphFont: Font, frc: FontRenderContext, glyphText: String): Float =
    if glyphText.isEmpty then 0.0f else memo(GlyphKey(glyphFont, frc, glyphText))
