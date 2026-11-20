package com.serenity.ui.layout

import com.serenity.ui.renderer.FontSpec

/** The advance width of a drop cap glyph, measured once per `(font, text)` by the owner of this instance. The font is
  * already sized via `RichTextStyling.dropCapGlyphStyle`/`TextStyle.styledFont`.
  */
final private[serenity] class DropCapWidths(widthOf: (FontSpec, String) => Float):

  final private case class GlyphKey(font: FontSpec, text: String)

  private val memo = MeasurementMemo[GlyphKey, Float](maxEntries = 256)(key => widthOf(key.font, key.text))

  /** How many glyph widths were measured rather than served from the memo. */
  def measurements: Long = memo.measurements

  /** The width of `glyphText` at `glyphFont`, for a single character/grapheme; 0 for an empty glyph. */
  def apply(glyphFont: FontSpec, glyphText: String): Float =
    if glyphText.isEmpty then 0.0f else memo(GlyphKey(glyphFont, glyphText))
