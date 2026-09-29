package com.serenity.ui.layout

import java.awt.Font
import java.awt.font.FontRenderContext

import com.serenity.richtext.ParagraphRole

/** Pure geometry for a multi-line drop cap paragraph: which of a paragraph's visual lines the glyph spans, and the
  * left inset those lines reserve so body text wraps in beside the glyph instead of under it.
  *
  * Deliberately kept separate from [[TextCaretMeasurement.LineFontResolver.lineMetrics]] (which drives each visual
  * line's own `heightPx`/`ascentPx`): a drop cap's oversized glyph must NOT inflate its home line's height the way a
  * `Heading` run's bigger font does there (see the type's own doc) -- the glyph is drawn across the paragraph's
  * *normal*-height lines, not one abnormally tall line. This module only ever computes an extra left margin for those
  * lines; callers keep using the paragraph's ordinary (non-inflated) line metrics for layout.
  */
object DropCapLayout:

  /** Whether `lineWithinParagraph` (0-based: 0 is the paragraph's own first visual line) falls under a drop cap
    * paragraph's glyph span. `false` for every non-drop-cap role.
    */
  def spansLine(role: ParagraphRole, lineWithinParagraph: Int): Boolean =
    role match
      case ParagraphRole.DropCap(lines) => lineWithinParagraph >= 0 && lineWithinParagraph < lines.max(1)
      case _                            => false

  /** The glyph's measured advance width at `glyphFont` (already sized via
    * `RichTextStyling.dropCapGlyphStyle`/`TextStyle.styledFont`), for a single character/grapheme of `glyphText`.
    * Returns 0 for an empty glyph.
    */
  def glyphWidthPx(glyphFont: Font, frc: FontRenderContext, glyphText: String): Float =
    if glyphText.isEmpty then 0.0f
    else glyphFont.getStringBounds(glyphText, frc).getWidth.toFloat

  /** The extra left margin, in pixels, a visual line reserves for the drop cap glyph: the glyph's measured width on
    * every line it spans, zero elsewhere. Added to a line's ordinary left origin so its body text starts to the right
    * of the glyph instead of under it.
    */
  def leftInsetPx(role: ParagraphRole, lineWithinParagraph: Int, glyphWidthPx: Float): Float =
    if spansLine(role, lineWithinParagraph) then glyphWidthPx.max(0.0f) else 0.0f

  /** The glyph's drawn height, in pixels: `lines` full normal-line-heights, so it sits flush with the top of the
    * paragraph's first visual line and the foot of its last spanned line, the way a printed drop cap does. `0` for a
    * non-drop-cap role (nothing to draw).
    */
  def glyphHeightPx(role: ParagraphRole, normalLineHeightPx: Int): Int =
    role match
      case ParagraphRole.DropCap(lines) => normalLineHeightPx.max(0) * lines.max(1)
      case _                            => 0
