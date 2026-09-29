package com.serenity.ui.layout

import java.awt.Font
import java.awt.font.FontRenderContext

import com.serenity.richtext.{ParagraphRole, RichTextDocument}
import com.serenity.state.models.TextVisualLine
import com.serenity.ui.theme.{RichTextStyling, TextStyle}

/** Pure geometry for a multi-line drop cap paragraph: which of a paragraph's visual lines the glyph spans, and the left
  * inset those lines reserve so body text wraps in beside the glyph instead of under it.
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

  /** The drop cap role a visual line's paragraph should paint a glyph for -- only at the paragraph's home line
    * (`startColumn == 0`, the same condition `RichTextStyling.dropCapSplitFontSpans` singles the glyph character out
    * on), so a caller never re-derives that "is this the glyph's line" check on its own. `None` for every other line
    * (including a non-home wrapped continuation of a drop cap paragraph) or a non-drop-cap role.
    */
  def homeLineRole(role: ParagraphRole, startColumn: Int): Option[ParagraphRole.DropCap] =
    role match
      case dropCap: ParagraphRole.DropCap if startColumn == 0 => Some(dropCap)
      case _                                                  => None

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

  /** The drop cap glyph's measured pixel width for the paragraph at `lineIndex`, sized exactly as
    * `RichTextStyling.dropCapGlyphStyle` sizes the glyph the renderer paints -- so the space this reserves always
    * matches the glyph actually drawn. `0` for a non-drop-cap `effectiveRole` (already degraded from the config toggle
    * by the caller) or a paragraph with no runs to take a first character from.
    */
  def measuredGlyphWidthPx(
    baseFont: Font,
    frc: FontRenderContext,
    richDocument: Option[RichTextDocument],
    lineIndex: Int,
    effectiveRole: ParagraphRole,
    proseScale: Float
  ): Float =
    effectiveRole match
      case dropCap: ParagraphRole.DropCap =>
        richDocument
          .flatMap(_.paragraphAt(lineIndex))
          .flatMap(_.runs.headOption)
          .filter(_.text.nonEmpty)
          .flatMap { firstRun =>
            val charCount = Character.charCount(firstRun.text.codePointAt(0))
            val glyphText = firstRun.text.take(charCount)
            RichTextStyling
              .dropCapGlyphStyle(firstRun.style, dropCap, RichTextStyling.ProseZoomBaselinePx, proseScale)
              .map(style => glyphWidthPx(TextStyle.styledFont(baseFont, style), frc, glyphText))
          }
          .getOrElse(0.0f)
      case _ => 0.0f

  /** Shifts a visual line's caret geometry right by the drop cap glyph's left margin, for a line within a drop cap
    * paragraph's glyph span (`leftInsetPx`, `0` -- a no-op -- everywhere else). Mirrors `applyAlignment`'s own
    * caret-stop shift so both compose the same way; `heightPx`/`ascentPx` are untouched -- this only ever moves text
    * sideways, never the line-height math `resolver.lineMetrics` already keeps drop-cap-safe by measuring
    * `RichTextStyling.styledFontSpans` (the paragraph's ordinary body style), not the oversized glyph style
    * `RichTextStyling.dropCapGlyphStyle`/`dropCapSplitFontSpans` only the renderer's split-out glyph span uses.
    */
  def applyInset(
    line: TextVisualLine,
    role: ParagraphRole,
    lineWithinParagraph: Int,
    glyphWidthPx: Float
  ): TextVisualLine =
    val insetPx = leftInsetPx(role, lineWithinParagraph, glyphWidthPx)
    if insetPx <= 0.0f then line
    else
      line.copy(
        caretStops = line.caretStops.map(stop => stop.copy(xPx = stop.xPx + insetPx)),
        xSortedCaretStops = line.xSortedCaretStops.map(stop => stop.copy(xPx = stop.xPx + insetPx)),
        xOffsetPx = line.xOffsetPx + insetPx
      )
