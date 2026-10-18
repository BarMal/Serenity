package com.serenity.ui.layout

import java.awt.Font
import java.awt.font.FontRenderContext

import scala.annotation.tailrec

import com.serenity.richtext.{ParagraphRole, RichTextDocument}
import com.serenity.state.models.{MeasuredRow, TextVisualLine}
import com.serenity.ui.openings.LetterShape
import com.serenity.ui.renderer.FontFirstUse
import com.serenity.ui.theme.DropCapGap

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

  /** Whether the row at `lineWithinParagraph` starting at `startColumn` is the row a drop cap's glyph is drawn from:
    * the paragraph's first, at its own first column.
    */
  def isHomeRow(role: ParagraphRole, lineWithinParagraph: Int, startColumn: Int): Boolean =
    lineWithinParagraph == 0 && homeLineRole(role, startColumn).isDefined

  /** How far in a home row's text its first character reaches: the character the glyph is drawn in place of keeps its
    * caret stops, and so its width, but is never drawn. `0` for a row with no text.
    */
  private[layout] def replacedCharacterWidthPx(row: MeasuredRow): Float =
    if row.text.isEmpty then 0.0f
    else row.xForColumn(row.startColumn + Character.charCount(row.text.codePointAt(0))).getOrElse(0.0f)

  /** A cached row read onto `bufferLine`. The drop cap indent is the only offset the row carries until alignment. The
    * home row, which does not draw its first character (the glyph is drawn in its place), gives the width of that
    * character back, so the text it does draw starts at the indent like every other spanned row; its caret stops keep
    * the character, so a caret or a click still lands on the column it is on.
    */
  def placed(
    row: MeasuredRow,
    bufferLine: Int,
    role: ParagraphRole,
    lineWithinParagraph: Int,
    dropCap: Reserve
  ): TextVisualLine =
    val indentPx = leftInsetPx(role, lineWithinParagraph, dropCap.indentPx)
    val home     = isHomeRow(role, lineWithinParagraph, row.startColumn)
    TextVisualLine(
      bufferLine,
      if home then homeRow(row, dropCap) else row,
      if home then (indentPx - replacedCharacterWidthPx(row)).max(0.0f) else indentPx,
      spaceBeforePx = 0,
      spaceAfterPx = 0
    )

  private def homeRow(row: MeasuredRow, dropCap: Reserve): MeasuredRow =
    val grown = if row.heightPx > 0 then dropCap.growthPx else 0
    row.copy(heightPx = row.heightPx + grown, ascentPx = row.ascentPx + grown, dropCapHeightPx = dropCap.heightPx)

  /** The glyph's measured advance width at `glyphFont` (already sized via
    * `RichTextStyling.dropCapGlyphStyle`/`TextStyle.styledFont`), for a single character/grapheme of `glyphText`.
    * Returns 0 for an empty glyph.
    */
  def glyphWidthPx(glyphFont: Font, frc: FontRenderContext, glyphText: String): Float =
    if glyphText.isEmpty then 0.0f
    else glyphWidths(GlyphKey(glyphFont, frc, glyphText))

  /** How many glyph widths were measured rather than served from the cache. */
  private[layout] def glyphWidthMeasurements: Long = glyphWidths.measurements

  final private case class GlyphKey(font: Font, frc: FontRenderContext, text: String)

  private val glyphWidths =
    MeasurementMemo[GlyphKey, Float](maxEntries = 256) { key =>
      FontFirstUse.settle(key.font, key.frc)
      key.font.getStringBounds(key.text, key.frc).getWidth.toFloat
    }

  /** The room a drop cap paragraph keeps beside its glyph. */
  val DefaultGapEm: Float = DropCapGap.Default

  /** What a drop cap paragraph reserves: the one x every line beside the glyph starts at, and the height from the top
    * of the paragraph's first row down to the foot of the glyph, descenders included. A letter that rises above its
    * first line makes that row `growthPx` taller, which `heightPx` then includes. Nothing for any other paragraph.
    */
  final case class Reserve(indentPx: Float, heightPx: Int, growthPx: Int = 0)

  object Reserve:
    val none: Reserve = Reserve(0.0f, 0)

  /** The room the drop cap of the paragraph at `lineIndex` reserves, with `gapEm` body fonts between the glyph and the
    * text. The glyph is fitted by [[DropCapGlyph.forParagraph]], so what this reserves is what the renderer draws.
    */
  def reserve(
    baseFont: Font,
    frc: FontRenderContext,
    richDocument: Option[RichTextDocument],
    lineIndex: Int,
    effectiveRole: ParagraphRole,
    proseScale: Float,
    gapEm: Float,
    shape: LetterShape = LetterShape.Drop
  ): Reserve =
    DropCapGlyph
      .forParagraph(baseFont, frc, richDocument, lineIndex, effectiveRole, proseScale, shape)
      .fold(Reserve.none)(glyph => Reserve(glyph.indentPx(gapEm), glyph.heightPx + glyph.growthPx, glyph.growthPx))

  /** The left margin, in pixels, every line a drop cap spans starts at: the glyph's ink edge and the gap, the same on
    * all of them, zero elsewhere. Added to a line's ordinary left origin so its body text starts to the right of the
    * glyph instead of under it.
    */
  def leftInsetPx(role: ParagraphRole, lineWithinParagraph: Int, indentPx: Float): Float =
    if spansLine(role, lineWithinParagraph) then indentPx.max(0.0f) else 0.0f

  /** The room of the drop cap's own lines, in pixels: `lines` full normal-line-heights. A glyph's reserved height is
    * never less, so the paragraph below clears the cap even when the letter has no descender. `0` for a non-drop-cap
    * role (nothing to draw).
    */
  def glyphHeightPx(role: ParagraphRole, normalLineHeightPx: Int): Int =
    role match
      case ParagraphRole.DropCap(lines) => normalLineHeightPx.max(0) * lines.max(1)
      case _                            => 0

  /** `lines` with the last row padded so the paragraph is at least as tall as its glyph: a paragraph that wraps to
    * fewer rows than the cap spans would otherwise leave the next paragraph's first rows under the glyph's opaque box.
    * Only a complete wrap from the home row can be measured, and only a measured layout has row heights to add up. The
    * glyph's height is the one its home row records, which covers a descender hanging below the last spanned line.
    */
  def reservingGlyphHeight(lines: Vector[TextVisualLine], role: ParagraphRole): Vector[TextVisualLine] =
    role match
      case _: ParagraphRole.DropCap =>
        (lines.headOption, lines.lastOption) match
          case (Some(home), Some(last)) if home.startColumn == 0 && home.heightPx > 0 =>
            val shortfallPx = home.dropCapHeightPx - lines.map(_.heightPx).sum
            if shortfallPx <= 0 then lines
            else lines.updated(lines.length - 1, last.copy(row = last.row.copy(reservedBelowPx = shortfallPx)))
          case _ => lines
      case _ => lines

  /** The rows each drop cap glyph of `snapshot` is painted over, keyed by the row it is painted from. A glyph is one
    * opaque box, so a pixel changed on any of these rows without repainting the others loses or leaves behind part of
    * it. Empty when the layout is not measured (the cell grid never spans a glyph across rows).
    */
  def glyphRowSpan(snapshot: TextLayoutSnapshot): Map[Int, Range] =
    snapshot.richTextDocument.filter(_ => snapshot.usesMeasuredLayout).fold(Map.empty[Int, Range]) { document =>
      val lines = snapshot.visualLines
      (for
        home <- lines.indices if lines(home).startColumn == 0
        _    <- homeLineRole(snapshot.roles.layoutRole(document, lines(home).bufferLine), lines(home).startColumn)
      yield home -> spanFrom(snapshot, home)).toMap
    }

  private def spanFrom(snapshot: TextLayoutSnapshot, home: Int): Range =
    val lines                   = snapshot.visualLines
    def heightOf(row: Int): Int = if lines(row).heightPx > 0 then lines(row).heightPx else snapshot.lineHeightPx
    val glyphBottomPx           = lines(home).dropCapHeightPx
    @tailrec def end(row: Int, topPx: Int): Int =
      if topPx >= glyphBottomPx || row >= lines.length || lines(row).bufferLine != lines(home).bufferLine then row
      else end(row + 1, topPx + heightOf(row))
    home until end(home, 0)

  /** `rows` with every row of each glyph span they touch. */
  def closedOverGlyphs(rows: Set[Int], snapshot: TextLayoutSnapshot): Set[Int] =
    if rows.isEmpty then rows
    else rows ++ glyphRowSpan(snapshot).valuesIterator.filter(_.exists(rows.contains)).flatten
