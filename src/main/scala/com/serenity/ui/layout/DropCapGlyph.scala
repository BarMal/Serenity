package com.serenity.ui.layout

import java.awt.Font
import java.awt.font.FontRenderContext

import com.serenity.richtext.{ParagraphRole, RichTextDocument}
import com.serenity.ui.renderer.FontFirstUse
import com.serenity.ui.theme.{RichTextStyling, TextStyle}

/** A drop cap's glyph, fitted to the lines it spans the way a printed one is: the top of its capitals is level with the
  * top of the capitals on the first line and its baseline sits on the baseline of the last spanned line, so its font
  * size follows from the body font's metrics rather than from a multiple of its size.
  *
  * `baselinePx` and `heightPx` are measured down from the top of the first line. The height covers the room of the
  * lines the cap spans and anything the letter itself hangs below the last baseline, a `Q`'s tail or a `J`'s hook.
  */
final case class DropCapGlyph(
    text: String,
    charCount: Int,
    style: TextStyle,
    font: Font,
    widthPx: Float,
    baselinePx: Int,
    heightPx: Int,
    bodyFontSizePx: Float
):

  def fontSizePx: Float = font.getSize2D

  /** The x, from the paragraph's origin, every line beside the glyph starts at: the glyph's advance plus `gapEm` body
    * fonts.
    */
  def indentPx(gapEm: Float): Float = widthPx + gapEm.max(0.0f) * bodyFontSizePx

object DropCapGlyph:

  /** The glyph of the drop cap paragraph at `lineIndex`, or none when `role` is no drop cap or the paragraph has no
    * first character to set. `proseScale` is the zoom the paragraph's own run sizes are drawn at.
    */
  def forParagraph(
    baseFont: Font,
    frc: FontRenderContext,
    document: Option[RichTextDocument],
    lineIndex: Int,
    role: ParagraphRole,
    proseScale: Float
  ): Option[DropCapGlyph] =
    role match
      case ParagraphRole.DropCap(lines) =>
        for
          firstRun <- document.flatMap(_.paragraphAt(lineIndex)).flatMap(_.runs.headOption).filter(_.text.nonEmpty)
          template <- RichTextStyling.dropCapGlyphStyle(firstRun.style, role, ReferenceSizePx)
        yield
          val charCount = Character.charCount(firstRun.text.codePointAt(0))
          fits(
            FitKey(
              baseFont,
              RichTextStyling.dropCapBodyStyle(firstRun.style, proseScale),
              template,
              frc,
              lines.max(1),
              firstRun.text.take(charCount)
            )
          )
      case _ => None

  /** The size capital heights are read at: large enough that outline rounding is far below a pixel of the result. */
  private val ReferenceSizePx = 100.0f

  final private case class FitKey(
      baseFont: Font,
      bodyStyle: TextStyle,
      glyphTemplate: TextStyle,
      frc: FontRenderContext,
      lines: Int,
      text: String
  )

  private val fits = MeasurementMemo[FitKey, DropCapGlyph](maxEntries = 256)(measure)

  private def measure(key: FitKey): DropCapGlyph =
    val bodyFont = TextStyle.styledFont(key.baseFont, key.bodyStyle)
    val template = TextStyle.styledFont(key.baseFont, key.glyphTemplate)
    FontFirstUse.settle(bodyFont, key.frc)
    FontFirstUse.settle(template, key.frc)
    val extent = FontLineExtents.of(bodyFont, key.frc)
    val sizePx = RichTextStyling.dropCapGlyphFontSize(
      bodyCapHeightPx = capHeightPx(bodyFont, key.frc),
      lineHeightPx = extent.heightPx.toFloat,
      glyphCapHeightEm = capHeightPx(template, key.frc) / ReferenceSizePx,
      lines = key.lines
    )
    val style = key.glyphTemplate.copy(fontSize = Some(sizePx))
    val font  = TextStyle.styledFont(key.baseFont, style)
    FontFirstUse.settle(font, key.frc)
    val baselinePx = extent.ascentPx + (key.lines - 1) * extent.heightPx
    val hangPx     = math.ceil(font.createGlyphVector(key.frc, key.text).getVisualBounds.getMaxY).toInt.max(0)
    DropCapGlyph(
      key.text,
      key.text.length,
      style,
      font,
      DropCapLayout.glyphWidthPx(font, key.frc, key.text),
      baselinePx,
      DropCapLayout.glyphHeightPx(ParagraphRole.dropCap(key.lines), extent.heightPx).max(baselinePx + hangPx),
      bodyFont.getSize2D
    )

  /** The height of a capital `H`, read off its outline rather than the font's ascent, which is taller than any letter.
    * A font with no `H` has its ascent's usual share.
    */
  private def capHeightPx(font: Font, frc: FontRenderContext): Float =
    val outline = -font.createGlyphVector(frc, "H").getVisualBounds.getMinY.toFloat
    if outline > 0.0f then outline else font.getLineMetrics("H", frc).getAscent * FallbackCapShareOfAscent

  private val FallbackCapShareOfAscent = 0.7f
