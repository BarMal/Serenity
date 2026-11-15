package com.serenity.ui.layout

import java.awt.Font

import com.serenity.richtext.{ParagraphRole, RichTextDocument}
import com.serenity.ui.openings.LetterShape
import com.serenity.ui.renderer.FontSpec
import com.serenity.ui.theme.{RichTextStyling, TextStyle}

/** A drop cap's glyph, fitted to the lines it spans the way a printed one is: the top of its capitals is level with the
  * top of the capitals on the first line and its baseline sits on the baseline of the last spanned line, so its font
  * size follows from the body font's metrics rather than from a multiple of its size.
  *
  * `baselinePx` and `heightPx` are measured down from the top of the first line. The height covers the room of the
  * lines the cap spans and anything the letter itself hangs below the last baseline, a `Q`'s tail or a `J`'s hook.
  *
  * A rising letter whose row grows is fitted to one more line than it spans, so its capitals stand one line above those
  * of the first line, and `growthPx` is how far its ink reaches above the top of the first line, which that line makes
  * room for. One whose row does not grow is shrunk until its ink reaches no higher than the top of the first line.
  */
final case class DropCapGlyph(
    text: String,
    charCount: Int,
    style: TextStyle,
    font: Font,
    widthPx: Float,
    inkRightPx: Int,
    baselinePx: Int,
    heightPx: Int,
    bodyFontSizePx: Float,
    growthPx: Int = 0
):

  def fontSizePx: Float = font.getSize2D

  /** The x, from the paragraph's origin, every line beside the glyph starts at: the right edge of the glyph's ink plus
    * `gapEm` body fonts. The ink is measured from the draw origin, so a letter reaching left of it is still cleared.
    */
  def indentPx(gapEm: Float): Float = inkRightPx + gapEm.max(0.0f) * bodyFontSizePx

object DropCapGlyph:

  /** The glyph of the drop cap paragraph at `lineIndex`, or none when `role` is no drop cap, the paragraph has no first
    * character to set or the owner of `memos` has no measurer to fit it with. `proseScale` is the zoom the paragraph's
    * own run sizes are drawn at.
    */
  def forParagraph(
    baseFont: Font,
    document: Option[RichTextDocument],
    lineIndex: Int,
    role: ParagraphRole,
    proseScale: Float,
    memos: LayoutMemos,
    shape: LetterShape = LetterShape.Drop
  ): Option[DropCapGlyph] =
    role match
      case ParagraphRole.DropCap(lines) =>
        for
          firstRun <- document.flatMap(_.paragraphAt(lineIndex)).flatMap(_.runs.headOption).filter(_.text.nonEmpty)
          template <- RichTextStyling.dropCapGlyphStyle(firstRun.style, role, ReferenceSizePx)
          glyph <- memos.dropCapFits(
            FitKey(
              baseFont,
              RichTextStyling.dropCapBodyStyle(firstRun.style, proseScale),
              template,
              lines.max(1),
              firstRun.text.take(Character.charCount(firstRun.text.codePointAt(0))),
              shape
            )
          )
        yield glyph
      case _ => None

  /** The size capital heights are read at: large enough that outline rounding is far below a pixel of the result. */
  private val ReferenceSizePx = 100.0f

  final private[layout] case class FitKey(
      baseFont: Font,
      bodyStyle: TextStyle,
      glyphTemplate: TextStyle,
      lines: Int,
      text: String,
      shape: LetterShape
  )

  private[layout] def measure(
    key: FitKey,
    measurer: TextMeasurer,
    extents: FontLineExtents,
    widths: DropCapWidths
  ): DropCapGlyph =
    val bodyFont   = TextStyle.styledFont(key.baseFont, key.bodyStyle)
    val template   = FontSpec.fromAwt(TextStyle.styledFont(key.baseFont, key.glyphTemplate))
    val extent     = extents.of(FontSpec.fromAwt(bodyFont))
    val baselinePx = extent.ascentPx + (key.lines - 1) * extent.heightPx
    def sizeFor(lines: Int): Float =
      RichTextStyling.dropCapGlyphFontSize(
        bodyCapHeightPx = capHeightPx(FontSpec.fromAwt(bodyFont), measurer),
        lineHeightPx = extent.heightPx.toFloat,
        glyphCapHeightEm = capHeightPx(template, measurer) / ReferenceSizePx,
        lines = lines
      )
    def styleAt(sizePx: Float): TextStyle = key.glyphTemplate.copy(fontSize = Some(sizePx))
    def fontAt(sizePx: Float): Font       = TextStyle.styledFont(key.baseFont, styleAt(sizePx))
    val sizePx = key.shape match
      case LetterShape.Drop         => sizeFor(key.lines)
      case LetterShape.Rising(true) => sizeFor(key.lines + 1)
      case LetterShape.Rising(false) =>
        shrunkToFit(
          sizeFor(key.lines + 1),
          sizeFor(1),
          baselinePx,
          size => FontSpec.fromAwt(fontAt(size)),
          key,
          measurer
        )
    val font       = fontAt(sizePx)
    val spec       = FontSpec.fromAwt(font)
    val ink        = measurer.inkBounds(spec, key.text)
    val hangPx     = math.ceil(ink.bottom.toDouble).toInt.max(0)
    val inkRightPx = math.ceil(ink.right.toDouble).toInt.max(0)
    val growthPx =
      if key.shape == LetterShape.Rising(growsRow = true) then riseAbove(baselinePx, spec, key.text, measurer) else 0
    DropCapGlyph(
      key.text,
      key.text.length,
      styleAt(sizePx),
      font,
      widths(spec, key.text),
      inkRightPx,
      baselinePx,
      DropCapLayout.glyphHeightPx(ParagraphRole.dropCap(key.lines), extent.heightPx).max(baselinePx + hangPx),
      bodyFont.getSize2D,
      growthPx
    )

  /** The largest size up to `tallestPx` at which the letter's capitals, or its ink if that goes higher, reach no higher
    * above its baseline than `roomAboveBaselinePx`, the room from the top of the first row down to the baseline; never
    * below `shortestPx`, the size at which its capitals are as tall as the body's.
    */
  private def shrunkToFit(
    tallestPx: Float,
    shortestPx: Float,
    roomAboveBaselinePx: Int,
    fontAt: Float => FontSpec,
    key: FitKey,
    measurer: TextMeasurer
  ): Float =
    val reachPx = aboveBaselinePx(fontAt(tallestPx), key.text, measurer)
    if reachPx <= roomAboveBaselinePx then tallestPx
    else (tallestPx * roomAboveBaselinePx / reachPx).max(shortestPx)

  private def aboveBaselinePx(font: FontSpec, text: String, measurer: TextMeasurer): Int =
    val inkTop = measurer.inkBounds(font, text).top
    math.max(math.ceil(capHeightPx(font, measurer).toDouble), math.ceil((-inkTop).toDouble)).toInt

  /** How far above the first line's top a letter standing on `baselinePx` reaches: its capitals, or its ink if that
    * goes higher, so every letter of a font leaves the same room.
    */
  private def riseAbove(baselinePx: Int, font: FontSpec, text: String, measurer: TextMeasurer): Int =
    (aboveBaselinePx(font, text, measurer) - baselinePx).max(0)

  /** The height of a capital `H`, read off its ink rather than the font's ascent, which is taller than any letter. A
    * font with no `H` has its ascent's usual share.
    */
  private def capHeightPx(font: FontSpec, measurer: TextMeasurer): Float =
    val outline = -measurer.inkBounds(font, "H").top
    if outline > 0.0f then outline else measurer.cellMetrics(font).ascent * FallbackCapShareOfAscent

  private val FallbackCapShareOfAscent = 0.7f
