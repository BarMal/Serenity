package com.serenity.ui.theme

import java.awt.Color

import com.serenity.richtext.{InlineMark, ParagraphRole, RichTextDocument}

object RichTextStyling:

  /** The prose font size at which the zoom is neutral (1x): documents render at their authored sizes. This is the
    * default `FontConfig.textFontSize`; raising the Prose Font Size setting above it zooms every non-code format up
    * proportionally. See [[proseZoom]].
    */
  val ProseZoomBaselinePx: Float = 12.0f

  /** The zoom factor a base prose font size implies, anchored so the default (12pt) is 1x. */
  def proseZoom(baseProseFontSizePx: Float): Float =
    if baseProseFontSizePx > 0.0f then baseProseFontSizePx / ProseZoomBaselinePx else 1.0f

  /** One resolved rich-text span: the text and the concrete style it is drawn/measured with (font size already
    * multiplied by the prose zoom). Colour is added only by [[styledLine]] for the draw path.
    */
  final case class RichSpan(text: String, style: TextStyle)

  def styledLine(
    document: RichTextDocument,
    bufferLine: Int,
    startColumn: Int,
    endColumn: Int,
    theme: Theme,
    scale: Float = 1.0f
  ): List[StyledText] =
    slices(document, bufferLine, startColumn, endColumn).map {
      case (content, style, role) =>
        StyledText(content, scaledTextStyle(style, role, scale), foregroundColor(style, theme), theme.background)
    }

  /** The font-only view of a rich line, shared with the measured layout so caret advances and per-line heights are
    * computed from the very same per-run styles the draw path paints with. No theme/colour needed.
    */
  def styledFontSpans(
    document: RichTextDocument,
    bufferLine: Int,
    startColumn: Int,
    endColumn: Int,
    scale: Float = 1.0f
  ): List[RichSpan] =
    slices(document, bufferLine, startColumn, endColumn).map {
      case (content, style, role) => RichSpan(content, scaledTextStyle(style, role, scale))
    }

  private def slices(
    document: RichTextDocument,
    bufferLine: Int,
    startColumn: Int,
    endColumn: Int
  ): List[(String, com.serenity.richtext.RichTextStyle, ParagraphRole)] =
    document
      .paragraphAt(bufferLine)
      .map { paragraph =>
        paragraph.runs
          .foldLeft((0, List.empty[(String, com.serenity.richtext.RichTextStyle, ParagraphRole)])) {
            case ((offset, acc), run) =>
              val nextOffset = offset + run.text.length
              val slice      = sliceRun(run, paragraph.role, offset, startColumn, endColumn)
              (nextOffset, slice.fold(acc)(_ :: acc))
          }
          ._2
          .reverse
      }
      .getOrElse(Nil)

  private def sliceRun(
    run: com.serenity.richtext.RichTextRun,
    role: ParagraphRole,
    runStart: Int,
    startColumn: Int,
    endColumn: Int
  ): Option[(String, com.serenity.richtext.RichTextStyle, ParagraphRole)] =
    val runEnd = runStart + run.text.length
    if runEnd <= startColumn || runStart >= endColumn then None
    else
      val localStart = (startColumn - runStart).max(0).min(run.text.length)
      val localEnd   = (endColumn - runStart).max(localStart).min(run.text.length)
      val content    = run.text.slice(localStart, localEnd)
      Option.when(content.nonEmpty)((content, run.style, role))

  private def scaledTextStyle(
    style: com.serenity.richtext.RichTextStyle,
    role: ParagraphRole,
    scale: Float
  ): TextStyle =
    val resolved = textStyle(style, role)
    resolved.copy(fontSize = resolved.fontSize.map(_ * scale))

  private def textStyle(style: com.serenity.richtext.RichTextStyle, role: ParagraphRole): TextStyle =
    headingStyle(role).combine(
      TextStyle(
        isBold = style.marks.contains(InlineMark.Bold),
        isItalic = style.marks.contains(InlineMark.Italic),
        isUnderlined = style.marks.contains(InlineMark.Underline),
        fontFamily = style.fontFamily,
        fontSize = style.fontSize
      )
    )

  private def headingStyle(role: ParagraphRole): TextStyle =
    role match
      case ParagraphRole.Body =>
        TextStyle.normal
      case ParagraphRole.Heading(level) =>
        TextStyle(
          isBold = true,
          fontSize = Some(headingFontSize(level))
        )
      case ParagraphRole.DropCap(_) =>
        // The paragraph's own runs render at their ordinary (non-inflated) style -- only the singled-out first
        // character uses the large glyph size, via [[dropCapGlyphStyle]]. Mirrors how a printed drop cap's body text
        // is set at the normal size around the oversized initial.
        TextStyle.normal

  private def headingFontSize(level: Int): Float =
    level match
      case 1 => 22.0f
      case 2 => 18.0f
      case 3 => 16.0f
      case _ => 14.0f

  /** The font size, in the same units as [[ProseZoomBaselinePx]], a drop cap glyph spanning `lines` visual lines
    * should render at -- so it visually occupies about that many lines of the paragraph's normal line height, the way
    * a printed drop cap sits flush with the top and bottom of the lines it spans. `baseFontSizePx` is the paragraph's
    * own (unscaled) body font size; `lines` is clamped to at least 1 (matching [[ParagraphRole.dropCap]]'s own
    * clamp, so a corrupt/hand-edited session value can't shrink or invert the glyph).
    */
  def dropCapGlyphFontSize(baseFontSizePx: Float, lines: Int): Float =
    baseFontSizePx.max(1.0f) * lines.max(1).toFloat

  /** The role rendering/layout should treat `role` as, honouring the `document.drop_caps_enabled` config toggle. When
    * the toggle is off, a [[ParagraphRole.DropCap]] degrades to [[ParagraphRole.Body]] for this purpose only -- the
    * document itself keeps the real role (`RichTextDocument.setParagraphRole` never sees this function), so turning
    * the feature back on restores the multi-line glyph with no data lost in between. Every other role passes through
    * unchanged.
    */
  def effectiveRole(role: ParagraphRole, dropCapsEnabled: Boolean): ParagraphRole =
    role match
      case ParagraphRole.DropCap(_) if !dropCapsEnabled => ParagraphRole.Body
      case other                                         => other

  /** The resolved, zoom-scaled style for a drop cap paragraph's singled-out first character: bold, sized to span
    * `role.lines` visual lines at the paragraph's base body font size, with any inline styling on that character
    * (family/colour/explicit marks) layered on top the same way [[textStyle]] layers inline style over role style.
    * Returns `None` for a non-drop-cap role, since there is no glyph to size in that case.
    */
  def dropCapGlyphStyle(
    firstCharacterStyle: com.serenity.richtext.RichTextStyle,
    role: ParagraphRole,
    baseFontSizePx: Float,
    scale: Float = 1.0f
  ): Option[TextStyle] =
    role match
      case ParagraphRole.DropCap(lines) =>
        val glyphSize = dropCapGlyphFontSize(baseFontSizePx, lines) * scale
        Some(
          TextStyle(
            isBold = true,
            isItalic = firstCharacterStyle.marks.contains(InlineMark.Italic),
            isUnderlined = firstCharacterStyle.marks.contains(InlineMark.Underline),
            fontFamily = firstCharacterStyle.fontFamily,
            fontSize = Some(glyphSize)
          )
        )
      case _ => None

  /** One line's styled spans (as [[styledFontSpans]] produces), with the paragraph's very first character singled
    * out into its own [[RichSpan]] carrying [[dropCapGlyphStyle]] instead of the paragraph's ordinary resolved style
    * -- the split a multi-line drop cap layout needs to measure and draw that glyph separately from the body text
    * wrapping in beside it. Only applies at `startColumn == 0` on a [[ParagraphRole.DropCap]] paragraph (the drop
    * cap's home line); every other line, and every non-drop-cap paragraph, comes back unsplit as `(None, spans)`.
    */
  def dropCapSplitFontSpans(
    document: RichTextDocument,
    bufferLine: Int,
    startColumn: Int,
    endColumn: Int,
    baseFontSizePx: Float,
    scale: Float = 1.0f
  ): (Option[RichSpan], List[RichSpan]) =
    val spans = styledFontSpans(document, bufferLine, startColumn, endColumn, scale)
    val role  = document.paragraphAt(bufferLine).map(_.role)
    (role, startColumn, spans) match
      case (Some(dropCap: ParagraphRole.DropCap), 0, firstSpan :: restOfFirstSpan) if firstSpan.text.nonEmpty =>
        val firstCharacterStyle = document
          .paragraphAt(bufferLine)
          .flatMap(_.runs.headOption)
          .map(_.style)
          .getOrElse(com.serenity.richtext.RichTextStyle.empty)
        val codePoint  = firstSpan.text.codePointAt(0)
        val charCount  = Character.charCount(codePoint)
        val glyphText  = firstSpan.text.take(charCount)
        val remainder  = firstSpan.text.drop(charCount)
        val glyphStyle = dropCapGlyphStyle(firstCharacterStyle, dropCap, baseFontSizePx, scale).getOrElse(firstSpan.style)
        val remainderSpan = Option.when(remainder.nonEmpty)(RichSpan(remainder, firstSpan.style))
        (Some(RichSpan(glyphText, glyphStyle)), remainderSpan.toList ++ restOfFirstSpan)
      case _ => (None, spans)

  private def foregroundColor(style: com.serenity.richtext.RichTextStyle, theme: Theme): Color =
    style.color.flatMap(hexColor).getOrElse(theme.foreground)

  private def hexColor(value: String): Option[Color] =
    val normalized = value.stripPrefix("#")
    Option
      .when(normalized.length == 6 && normalized.forall(isHexDigit)) {
        val red   = hexByte(normalized.substring(0, 2))
        val green = hexByte(normalized.substring(2, 4))
        val blue  = hexByte(normalized.substring(4, 6))
        Color(red, green, blue)
      }

  private def isHexDigit(char: Char): Boolean =
    char.isDigit ||
      (char >= 'a' && char <= 'f') ||
      (char >= 'A' && char <= 'F')

  private def hexByte(value: String): Int =
    value.foldLeft(0)((total, char) => total * 16 + hexValue(char))

  private def hexValue(char: Char): Int =
    if char.isDigit then char - '0'
    else if char >= 'a' && char <= 'f' then char - 'a' + 10
    else char - 'A' + 10
