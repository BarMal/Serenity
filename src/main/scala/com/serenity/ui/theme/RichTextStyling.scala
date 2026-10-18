package com.serenity.ui.theme

import com.serenity.richtext.{InlineAtom, InlineMark, ParagraphRole, RichTextDocument}
import com.serenity.ui.color.RenderColor
import com.serenity.ui.openings.{ChapterOpenings, ParagraphRoles}

object RichTextStyling:

  /** The prose font size at which the zoom is neutral (1x): documents render at their authored sizes. This is the
    * default `FontConfig.textFontSize`; raising the Prose Font Size setting above it zooms every non-code format up
    * proportionally. See [[proseZoom]].
    */
  val ProseZoomBaselinePx: Float = 12.0f

  val SoftBreakGlyph: String = "\u21b5"

  /** Stands in for a visible opaque object (an image, a footnote reference) that cannot be drawn in text yet. */
  val OpaqueObjectGlyph: String = "\u25a3"

  /** Stands in for a read-only block (a table, a content control) on its line. */
  val BlockGlyph: String = "\u25a6"

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
    scale: Float = 1.0f,
    headings: HeadingSettings = HeadingSettings.default,
    roles: ParagraphRoles = ParagraphRoles.enabled
  ): List[StyledText] =
    slices(document, bufferLine, startColumn, endColumn, roles).map {
      case (content, style, role) =>
        StyledText(
          content,
          scaledTextStyle(style, role, scale, headings),
          foregroundColor(style, role, theme, headings),
          theme.background
        )
    }

  /** The font-only view of a rich line, shared with the measured layout so caret advances and per-line heights are
    * computed from the very same per-run styles the draw path paints with. No theme/colour needed.
    */
  def styledFontSpans(
    document: RichTextDocument,
    bufferLine: Int,
    startColumn: Int,
    endColumn: Int,
    scale: Float = 1.0f,
    headings: HeadingSettings = HeadingSettings.default,
    roles: ParagraphRoles = ParagraphRoles.enabled
  ): List[RichSpan] =
    slices(document, bufferLine, startColumn, endColumn, roles).map {
      case (content, style, role) => RichSpan(content, scaledTextStyle(style, role, scale, headings))
    }

  private def slices(
    document: RichTextDocument,
    bufferLine: Int,
    startColumn: Int,
    endColumn: Int,
    roles: ParagraphRoles
  ): List[(String, com.serenity.richtext.RichTextStyle, ParagraphRole)] =
    val role = roles.layoutRole(document, bufferLine)
    document
      .paragraphAt(bufferLine)
      .map { paragraph =>
        paragraph.runs
          .foldLeft((0, List.empty[(String, com.serenity.richtext.RichTextStyle, ParagraphRole)])) {
            case ((offset, acc), run) =>
              val nextOffset = offset + run.text.length
              val slice      = sliceRun(run, role, offset, startColumn, endColumn)
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
      Option.when(content.nonEmpty)((visibleText(run, content), run.style, role))

  /** Until layout can break a row at a soft break, the atom is drawn as a visible return glyph in its one-character
    * slot, so the break can be seen and selected. Paint and measurement both read these spans, so they agree.
    */
  private def visibleText(run: com.serenity.richtext.RichTextRun, content: String): String =
    run.atom match
      case Some(InlineAtom.SoftBreak)       => SoftBreakGlyph
      case Some(InlineAtom.Opaque(_, true)) => OpaqueObjectGlyph
      case Some(InlineAtom.Block(_, _))     => BlockGlyph
      case _                                => content

  private def scaledTextStyle(
    style: com.serenity.richtext.RichTextStyle,
    role: ParagraphRole,
    scale: Float,
    headings: HeadingSettings
  ): TextStyle =
    val resolved = textStyle(style, role, headings)
    resolved.copy(fontSize = resolved.fontSize.map(_ * scale))

  private def textStyle(
    style: com.serenity.richtext.RichTextStyle,
    role: ParagraphRole,
    headings: HeadingSettings
  ): TextStyle =
    headingStyle(role, headings).combine(
      TextStyle(
        isBold = style.marks.contains(InlineMark.Bold),
        isItalic = style.marks.contains(InlineMark.Italic),
        isUnderlined = style.marks.contains(InlineMark.Underline),
        fontFamily = style.fontFamily,
        fontSize = style.fontSize
      )
    )

  private def headingStyle(role: ParagraphRole, headings: HeadingSettings): TextStyle =
    role match
      case ParagraphRole.Body =>
        TextStyle.normal
      case ParagraphRole.Heading(level) =>
        headings.textStyle(level)
      case ParagraphRole.DropCap(_) =>
        // The paragraph's own runs render at their ordinary (non-inflated) style -- only the singled-out first
        // character uses the large glyph size, via [[dropCapGlyphStyle]]. Mirrors how a printed drop cap's body text
        // is set at the normal size around the oversized initial.
        TextStyle.normal

  /** The font size a drop cap glyph spanning `lines` visual lines is set at, so that the top of its capitals is level
    * with the top of the body's capitals on the first line and its baseline sits on the baseline of the last: the
    * glyph's capitals are then `(lines - 1)` line heights plus the body's own capital height tall. `glyphCapHeightEm`
    * is the glyph font's capital height per unit of font size. `lines` is clamped to at least 1 (matching
    * [[ParagraphRole.dropCap]]'s own clamp, so a corrupt/hand-edited session value can't shrink or invert the glyph).
    */
  def dropCapGlyphFontSize(bodyCapHeightPx: Float, lineHeightPx: Float, glyphCapHeightEm: Float, lines: Int): Float =
    val spanPx = (lines.max(1) - 1) * lineHeightPx + bodyCapHeightPx
    spanPx / glyphCapHeightEm.max(MinCapHeightEm)

  private val MinCapHeightEm: Float = 0.01f

  /** The role rendering/layout should treat `role` as, honouring the `document.drop_caps_enabled` config toggle. When
    * the toggle is off, a [[ParagraphRole.DropCap]] degrades to [[ParagraphRole.Body]] for this purpose only -- the
    * document itself keeps the real role (`RichTextDocument.setParagraphRole` never sees this function), so turning the
    * feature back on restores the multi-line glyph with no data lost in between. Every other role passes through
    * unchanged.
    */
  def effectiveRole(role: ParagraphRole, dropCapsEnabled: Boolean): ParagraphRole =
    ChapterOpenings.degraded(role, dropCapsEnabled)

  /** The style a drop cap paragraph's own text is set in around the glyph: the ordinary body style of its first run. */
  def dropCapBodyStyle(firstCharacterStyle: com.serenity.richtext.RichTextStyle, scale: Float): TextStyle =
    scaledTextStyle(firstCharacterStyle, ParagraphRole.Body, scale, HeadingSettings.default)

  /** The style for a drop cap paragraph's singled-out first character: bold, at `glyphFontSizePx` (see
    * [[dropCapGlyphFontSize]]), with any inline styling on that character (family/marks) layered on top the same way
    * [[textStyle]] layers inline style over role style. Returns `None` for a non-drop-cap role, since there is no glyph
    * to style in that case.
    */
  def dropCapGlyphStyle(
    firstCharacterStyle: com.serenity.richtext.RichTextStyle,
    role: ParagraphRole,
    glyphFontSizePx: Float
  ): Option[TextStyle] =
    role match
      case _: ParagraphRole.DropCap =>
        Some(
          TextStyle(
            isBold = true,
            isItalic = firstCharacterStyle.marks.contains(InlineMark.Italic),
            isUnderlined = firstCharacterStyle.marks.contains(InlineMark.Underline),
            fontFamily = firstCharacterStyle.fontFamily,
            fontSize = Some(glyphFontSizePx)
          )
        )
      case _ => None

  /** One line's styled spans (as [[styledFontSpans]] produces), with the paragraph's very first character singled out
    * into its own [[RichSpan]] carrying [[dropCapGlyphStyle]] instead of the paragraph's ordinary resolved style -- the
    * split a multi-line drop cap layout needs to measure and draw that glyph separately from the body text wrapping in
    * beside it. Only applies at `startColumn == 0` on a [[ParagraphRole.DropCap]] paragraph (the drop cap's home line);
    * every other line, and every non-drop-cap paragraph, comes back unsplit as `(None, spans)`.
    */
  def dropCapSplitFontSpans(
    document: RichTextDocument,
    bufferLine: Int,
    startColumn: Int,
    endColumn: Int,
    glyphFontSizePx: Float,
    scale: Float = 1.0f,
    roles: ParagraphRoles = ParagraphRoles.enabled
  ): (Option[RichSpan], List[RichSpan]) =
    val spans = styledFontSpans(document, bufferLine, startColumn, endColumn, scale, roles = roles)
    val role  = roles.layoutRole(document, bufferLine)
    (role, startColumn, spans) match
      case (dropCap: ParagraphRole.DropCap, 0, firstSpan :: restOfFirstSpan) if firstSpan.text.nonEmpty =>
        val firstCharacterStyle = document
          .paragraphAt(bufferLine)
          .flatMap(_.runs.headOption)
          .map(_.style)
          .getOrElse(com.serenity.richtext.RichTextStyle.empty)
        val codePoint = firstSpan.text.codePointAt(0)
        val charCount = Character.charCount(codePoint)
        val glyphText = firstSpan.text.take(charCount)
        val remainder = firstSpan.text.drop(charCount)
        val glyphStyle =
          dropCapGlyphStyle(firstCharacterStyle, dropCap, glyphFontSizePx).getOrElse(firstSpan.style)
        val remainderSpan = Option.when(remainder.nonEmpty)(RichSpan(remainder, firstSpan.style))
        (Some(RichSpan(glyphText, glyphStyle)), remainderSpan.toList ++ restOfFirstSpan)
      case _ => (None, spans)

  private def foregroundColor(
    style: com.serenity.richtext.RichTextStyle,
    role: ParagraphRole,
    theme: Theme,
    headings: HeadingSettings
  ): RenderColor =
    style.color.flatMap(hexColor).getOrElse(roleColor(role, theme, headings))

  private def roleColor(role: ParagraphRole, theme: Theme, headings: HeadingSettings): RenderColor =
    role match
      case ParagraphRole.Heading(level) => headings.colourFor(level, markdown = false).resolve(theme)
      case _                            => theme.foreground

  private def hexColor(value: String): Option[RenderColor] =
    val normalized = value.stripPrefix("#")
    Option
      .when(normalized.length == 6 && normalized.forall(isHexDigit)) {
        val red   = hexByte(normalized.substring(0, 2))
        val green = hexByte(normalized.substring(2, 4))
        val blue  = hexByte(normalized.substring(4, 6))
        RenderColor.fromRgba(red, green, blue)
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
