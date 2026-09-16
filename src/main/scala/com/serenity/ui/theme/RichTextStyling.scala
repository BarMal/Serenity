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

  private def headingFontSize(level: Int): Float =
    level match
      case 1 => 22.0f
      case 2 => 18.0f
      case 3 => 16.0f
      case _ => 14.0f

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
