package com.serenity.ui.theme

import java.awt.Color

import com.serenity.richtext.{InlineMark, ParagraphRole, RichTextDocument}

object RichTextStyling:

  def styledLine(
    document: RichTextDocument,
    bufferLine: Int,
    startColumn: Int,
    endColumn: Int,
    theme: Theme
  ): List[StyledText] =
    document.paragraphs
      .lift(bufferLine)
      .map { paragraph =>
        paragraph.runs
          .foldLeft((0, List.empty[StyledText])) {
            case ((offset, acc), run) =>
              val nextOffset = offset + run.text.length
              val segment    = sliceRun(run, paragraph.role, offset, startColumn, endColumn, theme)
              (nextOffset, segment.fold(acc)(_ :: acc))
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
    endColumn: Int,
    theme: Theme
  ): Option[StyledText] =
    val runEnd = runStart + run.text.length
    if runEnd <= startColumn || runStart >= endColumn then None
    else
      val localStart = (startColumn - runStart).max(0).min(run.text.length)
      val localEnd   = (endColumn - runStart).max(localStart).min(run.text.length)
      val content    = run.text.slice(localStart, localEnd)
      Option.when(content.nonEmpty)(
        StyledText(
          content,
          textStyle(run.style, role),
          foregroundColor(run.style, theme),
          theme.background
        )
      )

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
