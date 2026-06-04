package com.serenity.ui.theme

import com.serenity.lsp.config.LanguageId
import com.serenity.rope.Rope

case class StyledSegment(
    content: String,
    element: SyntaxElement,
    startOffset: Int,
    endOffset: Int
)

object ThemeManager:

  /** Apply theme styling to rope content, returning styled segments */
  def applyTheme(rope: Rope, theme: Theme): List[StyledSegment] =
    val content = rope.collect()
    if content.isEmpty then List.empty
    else parseAndStyle(content)

  /** Simple syntax parsing - can be enhanced with more sophisticated parsers */
  private def parseAndStyle(content: String): List[StyledSegment] =
    val tokens = tokenize(content)
    tokens.map {
      case (text, offset) =>
        val element = SyntaxElement.fromText(text)
        StyledSegment(text, element, offset, offset + text.length)
    }

  /** Basic tokenizer - splits content into meaningful tokens */
  private def tokenize(content: String): List[(String, Int)] =
    def tokenizeRecursive(remaining: String, offset: Int, acc: List[(String, Int)]): List[(String, Int)] =
      if remaining.isEmpty then acc.reverse
      else
        val (token, rest) = extractNextToken(remaining)
        tokenizeRecursive(rest, offset + token.length, (token, offset) :: acc)

    tokenizeRecursive(content, 0, List.empty)

  /** Extract the next meaningful token from the string */
  private def extractNextToken(content: String): (String, String) =
    if content.isEmpty then ("", "")
    else
      content.head match
        case ' ' | '\t' | '\n' | '\r' =>
          // Whitespace token
          val whitespace = content.takeWhile(c => c == ' ' || c == '\t' || c == '\n' || c == '\r')
          (whitespace, content.drop(whitespace.length))

        case '"' =>
          // String literal (simplified - doesn't handle escapes)
          val closing = content.indexOf('"', 1)
          if closing == -1 then (content, "")
          else
            val str = content.substring(0, closing + 1)
            (str, content.drop(str.length))

        case '\'' =>
          // Character literal
          val closing = content.indexOf('\'', 1)
          if closing == -1 then (content, "")
          else
            val char = content.substring(0, closing + 1)
            (char, content.drop(char.length))

        case '/' if content.startsWith("//") =>
          // Line comment
          val newlineIndex = content.indexOf('\n')
          if newlineIndex == -1 then (content, "")
          else
            val comment = content.substring(0, newlineIndex)
            (comment, content.drop(comment.length))

        case '/' if content.startsWith("/*") =>
          // Block comment
          val endIndex = content.indexOf("*/")
          if endIndex == -1 then (content, "")
          else
            val comment = content.substring(0, endIndex + 2)
            (comment, content.drop(comment.length))

        case c if c.isLetter || c == '_' =>
          // Identifier or keyword
          val identifier = content.takeWhile(c => c.isLetterOrDigit || c == '_')
          (identifier, content.drop(identifier.length))

        case c if c.isDigit =>
          // Number
          val number = content.takeWhile(c => c.isDigit || c == '.' || c == 'f' || c == 'F' || c == 'd' || c == 'D')
          (number, content.drop(number.length))

        case c if "(){}[],;:.?".contains(c) =>
          // Single character delimiters
          (c.toString, content.tail)

        case c if "+-*/%=<>!&|^~".contains(c) =>
          // Operators (might be multi-character)
          val operator = content.takeWhile("+-*/%=<>!&|^~".contains(_))
          (operator, content.drop(operator.length))

        case _ =>
          // Unknown character - take as single character
          (content.head.toString, content.tail)

  /** Apply syntax highlighting to a line of text */
  def highlightLine(line: String, theme: Theme, language: Option[LanguageId] = None): List[StyledText] =
    language match
      case Some(LanguageId.Markdown) => highlightMarkdownLine(line, theme)
      case _ =>
        val segments = parseAndStyle(line)
        segments.map { segment =>
          val themeColor = theme.colorFor(segment.element)
          StyledText(
            segment.content,
            themeColor.style,
            themeColor.foreground,
            themeColor.background
          )
        }

  private def highlightMarkdownLine(line: String, theme: Theme): List[StyledText] =
    val headingPattern        = raw"^(#{1,6}\s+)(.*)$$".r
    val unorderedListPattern  = raw"^(\s*[-*+]\s+)(.*)$$".r
    val orderedListPattern    = raw"^(\s*\d+\.\s+)(.*)$$".r
    val blockQuotePattern     = raw"^(\s*>\s?)(.*)$$".r
    val inlineCodePattern     = raw"`[^`]+`".r
    val linkPattern           = raw"\[([^\]]+)\]\(([^)]+)\)".r
    val markerColor           = theme.colorFor(SyntaxElement.Delimiter)
    val headingColor          = theme.colorFor(SyntaxElement.Keyword)
    val inlineCodeColor       = theme.colorFor(SyntaxElement.String)
    val linkTextColor         = theme.colorFor(SyntaxElement.Keyword)
    val linkUrlColor          = theme.colorFor(SyntaxElement.String)

    def withInlineMarkdownStyling(
      text: String,
      baseStyle: TextStyle = TextStyle.normal,
      defaultForeground: java.awt.Color = theme.foreground
    ): List[StyledText] =
      enum InlineTokenKind:
        case InlineCode, Link

      val segments = scala.collection.mutable.ListBuffer.empty[StyledText]
      var cursor   = 0

      def appendPlain(until: Int): Unit =
        if until > cursor then
          segments += StyledText(text.substring(cursor, until), baseStyle, defaultForeground, theme.background)

      while cursor < text.length do
        val codeMatch = inlineCodePattern.findFirstMatchIn(text.substring(cursor)).map { m =>
          (cursor + m.start, cursor + m.end, InlineTokenKind.InlineCode, m)
        }
        val linkMatch = linkPattern.findFirstMatchIn(text.substring(cursor)).map { m =>
          (cursor + m.start, cursor + m.end, InlineTokenKind.Link, m)
        }

        val nextMatch =
          List(codeMatch, linkMatch).flatten.sortBy(_._1).headOption

        nextMatch match
          case Some((start, end, kind, matched)) if start > cursor =>
            appendPlain(start)
            kind match
              case InlineTokenKind.InlineCode =>
                segments += StyledText(
                  matched.matched,
                  baseStyle.combine(TextStyle.italic),
                  inlineCodeColor.foreground,
                  inlineCodeColor.background
                )
              case InlineTokenKind.Link =>
                segments += StyledText("[", baseStyle, markerColor.foreground, theme.background)
                segments += StyledText(matched.group(1), baseStyle.combine(TextStyle.underlined), linkTextColor.foreground, theme.background)
                segments += StyledText("](", baseStyle, markerColor.foreground, theme.background)
                segments += StyledText(matched.group(2), baseStyle.combine(TextStyle.underlined), linkUrlColor.foreground, theme.background)
                segments += StyledText(")", baseStyle, markerColor.foreground, theme.background)
            cursor = end
          case Some((_, end, kind, matched)) =>
            kind match
              case InlineTokenKind.InlineCode =>
                segments += StyledText(
                  matched.matched,
                  baseStyle.combine(TextStyle.italic),
                  inlineCodeColor.foreground,
                  inlineCodeColor.background
                )
              case InlineTokenKind.Link =>
                segments += StyledText("[", baseStyle, markerColor.foreground, theme.background)
                segments += StyledText(matched.group(1), baseStyle.combine(TextStyle.underlined), linkTextColor.foreground, theme.background)
                segments += StyledText("](", baseStyle, markerColor.foreground, theme.background)
                segments += StyledText(matched.group(2), baseStyle.combine(TextStyle.underlined), linkUrlColor.foreground, theme.background)
                segments += StyledText(")", baseStyle, markerColor.foreground, theme.background)
            cursor = end
          case None =>
            appendPlain(text.length)
            cursor = text.length

      segments.toList

    line match
      case headingPattern(marker, content) =>
        StyledText(marker, TextStyle.bold, markerColor.foreground, theme.background) ::
          withInlineMarkdownStyling(content, TextStyle.bold, headingColor.foreground)
      case unorderedListPattern(marker, content) =>
        StyledText(marker, TextStyle.bold, markerColor.foreground, theme.background) ::
          withInlineMarkdownStyling(content)
      case orderedListPattern(marker, content) =>
        StyledText(marker, TextStyle.bold, markerColor.foreground, theme.background) ::
          withInlineMarkdownStyling(content)
      case blockQuotePattern(marker, content) =>
        StyledText(marker, TextStyle.italic, theme.muted, theme.background) ::
          withInlineMarkdownStyling(content, TextStyle.italic, theme.muted)
      case _ =>
        withInlineMarkdownStyling(line)
