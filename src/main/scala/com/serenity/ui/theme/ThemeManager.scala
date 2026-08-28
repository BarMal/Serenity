package com.serenity.ui.theme

import java.util.LinkedHashMap

import com.serenity.lsp.config.LanguageId

object ThemeManager:

  private val headingPattern       = raw"^(#{1,6}\s+)(.*)$$".r
  private val unorderedListPattern = raw"^(\s*[-*+]\s+)(.*)$$".r
  private val orderedListPattern   = raw"^(\s*\d+\.\s+)(.*)$$".r
  private val blockQuotePattern    = raw"^(\s*>\s?)(.*)$$".r
  private val inlineCodePattern    = "`[^`]+`".r
  private val linkPattern          = raw"\[([^\]]+)\]\(([^)]+)\)".r

  private val MaxHighlightCacheEntries = 4096
  private val MaxLexIndexCacheEntries  = 64

  private val highlightCache =
    new LinkedHashMap[(String, Theme, Option[LanguageId], LexState), List[StyledText]](16, 0.75f, true):
      override def removeEldestEntry(
        eldest: java.util.Map.Entry[(String, Theme, Option[LanguageId], LexState), List[StyledText]]
      ): Boolean =
        size() > MaxHighlightCacheEntries

  /** The subset of languages that get token-aware highlighting today. Every other language, including no declared
    * language at all, renders as plain text rather than being coerced through Scala-shaped lexical rules -- see issue
    * #859.
    */
  private def isTokenAware(language: Option[LanguageId]): Boolean =
    language.contains(LanguageId.Scala)

  /** Apply syntax highlighting to a line of text, memoized by (line, theme, language, incoming lexical state).
    *
    * `startState` is the lexical state carried in from the end of the previous line (an open block comment or
    * triple-quoted string). Callers that need correct multiline behaviour across a document should derive it from
    * [[lineStartStates]]; a bare call defaults to [[LexState.Default]], which is correct for any line that isn't
    * continuing an unterminated comment or string.
    */
  def highlightLine(
    line: String,
    theme: Theme,
    language: Option[LanguageId] = None,
    startState: LexState = LexState.Default
  ): List[StyledText] =
    val key    = (line, theme, language, startState)
    val cached = highlightCache.synchronized(Option(highlightCache.get(key)))
    cached.getOrElse {
      val computed = computeHighlightLine(line, theme, language, startState)
      highlightCache.synchronized(highlightCache.put(key, computed): Unit)
      computed
    }

  private def computeHighlightLine(
    line: String,
    theme: Theme,
    language: Option[LanguageId],
    startState: LexState
  ): List[StyledText] =
    language match
      case Some(LanguageId.Markdown) => highlightMarkdownLine(line, theme)
      case _ if isTokenAware(language) =>
        val (tokens, _) = tokenize(line, startState)
        tokens.map {
          case (text, element) =>
            val themeColor = theme.colorFor(element)
            StyledText(text, themeColor.style, themeColor.foreground, themeColor.background)
        }
      case _ =>
        List(StyledText(line, TextStyle.normal, theme.foreground, theme.background))

  /** The lexical state at the *start* of each line in `lines`, threading an open block comment or triple-quoted string
    * forward from wherever it was opened. Languages without token-aware highlighting always start `Default`.
    *
    * Recomputation is incremental: `documentKey` identifies the document (callers pass something stable per buffer,
    * e.g. its buffer id) so that on a repeat call for the same document, only lines from the first one that actually
    * changed are re-scanned, and that re-scan stops as soon as the newly computed state re-converges with what was
    * previously cached -- an edit only invalidates the region whose lexical state it could plausibly have changed.
    */
  def lineStartStates(
    documentKey: String,
    lines: IndexedSeq[String],
    language: Option[LanguageId]
  ): Vector[LexState] =
    if !isTokenAware(language) then Vector.fill(lines.length)(LexState.Default)
    else
      val linesVec = lines.toVector
      val previous = lexIndexCache.synchronized(Option(lexIndexCache.get(documentKey)))
      val computed = previous match
        case Some(entry) => incrementalStates(entry, linesVec)
        case None        => fullStates(linesVec)
      lexIndexCache.synchronized(lexIndexCache.put(documentKey, LexIndexEntry(linesVec, computed)): Unit)
      computed

  final private case class LexIndexEntry(lines: Vector[String], startStates: Vector[LexState])

  private val lexIndexCache =
    new LinkedHashMap[String, LexIndexEntry](16, 0.75f, true):
      override def removeEldestEntry(eldest: java.util.Map.Entry[String, LexIndexEntry]): Boolean =
        size() > MaxLexIndexCacheEntries

  private def fullStates(lines: Vector[String]): Vector[LexState] =
    lines.scanLeft(LexState.Default)((state, line) => tokenize(line, state)._2).dropRight(1)

  private def incrementalStates(entry: LexIndexEntry, lines: Vector[String]): Vector[LexState] =
    if entry.lines.length != lines.length then fullStates(lines)
    else
      entry.lines.indices.find(i => entry.lines(i) != lines(i)) match
        case None => entry.startStates
        case Some(diffIndex) =>
          val prefix = entry.startStates.take(diffIndex)

          @annotation.tailrec
          def recompute(i: Int, state: LexState, acc: Vector[LexState]): Vector[LexState] =
            if i >= lines.length then acc
            else
              val endState = tokenize(lines(i), state)._2
              val nextAcc  = acc :+ state
              if i + 1 < lines.length && endState == entry.startStates(i + 1) then
                nextAcc ++ entry.startStates.drop(i + 1)
              else recompute(i + 1, endState, nextAcc)

          recompute(diffIndex, entry.startStates(diffIndex), prefix)

  /** Tokenize a single line, threading `startState` in and returning the state at the end of the line alongside the
    * classified tokens. This is the only token source consumed by rendering for token-aware languages.
    */
  private def tokenize(content: String, startState: LexState): (List[(String, SyntaxElement)], LexState) =
    @annotation.tailrec
    def loop(
      remaining: String,
      state: LexState,
      acc: List[(String, SyntaxElement)]
    ): (List[(String, SyntaxElement)], LexState) =
      if remaining.isEmpty then (acc.reverse, state)
      else
        val (token, rest, nextState, element) = extractNextToken(remaining, state)
        if token.isEmpty then (acc.reverse, state)
        else loop(rest, nextState, (token, element) :: acc)

    loop(content, startState, Nil)

  /** Extract the next token from `content` given the incoming lexical `state`, returning the token text, the remaining
    * content, the state after the token, and the token's syntax element.
    */
  private def extractNextToken(content: String, state: LexState): (String, String, LexState, SyntaxElement) =
    state match
      case LexState.InBlockComment =>
        val endIndex = content.indexOf("*/")
        if endIndex == -1 then (content, "", LexState.InBlockComment, SyntaxElement.Comment)
        else
          val token = content.substring(0, endIndex + 2)
          (token, content.drop(token.length), LexState.Default, SyntaxElement.Comment)

      case LexState.InTripleQuotedString =>
        val endIndex = content.indexOf("\"\"\"")
        if endIndex == -1 then (content, "", LexState.InTripleQuotedString, SyntaxElement.String)
        else
          val token = content.substring(0, endIndex + 3)
          (token, content.drop(token.length), LexState.Default, SyntaxElement.String)

      case LexState.Default =>
        if content.isEmpty then ("", "", LexState.Default, SyntaxElement.Normal)
        else
          content.head match
            case ' ' | '\t' | '\n' | '\r' =>
              val whitespace = content.takeWhile(c => c == ' ' || c == '\t' || c == '\n' || c == '\r')
              (whitespace, content.drop(whitespace.length), LexState.Default, SyntaxElement.Whitespace)

            case '"' if content.startsWith("\"\"\"") =>
              val closing = content.drop(3).indexOf("\"\"\"")
              if closing == -1 then (content, "", LexState.InTripleQuotedString, SyntaxElement.String)
              else
                val token = content.substring(0, 3 + closing + 3)
                (token, content.drop(token.length), LexState.Default, SyntaxElement.String)

            case '"' =>
              val closing = content.indexOf('"', 1)
              if closing == -1 then (content, "", LexState.Default, SyntaxElement.String)
              else
                val token = content.substring(0, closing + 1)
                (token, content.drop(token.length), LexState.Default, SyntaxElement.String)

            case '\'' =>
              val closing = content.indexOf('\'', 1)
              if closing == -1 then (content, "", LexState.Default, SyntaxElement.String)
              else
                val token = content.substring(0, closing + 1)
                (token, content.drop(token.length), LexState.Default, SyntaxElement.String)

            case '/' if content.startsWith("//") =>
              val newlineIndex = content.indexOf('\n')
              val token        = if newlineIndex == -1 then content else content.substring(0, newlineIndex)
              (token, content.drop(token.length), LexState.Default, SyntaxElement.Comment)

            case '/' if content.startsWith("/*") =>
              val endIndex = content.indexOf("*/", 2)
              if endIndex == -1 then (content, "", LexState.InBlockComment, SyntaxElement.Comment)
              else
                val token = content.substring(0, endIndex + 2)
                (token, content.drop(token.length), LexState.Default, SyntaxElement.Comment)

            case c if c.isLetter || c == '_' =>
              val identifier = content.takeWhile(c => c.isLetterOrDigit || c == '_')
              (identifier, content.drop(identifier.length), LexState.Default, SyntaxElement.fromText(identifier))

            case c if c.isDigit =>
              val number =
                content.takeWhile(c => c.isDigit || c == '.' || c == 'f' || c == 'F' || c == 'd' || c == 'D')
              (number, content.drop(number.length), LexState.Default, SyntaxElement.fromText(number))

            case c if "(){}[],;:.?".contains(c) =>
              (c.toString, content.tail, LexState.Default, SyntaxElement.Delimiter)

            case c if "+-*/%=<>!&|^~".contains(c) =>
              val operator = content.takeWhile("+-*/%=<>!&|^~".contains(_))
              (operator, content.drop(operator.length), LexState.Default, SyntaxElement.Operator)

            case _ =>
              (content.head.toString, content.tail, LexState.Default, SyntaxElement.Normal)

  private def highlightMarkdownLine(line: String, theme: Theme): List[StyledText] =
    val markerColor     = theme.colorFor(SyntaxElement.Delimiter)
    val headingColor    = theme.colorFor(SyntaxElement.Keyword)
    val inlineCodeColor = theme.colorFor(SyntaxElement.String)
    val linkTextColor   = theme.colorFor(SyntaxElement.Keyword)
    val linkUrlColor    = theme.colorFor(SyntaxElement.String)

    def withInlineMarkdownStyling(
      text: String,
      baseStyle: TextStyle = TextStyle.normal,
      defaultForeground: java.awt.Color = theme.foreground
    ): List[StyledText] =
      enum InlineTokenKind:
        case InlineCode, Link

      def plainSegment(cursor: Int, until: Int): List[StyledText] =
        if until > cursor then
          List(StyledText(text.substring(cursor, until), baseStyle, defaultForeground, theme.background))
        else Nil

      def styledToken(kind: InlineTokenKind, matched: scala.util.matching.Regex.Match): List[StyledText] =
        kind match
          case InlineTokenKind.InlineCode =>
            List(
              StyledText(
                matched.matched,
                baseStyle.combine(TextStyle.italic),
                inlineCodeColor.foreground,
                inlineCodeColor.background
              )
            )
          case InlineTokenKind.Link =>
            List(
              StyledText("[", baseStyle, markerColor.foreground, theme.background),
              StyledText(
                matched.group(1),
                baseStyle.combine(TextStyle.underlined),
                linkTextColor.foreground,
                theme.background
              ),
              StyledText("](", baseStyle, markerColor.foreground, theme.background),
              StyledText(
                matched.group(2),
                baseStyle.combine(TextStyle.underlined),
                linkUrlColor.foreground,
                theme.background
              ),
              StyledText(")", baseStyle, markerColor.foreground, theme.background)
            )

      @annotation.tailrec
      def loop(cursor: Int, acc: List[StyledText]): List[StyledText] =
        if cursor >= text.length then acc.reverse
        else
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
              loop(end, (plainSegment(cursor, start) ++ styledToken(kind, matched)).reverse ::: acc)
            case Some((_, end, kind, matched)) =>
              loop(end, styledToken(kind, matched).reverse ::: acc)
            case None =>
              (plainSegment(cursor, text.length).reverse ::: acc).reverse

      loop(0, Nil)

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
