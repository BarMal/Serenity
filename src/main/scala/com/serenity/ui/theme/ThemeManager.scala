package com.serenity.ui.theme

import java.util.concurrent.atomic.AtomicReference

import scala.collection.immutable.ListMap

import com.serenity.lsp.config.LanguageId
import com.serenity.lsp.model.SemanticToken

object ThemeManager:

  private val headingPattern       = raw"^(#{1,6}\s+)(.*)$$".r
  private val unorderedListPattern = raw"^(\s*[-*+]\s+)(.*)$$".r
  private val orderedListPattern   = raw"^(\s*\d+\.\s+)(.*)$$".r
  private val blockQuotePattern    = raw"^(\s*>\s?)(.*)$$".r
  private val inlineCodePattern    = "`[^`]+`".r
  private val linkPattern          = raw"\[([^\]]+)\]\(([^)]+)\)".r

  private val MaxHighlightCacheEntries = 4096

  private type HighlightKey = (String, Theme, Option[LanguageId], Option[List[SemanticToken]])

  /** Bounded, `AtomicReference`-backed replacement for the previous `LinkedHashMap` + `synchronized` highlight cache
    * (issue #1412). `highlightLine` stays a plain synchronous `def`: it's called from deep inside the Java2D/terminal
    * paint loop (`CharacterRenderer`, `RendererPaneContent`), which owns its own thread rather than running inside an
    * IO fiber, so making it return `IO` would mean threading `IO` through the entire rendering call graph -- well
    * beyond this package. This module was briefly `Ref[IO, ...]`-backed (#1431), with each accessor forcing that `IO`
    * synchronously via `unsafeRunSync` right back out again to keep this synchronous API -- which just hid a plain
    * in-memory compare-and-set behind an effect type nothing here ever suspended on, rather than pushing `IO` to an
    * edge. [[casUpdate]] below is that same compare-and-set directly, with no `IO` to force (#1434). Eviction here is
    * bounded-FIFO (oldest inserted, not oldest accessed) rather than an access-order LRU -- a deliberate
    * simplification, since a `ListMap` has no cheap way to bump an existing key to "most recently used" without an
    * extra write on every cache *hit* too.
    */
  private val highlightCacheRef: AtomicReference[ListMap[HighlightKey, List[StyledText]]] =
    new AtomicReference(ListMap.empty)

  /** Retries `f` against `ref`'s current value until its compare-and-set succeeds -- the plain-value equivalent of
    * `Ref.update`.
    */
  @annotation.tailrec
  private def casUpdate[A](ref: AtomicReference[A])(f: A => A): Unit =
    val current = ref.get()
    val next    = f(current)
    if !ref.compareAndSet(current, next) then casUpdate(ref)(f)

  private def boundedPut[K, V](cache: ListMap[K, V], key: K, value: V, maxEntries: Int): ListMap[K, V] =
    val updated = (cache - key) + (key -> value)
    if updated.size <= maxEntries then updated else updated.drop(updated.size - maxEntries)

  /** Apply syntax highlighting to a line of text, memoized by (line, theme, language, this line's semantic tokens).
    *
    * LSP `textDocument/semanticTokens` is the only token source (issues #859/#1177 replaced the previous handwritten,
    * Scala-shaped regex tokenizer entirely, rather than keeping it as a fallback): `semanticTokens` is `Some` with this
    * line's tokens (however many that is, including zero) when the document's connected language server has supplied
    * them, or `None` when it hasn't -- no server connected yet, the server doesn't support semantic tokens, or a
    * request is still in flight. `None` renders a visibly distinct "unavailable" style rather than silently falling
    * back to plain text, so a user isn't left wondering whether highlighting is simply absent for this line or
    * genuinely can't be provided right now.
    */
  def highlightLine(
    line: String,
    theme: Theme,
    language: Option[LanguageId] = None,
    semanticTokens: Option[List[SemanticToken]] = None
  ): List[StyledText] =
    val key    = (line, theme, language, semanticTokens)
    val cached = highlightCacheRef.get().get(key)
    cached.getOrElse {
      val computed = computeHighlightLine(line, theme, language, semanticTokens)
      casUpdate(highlightCacheRef)(boundedPut(_, key, computed, MaxHighlightCacheEntries))
      computed
    }

  private def computeHighlightLine(
    line: String,
    theme: Theme,
    language: Option[LanguageId],
    semanticTokens: Option[List[SemanticToken]]
  ): List[StyledText] =
    language match
      case Some(LanguageId.Markdown) => highlightMarkdownLine(line, theme)
      case Some(_) =>
        semanticTokens match
          case Some(tokens) => renderWithSemanticTokens(line, theme, tokens)
          case None         => renderUnavailable(line, theme)
      case None =>
        List(StyledText(line, TextStyle.normal, theme.foreground, theme.background))

  /** Splits `line` into styled runs from `tokens` (this line's [[SemanticToken]]s, in any order, with
    * `startCharacter`/`length` already relative to this line): each token's span gets its mapped
    * [[SyntaxElement.fromLspTokenType]] color, and every gap between/around tokens renders as [[SyntaxElement.Normal]].
    * A token whose reported span falls outside `line`'s bounds is clamped rather than trusted outright -- LSP servers
    * are an external, occasionally inconsistent input, and a bad span should degrade to imprecise highlighting, not an
    * exception or a corrupted line.
    */
  private def renderWithSemanticTokens(line: String, theme: Theme, tokens: List[SemanticToken]): List[StyledText] =
    val normalColor = theme.colorFor(SyntaxElement.Normal)
    def normalRun(text: String): StyledText =
      StyledText(text, TextStyle.normal, normalColor.foreground, normalColor.background)

    val sorted = tokens
      .filter(t => t.length > 0 && t.startCharacter < line.length && t.startCharacter + t.length > 0)
      .sortBy(_.startCharacter)

    @annotation.tailrec
    def loop(cursor: Int, remaining: List[SemanticToken], acc: List[StyledText]): List[StyledText] =
      remaining match
        case Nil =>
          (if cursor < line.length then normalRun(line.substring(cursor)) :: acc else acc).reverse
        case token :: rest =>
          val start = token.startCharacter.max(cursor).max(0)
          val end   = (token.startCharacter + token.length).min(line.length)
          if start >= end then loop(cursor, rest, acc)
          else
            val withGap = if start > cursor then normalRun(line.substring(cursor, start)) :: acc else acc
            val element = SyntaxElement.fromLspTokenType(token.tokenType)
            val color   = theme.colorFor(element)
            val styled  = StyledText(line.substring(start, end), color.style, color.foreground, color.background)
            loop(end, rest, styled :: withGap)

    if line.isEmpty then List(normalRun(""))
    else loop(0, sorted, Nil)

  /** The visible "no syntax highlighting available" treatment for a language-bearing line with no semantic tokens
    * (issue #859/#1177): distinct from both real highlighting and the plain-text rendering an undeclared language gets,
    * so a user can tell "nothing to highlight" apart from "highlighting isn't available right now."
    */
  private def renderUnavailable(line: String, theme: Theme): List[StyledText] =
    List(StyledText(line, TextStyle.italic, theme.muted, theme.background))

  private enum InlineTokenKind:
    case InlineCode, Link

  private def highlightMarkdownLine(line: String, theme: Theme): List[StyledText] =
    val markerColor  = theme.colorFor(SyntaxElement.Delimiter)
    val headingColor = theme.colorFor(SyntaxElement.Keyword)

    line match
      case headingPattern(marker, content) =>
        StyledText(marker, TextStyle.bold, markerColor.foreground, theme.background) ::
          withInlineMarkdownStyling(content, theme, TextStyle.bold, Some(headingColor.foreground))
      case unorderedListPattern(marker, content) =>
        StyledText(marker, TextStyle.bold, markerColor.foreground, theme.background) ::
          withInlineMarkdownStyling(content, theme)
      case orderedListPattern(marker, content) =>
        StyledText(marker, TextStyle.bold, markerColor.foreground, theme.background) ::
          withInlineMarkdownStyling(content, theme)
      case blockQuotePattern(marker, content) =>
        StyledText(marker, TextStyle.italic, theme.muted, theme.background) ::
          withInlineMarkdownStyling(content, theme, TextStyle.italic, Some(theme.muted))
      case _ =>
        withInlineMarkdownStyling(line, theme)

  private def withInlineMarkdownStyling(
    text: String,
    theme: Theme,
    baseStyle: TextStyle = TextStyle.normal,
    foregroundOverride: Option[java.awt.Color] = None
  ): List[StyledText] =
    val defaultForeground = foregroundOverride.getOrElse(theme.foreground)
    val markerColor       = theme.colorFor(SyntaxElement.Delimiter)
    val inlineCodeColor   = theme.colorFor(SyntaxElement.String)
    val linkTextColor     = theme.colorFor(SyntaxElement.Keyword)
    val linkUrlColor      = theme.colorFor(SyntaxElement.String)

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
