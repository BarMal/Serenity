package com.serenity.ui.renderer

import com.serenity.lsp.config.LanguageId
import com.serenity.lsp.model.SemanticToken
import com.serenity.state.models.{AppState, Buffer, TextVisualLine}
import com.serenity.ui.layout.TextLayoutSnapshot
import com.serenity.ui.theme.{HeadingColour, MarkdownInlineStyling, RichTextStyling, StyledText, TextStyle, Theme}

/** The styled segments one visual line is painted with when its styling is not simply the syntax highlighter's: rich
  * text runs, text outside the focused body dimmed, and inline Markdown restyled with its markers hidden. `None` leaves
  * the line to the highlighter.
  */
private[renderer] object RendererLineSegments:

  def forLine(
    visualLine: TextVisualLine,
    theme: Theme,
    snapshot: TextLayoutSnapshot,
    activeBodyLine: Int => Boolean,
    buffer: Buffer,
    state: AppState,
    context: RenderContext,
    semanticTokens: Option[List[SemanticToken]]
  ): Option[List[StyledText]] =
    val base = focusDimmed(visualLine, theme, snapshot, activeBodyLine)
    val view = snapshot.markdownInline
    if !view.isActive then base.orElse(markdownSourceHeading(visualLine, theme, snapshot, buffer, state, context))
    else
      // The whole logical line is scanned, since a wrapped row can sit inside an emphasis that opened on an earlier one.
      val line = buffer.document.content.getLine(visualLine.bufferLine).getOrElse("")
      val runs = view.runsOn(visualLine.bufferLine, line)
      if runs.isEmpty then base
      else
        val segments = base.getOrElse(highlighted(visualLine, theme, buffer, state, context, semanticTokens))
        Some(
          MarkdownInlineStyling.restyle(
            segments,
            runs,
            visualLine.startColumn,
            view.hidesMarkersOn(visualLine.bufferLine),
            theme,
            view.baseIsMonospaced,
            context.fontForBuffer(buffer).getSize2D,
            snapshot.headings
          )
        )

  /** A Markdown heading in source view is highlighted in the theme's heading colour; this repaints its text where the
    * heading settings ask for another, leaving the `#` marker as the theme has it.
    */
  private def markdownSourceHeading(
    visualLine: TextVisualLine,
    theme: Theme,
    snapshot: TextLayoutSnapshot,
    buffer: Buffer,
    state: AppState,
    context: RenderContext
  ): Option[List[StyledText]] =
    Option
      .when(state.syntaxHighlightingEnabled && buffer.document.language.contains(LanguageId.Markdown)) {
        val line = buffer.document.content.getLine(visualLine.bufferLine).getOrElse("")
        headingLevel(line).filter(snapshot.headings.colourFor(_, markdown = true) != HeadingColour.Heading)
      }
      .flatten
      .map { level =>
        highlighted(visualLine, theme, buffer, state, context, None).map { segment =>
          if segment.content.forall(char => char == '#' || char.isWhitespace) then segment
          else
            segment.copy(foregroundColor =
              MarkdownInlineStyling.headingForeground(segment.foregroundColor, level, theme, snapshot.headings)
            )
        }
      }

  private def headingLevel(line: String): Option[Int] =
    val hashes = line.takeWhile(_ == '#').length
    Option.when(hashes >= 1 && hashes <= 6 && line.length > hashes && line.charAt(hashes).isWhitespace)(hashes)

  private def highlighted(
    visualLine: TextVisualLine,
    theme: Theme,
    buffer: Buffer,
    state: AppState,
    context: RenderContext,
    semanticTokens: Option[List[SemanticToken]]
  ): List[StyledText] =
    if state.syntaxHighlightingEnabled then
      context.caches.themeHighlightCache.highlightLine(visualLine.text, theme, buffer.document.language, semanticTokens)
    else List(StyledText(visualLine.text, TextStyle.normal, theme.foreground, theme.background))

  private[renderer] def richTextStyledSegments(
    visualLine: TextVisualLine,
    theme: Theme,
    snapshot: TextLayoutSnapshot
  ): Option[List[StyledText]] =
    snapshot.richTextDocument
      .map { document =>
        RichTextStyling.styledLine(
          document,
          visualLine.bufferLine,
          visualLine.startColumn,
          visualLine.endColumn,
          theme,
          snapshot.proseScale,
          snapshot.headings
        )
      }
      .filter(segments => StyledText.spell(segments, visualLine.text))

  private def focusDimmed(
    visualLine: TextVisualLine,
    theme: Theme,
    snapshot: TextLayoutSnapshot,
    activeBodyLine: Int => Boolean
  ): Option[List[StyledText]] =
    val richSegments = richTextStyledSegments(visualLine, theme, snapshot)
    if activeBodyLine(visualLine.bufferLine) then richSegments
    else
      val baseSegments =
        richSegments.getOrElse(List(StyledText(visualLine.text, TextStyle.normal, theme.foreground, theme.background)))
      Some(baseSegments.map(segment => segment.copy(foregroundColor = theme.muted, backgroundColor = theme.background)))
