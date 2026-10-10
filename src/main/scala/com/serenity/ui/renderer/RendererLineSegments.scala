package com.serenity.ui.renderer

import com.serenity.lsp.model.SemanticToken
import com.serenity.state.models.{AppState, Buffer, TextVisualLine}
import com.serenity.ui.layout.TextLayoutSnapshot
import com.serenity.ui.theme.{MarkdownInlineStyling, RichTextStyling, StyledText, TextStyle, Theme}

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
    if !view.isActive then base
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
            context.fontForBuffer(buffer).getSize2D
          )
        )

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

  private def richTextStyledSegments(
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
          snapshot.proseScale
        )
      }
      .filter(segments => segments.map(_.content).mkString == visualLine.text)

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
