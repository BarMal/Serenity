package com.serenity.ui.renderer

import java.awt.Color

import com.serenity.state.models.*
import com.serenity.ui.layout.*
import com.serenity.ui.theme.*

/** Paints per-character background highlights over an already-drawn text row: selections, document comments, and LSP
  * diagnostics. All three share [[renderTextRangeBackground]]/[[columnsForRange]] so a highlight looks and clips
  * identically whether the row was drawn through the measured (GUI) or cell-based (TUI) path.
  */
object RendererHighlights:

  def renderSelectionHighlights(
    surface: RenderSurface,
    buffer: Buffer,
    visualLine: TextVisualLine,
    rect: LayoutRect,
    screenY: Int,
    lineTopPx: Int,
    theme: Theme,
    context: RenderContext,
    snapshot: TextLayoutSnapshot
  ): Unit =
    buffer.allSelections.foreach { selection =>
      columnsForRange(selection.start, selection.end, visualLine, markPoint = false).foreach {
        case (selectionStart, selectionEnd) =>
          if RendererPaneSetup.usesMeasuredDrawing(snapshot, context) then
            val localStart = selectionStart - visualLine.startColumn
            val localEnd   = selectionEnd - visualLine.startColumn
            if localStart >= 0 && localStart < localEnd && localEnd <= visualLine.text.length then
              val selectedText = visualLine.text.substring(localStart, localEnd)
              val lineOriginPx = context.cellMetrics.toPixelX(rect.x).toFloat
              val startXPx     = lineOriginPx + visualLine.xForColumn(selectionStart).getOrElse(visualLine.widthPx)
              val endXPx       = lineOriginPx + visualLine.xForColumn(selectionEnd).getOrElse(visualLine.widthPx)
              RendererCursorGlyphs.measuredRunWidthWithin(rect, context, startXPx, endXPx).foreach { widthPx =>
                surface.setForegroundColor(theme.highlighted.foreground)
                surface.setBackgroundColor(theme.highlighted.background)
                surface.text.drawRunPx(
                  startXPx,
                  lineTopPx,
                  widthPx,
                  snapshot.lineHeightPx,
                  snapshot.ascentPx,
                  selectedText,
                  clipGlyphToRun = true
                )
              }
          else
            (selectionStart until selectionEnd).foreach { bufferColumn =>
              val relativeColumn = bufferColumn - visualLine.startColumn
              val screenX = rect.x + RendererPaneContent.visualLineCellOffset(visualLine, context) + relativeColumn
              if screenX >= rect.x && screenX < rect.right then
                val charIndex = bufferColumn - visualLine.startColumn
                val charToRender =
                  if charIndex >= 0 && charIndex < visualLine.text.length then visualLine.text.charAt(charIndex)
                  else ' '
                surface.setForegroundColor(theme.highlighted.foreground)
                surface.setBackgroundColor(theme.highlighted.background)
                CharacterRenderer.renderChar(surface, screenX, screenY, charToRender)
            }
      }
    }

  def renderDocumentCommentHighlights(
    surface: RenderSurface,
    comments: List[DocumentComment],
    visualLine: TextVisualLine,
    rect: LayoutRect,
    screenY: Int,
    lineTopPx: Int,
    theme: Theme,
    context: RenderContext,
    snapshot: TextLayoutSnapshot
  ): Unit =
    comments.foreach { comment =>
      columnsForRange(comment.start, comment.end, visualLine, markPoint = true).foreach {
        case (commentStart, commentEnd) =>
          val foreground = theme.foreground
          renderTextRangeBackground(
            surface,
            visualLine,
            rect,
            screenY,
            lineTopPx,
            foreground,
            commentHighlightBackground(theme),
            context,
            snapshot,
            commentStart,
            commentEnd
          )
      }
    }

  def commentHighlightBackground(theme: Theme): Color =
    blend(theme.warning.background, theme.background, warningWeight = 0.45)

  /** Word-level counterpart to the gutter "!" marker `renderDiagnosticIndicator` paints: each diagnostic's own `range`
    * (an unknown word for spell-check, or an LSP diagnostic sharing the same `DiagnosticsState` pipeline) gets a
    * background highlight so the flagged text itself is visible, not just its line. Works in both the measured (GUI)
    * and cell-based (TUI) drawing paths via `renderTextRangeBackground`, the same helper
    * `renderDocumentCommentHighlights` uses.
    */
  def renderDiagnosticHighlights(
    surface: RenderSurface,
    lineDiagnostics: List[com.serenity.lsp.model.Diagnostic],
    visualLine: TextVisualLine,
    rect: LayoutRect,
    screenY: Int,
    lineTopPx: Int,
    theme: Theme,
    context: RenderContext,
    snapshot: TextLayoutSnapshot
  ): Unit =
    lineDiagnostics.foreach { diagnostic =>
      val start = CursorPosition(diagnostic.range.start.line, diagnostic.range.start.character)
      val end   = CursorPosition(diagnostic.range.end.line, diagnostic.range.end.character)
      columnsForRange(start, end, visualLine, markPoint = false).foreach {
        case (diagStart, diagEnd) =>
          renderTextRangeBackground(
            surface,
            visualLine,
            rect,
            screenY,
            lineTopPx,
            theme.foreground,
            diagnosticHighlightBackground(theme, diagnostic.severity.map(_.code)),
            context,
            snapshot,
            diagStart,
            diagEnd
          )
      }
    }

  def diagnosticHighlightBackground(theme: Theme, severityCode: Option[Int]): Color =
    val accent = severityCode match
      case Some(1) => theme.error.background
      case Some(2) => theme.warning.background
      case _       => theme.muted
    blend(accent, theme.background, warningWeight = 0.45)

  private def blend(foreground: Color, background: Color, warningWeight: Double): Color =
    val clampedWeight    = math.max(0.0, math.min(1.0, warningWeight))
    val backgroundWeight = 1.0 - clampedWeight
    def blendChannel(channel: Color => Int): Int =
      math.round(channel(foreground) * clampedWeight + channel(background) * backgroundWeight).toInt

    Color(blendChannel(_.getRed), blendChannel(_.getGreen), blendChannel(_.getBlue))

  private[renderer] def renderTextRangeBackground(
    surface: RenderSurface,
    visualLine: TextVisualLine,
    rect: LayoutRect,
    screenY: Int,
    lineTopPx: Int,
    foreground: java.awt.Color,
    background: java.awt.Color,
    context: RenderContext,
    snapshot: TextLayoutSnapshot,
    rangeStart: Int,
    rangeEnd: Int
  ): Unit =
    if RendererPaneSetup.usesMeasuredDrawing(snapshot, context) then
      val localStart = rangeStart - visualLine.startColumn
      val localEnd   = rangeEnd - visualLine.startColumn
      if localStart >= 0 && localStart < localEnd then
        val rangeText =
          if localStart < visualLine.text.length then
            visualLine.text.substring(localStart, math.min(localEnd, visualLine.text.length))
          else " "
        val lineOriginPx = context.cellMetrics.toPixelX(rect.x).toFloat
        val startXPx     = lineOriginPx + visualLine.xForColumn(rangeStart).getOrElse(visualLine.widthPx)
        val endXPx =
          if rangeStart == rangeEnd - 1 && rangeStart >= visualLine.endColumn then
            startXPx + context.cellMetrics.charWidth
          else lineOriginPx + visualLine.xForColumn(rangeEnd).getOrElse(visualLine.widthPx)
        val desiredWidthPx = math.max(context.cellMetrics.charWidth.toFloat, endXPx - startXPx)
        RendererCursorGlyphs.measuredRunWidthWithin(rect, context, startXPx, startXPx + desiredWidthPx).foreach {
          widthPx =>
            surface.setForegroundColor(foreground)
            surface.setBackgroundColor(background)
            surface.text.drawRunPx(
              startXPx,
              lineTopPx,
              widthPx,
              snapshot.lineHeightPx,
              snapshot.ascentPx,
              rangeText,
              clipGlyphToRun = true
            )
        }
    else
      (rangeStart until rangeEnd).foreach { bufferColumn =>
        val relativeColumn = bufferColumn - visualLine.startColumn
        val screenX        = rect.x + RendererPaneContent.visualLineCellOffset(visualLine, context) + relativeColumn
        if screenX >= rect.x && screenX < rect.right then
          val charIndex = bufferColumn - visualLine.startColumn
          val charToRender =
            if charIndex >= 0 && charIndex < visualLine.text.length then visualLine.text.charAt(charIndex)
            else ' '
          surface.setForegroundColor(foreground)
          surface.setBackgroundColor(background)
          CharacterRenderer.renderChar(surface, screenX, screenY, charToRender)
      }

  private def columnsForRange(
    start: CursorPosition,
    end: CursorPosition,
    visualLine: TextVisualLine,
    markPoint: Boolean
  ): Option[(Int, Int)] =
    if visualLine.bufferLine < start.line || visualLine.bufferLine > end.line then None
    else if markPoint && start == end && visualLine.bufferLine == start.line then
      Option.when(start.column >= visualLine.startColumn && start.column <= visualLine.endColumn)(
        start.column -> (start.column + 1)
      )
    else
      val rangeStart =
        if visualLine.bufferLine == start.line then start.column else visualLine.startColumn
      val rangeEnd =
        if visualLine.bufferLine == end.line then end.column else visualLine.endColumn
      val clippedStart = math.max(rangeStart, visualLine.startColumn)
      val clippedEnd   = math.min(rangeEnd, visualLine.endColumn)
      Option.when(clippedStart < clippedEnd)(clippedStart -> clippedEnd)
