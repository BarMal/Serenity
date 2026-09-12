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
    snapshot: TextLayoutSnapshot,
    styledSegments: Option[List[StyledText]] = None
  ): Unit =
    buffer.allSelections.foreach { selection =>
      columnsForRange(selection.start, selection.end, visualLine, markPoint = false).foreach {
        case (selectionStart, selectionEnd) =>
          renderTextRangeBackground(
            surface,
            visualLine,
            rect,
            screenY,
            lineTopPx,
            theme.highlighted.foreground,
            theme.highlighted.background,
            context,
            snapshot,
            selectionStart,
            selectionEnd,
            styledSegments
          )
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
    snapshot: TextLayoutSnapshot,
    styledSegments: Option[List[StyledText]] = None
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
            commentEnd,
            styledSegments
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
    snapshot: TextLayoutSnapshot,
    styledSegments: Option[List[StyledText]] = None
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
            diagEnd,
            styledSegments
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

  /** `rangeStart`/`rangeEnd` (buffer columns) split into the contiguous sub-ranges that share one [[TextStyle]]
    * according to `styledSegments` -- the same per-run style [[com.serenity.ui.renderer.CharacterRenderer]] painted
    * the underlying glyphs with. A highlight overlay that skipped this and always painted with whatever style the
    * surface happened to be left at (#1482) silently dropped the run's own font family/size/weight, so a redraw could
    * come out a different size than the glyphs it was covering. `styledSegments` absent, or not covering the whole
    * range, falls back to [[TextStyle.normal]] for the uncovered part -- the same default the underlying text itself
    * falls back to.
    */
  private def styleRangesWithin(
    styledSegments: Option[List[StyledText]],
    localStart: Int,
    localEnd: Int
  ): List[(Int, Int, TextStyle)] =
    @annotation.tailrec
    def loop(segments: List[StyledText], offset: Int, acc: List[(Int, Int, TextStyle)]): List[(Int, Int, TextStyle)] =
      segments match
        case _ if offset >= localEnd => acc.reverse
        case Nil                     => ((offset.max(localStart), localEnd, TextStyle.normal) :: acc).reverse
        case segment :: rest =>
          val segmentEnd  = offset + segment.content.length
          val chunkStart  = math.max(localStart, offset)
          val chunkEnd    = math.min(localEnd, segmentEnd)
          val nextAcc     = if chunkStart < chunkEnd then (chunkStart, chunkEnd, segment.style) :: acc else acc
          loop(rest, segmentEnd, nextAcc)

    styledSegments match
      case Some(segments) if segments.nonEmpty => loop(segments, 0, Nil)
      case _                                   => List((localStart, localEnd, TextStyle.normal))

  private def renderTextRangeBackground(
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
    rangeEnd: Int,
    styledSegments: Option[List[StyledText]]
  ): Unit =
    if RendererPaneSetup.usesMeasuredDrawing(snapshot, context) then
      val localStart = rangeStart - visualLine.startColumn
      val localEnd   = rangeEnd - visualLine.startColumn
      if localStart >= 0 && localStart < localEnd then
        val lineOriginPx = context.cellMetrics.toPixelX(rect.x).toFloat
        styleRangesWithin(styledSegments, localStart, localEnd).foreach {
          case (chunkStart, chunkEnd, style) =>
            val chunkRangeStart = visualLine.startColumn + chunkStart
            val chunkRangeEnd   = visualLine.startColumn + chunkEnd
            val chunkText =
              if chunkStart < visualLine.text.length then
                visualLine.text.substring(chunkStart, math.min(chunkEnd, visualLine.text.length))
              else " "
            val startXPx = lineOriginPx + visualLine.xForColumn(chunkRangeStart).getOrElse(visualLine.widthPx)
            val endXPx =
              if chunkRangeStart == chunkRangeEnd - 1 && chunkRangeStart >= visualLine.endColumn then
                startXPx + context.cellMetrics.charWidth
              else lineOriginPx + visualLine.xForColumn(chunkRangeEnd).getOrElse(visualLine.widthPx)
            val desiredWidthPx = math.max(context.cellMetrics.charWidth.toFloat, endXPx - startXPx)
            RendererCursorGlyphs.measuredRunWidthWithin(rect, context, startXPx, startXPx + desiredWidthPx).foreach {
              widthPx =>
                surface.setForegroundColor(foreground)
                surface.setBackgroundColor(background)
                surface.enableStyle(style)
                try
                  surface.text.drawRunPx(
                    startXPx,
                    lineTopPx,
                    widthPx,
                    snapshot.lineHeightPx,
                    snapshot.ascentPx,
                    chunkText,
                    clipGlyphToRun = true
                  )
                finally surface.disableStyle(style)
            }
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
