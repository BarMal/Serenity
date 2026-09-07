package com.serenity.ui.renderer

import com.serenity.config.MarkdownViewMode
import com.serenity.lsp.config.LanguageId
import com.serenity.markdown.MarkdownDocumentPreview
import com.serenity.state.manager.FocusedTextBody
import com.serenity.state.models.*
import com.serenity.ui.layout.*

final case class MarkdownLensFrame(
    firstSourceLine: Int,
    lines: Vector[String],
    previewWindow: MarkdownDocumentPreview.PreviewWindow,
    activeSourceRanges: List[Range.Inclusive],
    previewRows: Vector[MarkdownDocumentPreview.InlinePreviewLine],
    placements: Map[Range.Inclusive, RendererMarkdownLens.MarkdownLensPlacement]
)

/** The inline markdown-preview lens (`MarkdownViewMode.InlineLens`): the active source block(s) render as raw text over
  * the rest of the pane's rendered preview, so editing stays possible without leaving preview mode. Frame geometry
  * ([[MarkdownLensFrame]]) is computed once per pane paint and shared between the preview image
  * ([[RendererPaneContent.renderInlineMarkdownPreview]]), the raw-text lens itself ([[renderMarkdownRawLenses]]), and
  * the lens's own cursor painting ([[renderMarkdownLensCursors]]).
  */
object RendererMarkdownLens:

  private val MarkdownSelectionProbeLimit   = 512
  private val MinMarkdownPreviewSourceLines = 32
  private val MarkdownPreviewOverscanFactor = 4

  final case class MarkdownLensPlacement(top: Int, height: Int)

  final case class MarkdownLensPreviewWindow(
      window: MarkdownDocumentPreview.PreviewWindow,
      sourceLineCount: Int
  )

  def isInlineMarkdownLens(buffer: Buffer, state: AppState): Boolean =
    buffer.document.language.contains(
      LanguageId.Markdown
    ) && state.persisted.config.markdownViewMode == MarkdownViewMode.InlineLens

  def markdownLensFrameFor(buffer: Buffer, snapshot: TextLayoutSnapshot): MarkdownLensFrame =
    val previewWindow = markdownPreviewWindow(buffer, buffer.viewport.visibleLines)
    val lines = buffer.document.content.linesFrom(
      previewWindow.window.firstSourceLine,
      previewWindow.sourceLineCount
    )
    val activeRanges = activeMarkdownBlockRanges(buffer)
      .filter(range =>
        range.end >= previewWindow.window.firstSourceLine && range.start < previewWindow.window.firstSourceLine + lines.length
      )
      .map(range =>
        range.start.max(previewWindow.window.firstSourceLine) - previewWindow.window.firstSourceLine to
          range.end.min(previewWindow.window.firstSourceLine + lines.length - 1) - previewWindow.window.firstSourceLine
      )
    val baseRows = MarkdownDocumentPreview.inlinePreviewRows(
      lines,
      firstSourceLine = 0,
      maxSourceLines = lines.length
    )
    val (previewRows, placements) = markdownLensRows(
      baseRows,
      lines,
      activeRanges,
      buffer.editing.cursors.map(_.line - previewWindow.window.firstSourceLine).toSet,
      snapshot,
      previewWindow.window,
      previewWindow.window.firstSourceLine
    )
    MarkdownLensFrame(
      previewWindow.window.firstSourceLine,
      lines,
      previewWindow.window,
      activeRanges,
      previewRows,
      placements
    )

  private def markdownLensRows(
    baseRows: Vector[MarkdownDocumentPreview.InlinePreviewLine],
    lines: Vector[String],
    activeRanges: List[Range.Inclusive],
    activeLines: Set[Int],
    snapshot: TextLayoutSnapshot,
    previewWindow: MarkdownDocumentPreview.PreviewWindow,
    firstSourceLine: Int
  ): (Vector[MarkdownDocumentPreview.InlinePreviewLine], Map[Range.Inclusive, MarkdownLensPlacement]) =
    val (rows, placements, _) = activeRanges.foldLeft(
      (baseRows, Map.empty[Range.Inclusive, MarkdownLensPlacement], 0)
    ) {
      case ((rows, placements, rowDelta), blockRange) =>
        val previewRange = MarkdownDocumentPreview.previewRowsForSourceRange(lines, blockRange).map { range =>
          (range.start - previewWindow.firstPreviewRow + rowDelta) to
            (range.end - previewWindow.firstPreviewRow + rowDelta)
        }
        val visibleActiveLine =
          snapshot.visualLines.exists(line => activeLines.contains(line.bufferLine - firstSourceLine))
        val rawHeight =
          if visibleActiveLine then
            snapshot.visualLines.count(line => blockRange.contains(line.bufferLine - firstSourceLine))
          else 0
        previewRange match
          case Some(range) if rawHeight > 0 && range.end >= 0 && range.start < rows.length =>
            val start        = range.start.max(0).min(rows.length)
            val endExclusive = (range.end + 1).max(start).min(rows.length)
            val replacedRows = endExclusive - start
            val replacement  = Vector.fill(rawHeight)(MarkdownDocumentPreview.InlinePreviewLine(None, ""))
            val nextRows     = rows.take(start) ++ replacement ++ rows.drop(endExclusive)
            (
              nextRows,
              placements + (blockRange -> MarkdownLensPlacement(start, rawHeight)),
              rowDelta + rawHeight - replacedRows
            )
          case _ => (rows, placements, rowDelta)
    }
    rows -> placements

  private def markdownPreviewWindow(buffer: Buffer, visibleRows: Int): MarkdownLensPreviewWindow =
    val lineCount = buffer.document.content.lineCount
    if lineCount == 0 then MarkdownLensPreviewWindow(MarkdownDocumentPreview.PreviewWindow(0, 0, ""), 0)
    else
      val activeLine = buffer.editing.cursors.headOption
        .map(_.line)
        .filter(line => line >= 0 && line < lineCount)
      val activeBlock     = activeLine.map(line => FocusedTextBody.markdownBlock(buffer, line))
      val viewportTopLine = buffer.viewport.topLine.max(0).min(lineCount - 1)
      val windowTopLine = activeLine
        .filter(line =>
          line == viewportTopLine && line > 0 && buffer.document.content.getLine(line).exists(_.trim.isEmpty)
        )
        .filter(line => buffer.document.content.getLine(line - 1).exists(_.trim.matches("^#{1,6}\\s+.*")))
        .map(_ - 1)
        .getOrElse(viewportTopLine)
      val baseSourceLineLimit = markdownPreviewSourceLineLimit(visibleRows)
      val firstSourceLine = activeBlock
        .filter(blockRange => blockRange.end - blockRange.start + 1 <= baseSourceLineLimit)
        .filter(blockRange => blockRange.start < windowTopLine && blockRange.end >= windowTopLine)
        .map(_.start)
        .getOrElse(windowTopLine)
      val windowEndLine = firstSourceLine + baseSourceLineLimit - 1
      val maxSourceLines = activeBlock
        .filter(_ => activeLine.exists(_ <= windowEndLine))
        .filter(blockRange => blockRange.end - blockRange.start + 1 <= baseSourceLineLimit)
        .map(blockRange => math.max(baseSourceLineLimit, blockRange.end - firstSourceLine + baseSourceLineLimit))
        .getOrElse(baseSourceLineLimit)
      MarkdownLensPreviewWindow(
        window = MarkdownDocumentPreview.PreviewWindow(
          firstSourceLine = firstSourceLine,
          firstPreviewRow = MarkdownDocumentPreview
            .previewRowForSourceLine(
              buffer.document.content.linesFrom(firstSourceLine, math.min(maxSourceLines, lineCount - firstSourceLine)),
              0
            )
            .getOrElse(0),
          source = ""
        ),
        sourceLineCount = math.min(maxSourceLines, lineCount - firstSourceLine)
      )

  private[renderer] def markdownPreviewSourceLineLimit(visibleRows: Int): Int =
    math.max(MinMarkdownPreviewSourceLines, visibleRows.max(1) * MarkdownPreviewOverscanFactor)

  private def activeMarkdownBlockRanges(buffer: Buffer): List[Range.Inclusive] =
    val lineCount = buffer.document.content.lineCount
    val cursorRanges = buffer.editing.cursors
      .map(_.line)
      .filter(line => line >= 0 && line < lineCount)
      .map(line => FocusedTextBody.markdownBlock(buffer, line))
    val selectionRanges = buffer.allSelections.flatMap { selection =>
      if lineCount == 0 then Nil
      else
        val startLine = selection.start.line.max(0).min(lineCount - 1)
        val endLine   = selection.end.line.max(0).min(lineCount - 1)
        val selectedLines =
          if endLine - startLine <= MarkdownSelectionProbeLimit then startLine to endLine
          else List(startLine, endLine)
        selectedLines.map(line => FocusedTextBody.markdownBlock(buffer, line)).distinct
    }
    mergeOverlappingMarkdownRanges(cursorRanges ++ selectionRanges)

  private def mergeOverlappingMarkdownRanges(ranges: List[Range.Inclusive]): List[Range.Inclusive] =
    ranges
      .sortBy(range => (range.start, range.end))
      .foldLeft(List.empty[Range.Inclusive]) {
        case (last :: rest, range) if range.start <= last.end =>
          (last.start to last.end.max(range.end)) :: rest
        case (merged, range) =>
          range :: merged
      }
      .reverse

  def renderMarkdownRawLenses(
    buffer: Buffer,
    rect: LayoutRect,
    state: AppState,
    context: RenderContext,
    snapshot: TextLayoutSnapshot,
    frame: MarkdownLensFrame
  ): Unit =
    val lines         = frame.lines
    val previewWindow = frame.previewWindow
    frame.activeSourceRanges.foreach { blockRange =>
      val absoluteBlockRange = (blockRange.start + frame.firstSourceLine) to (blockRange.end + frame.firstSourceLine)
      val blockVisualLines   = snapshot.visualLines.filter(line => absoluteBlockRange.contains(line.bufferLine))
      if blockVisualLines.nonEmpty then
        val placement = frame.placements.getOrElse(
          blockRange,
          markdownLensPlacement(blockRange, blockVisualLines, rect.height, lines, previewWindow)
        )
        val lensY = rect.y + placement.top
        context.surface.setBackgroundColor(state.persisted.theme.panel.background)
        context.surface.fillRect(rect.x, lensY, rect.width, placement.height, ' ')
        blockVisualLines.zipWithIndex.foreach {
          case (visualLine, index) =>
            val screenY = lensY + index
            if screenY >= rect.y && screenY < rect.bottom && screenY >= 0 && screenY < context.surface.viewportHeight
            then
              context.surface.setForegroundColor(state.persisted.theme.foreground)
              context.surface.setBackgroundColor(state.persisted.theme.panel.background)
              if RendererPaneSetup.usesMeasuredDrawing(snapshot, context) then
                CharacterRenderer.renderMeasuredLineWithAnimation(
                  context.surface,
                  context.cellMetrics.toPixelX(rect.x).toFloat,
                  context.cellMetrics.toPixelY(screenY),
                  snapshot.lineHeightPx,
                  snapshot.ascentPx,
                  visualLine,
                  state.persisted.theme.copy(background = state.persisted.theme.panel.background),
                  context.bufferAnimations.getOrElse(buffer.id, com.serenity.animation.AnimationState.empty),
                  syntaxHighlightingEnabled = false,
                  language = None,
                  clipRightXPx = Some(context.cellMetrics.toPixelX(rect.right).toFloat)
                )
              else
                CharacterRenderer.renderStringWithAnimation(
                  context.surface,
                  rect.x,
                  screenY,
                  visualLine.text,
                  state.persisted.theme.copy(background = state.persisted.theme.panel.background),
                  context.bufferAnimations.getOrElse(buffer.id, com.serenity.animation.AnimationState.empty),
                  syntaxHighlightingEnabled = false,
                  language = None,
                  bufferLine = visualLine.bufferLine,
                  bufferStartColumn = visualLine.startColumn,
                  maxColumn = Some(rect.right)
                )
              RendererHighlights.renderSelectionHighlights(
                context.surface,
                buffer,
                visualLine,
                rect,
                screenY,
                context.cellMetrics.toPixelY(screenY),
                state.persisted.theme,
                context,
                snapshot
              )
        }
    }

  def renderMarkdownLensCursors(
    buffer: Buffer,
    rect: LayoutRect,
    theme: com.serenity.ui.theme.Theme,
    config: com.serenity.config.AppConfig,
    context: RenderContext,
    snapshot: TextLayoutSnapshot,
    frame: MarkdownLensFrame
  ): Unit =
    val lines         = frame.lines
    val previewWindow = frame.previewWindow
    frame.activeSourceRanges.foreach { blockRange =>
      val absoluteBlockRange = (blockRange.start + frame.firstSourceLine) to (blockRange.end + frame.firstSourceLine)
      val blockVisualLines   = snapshot.visualLines.filter(line => absoluteBlockRange.contains(line.bufferLine))
      if blockVisualLines.nonEmpty then
        val placement = frame.placements.getOrElse(
          blockRange,
          markdownLensPlacement(blockRange, blockVisualLines, rect.height, lines, previewWindow)
        )
        buffer.editing.cursors.zipWithIndex.foreach { (cursor, cursorIndex) =>
          val isPrimaryCursor = cursorIndex == 0
          val shouldRenderCursor =
            context.cursorVisible || (buffer.editing.cursors.size > 1 && !isPrimaryCursor)
          blockVisualLines.zipWithIndex.collectFirst {
            case (line, visualIndex)
                if line.bufferLine == cursor.line && cursor.column >= line.startColumn && cursor.column <= line.endColumn =>
              val xPx = line.xForColumn(cursor.column).getOrElse(line.widthPx)
              (visualIndex, xPx)
          } match
            case Some((visualLine, xPx)) if shouldRenderCursor =>
              val screenYCell = rect.y + placement.top + visualLine
              if screenYCell >= rect.y && screenYCell < rect.bottom &&
                  screenYCell >= 0 && screenYCell < context.surface.viewportHeight
              then
                val effectiveCursorColor = RendererCursorGlyphs.cursorColorFor(config, theme, context, isPrimaryCursor)
                val caretWidthPx         = math.max(2, math.round(context.cellMetrics.charWidth * 0.12f))
                val screenXPx            = context.cellMetrics.toPixelX(rect.x) + math.round(xPx)
                val screenYPx = TextRowMetrics(
                  contentRect = rect.copy(y = rect.y + placement.top, height = placement.height),
                  gridMetrics = context.cellMetrics,
                  rowLineHeightPx = context.cellMetrics.lineHeight,
                  usesMeasuredLayout = false
                ).cursorTopPx(visualLine)
                RendererCursorGlyphs.caretWithin(rect, context.cellMetrics, screenXPx, caretWidthPx).foreach {
                  (caretXPx, widthPx) =>
                    context.surface.pixels.fillPixelRect(
                      caretXPx,
                      screenYPx,
                      widthPx,
                      context.cellMetrics.lineHeight,
                      effectiveCursorColor
                    )
                }
            case _ => ()
        }
    }

  private def markdownLensPlacement(
    blockRange: Range.Inclusive,
    blockVisualLines: Vector[TextVisualLine],
    visibleHeight: Int,
    markdownLines: Vector[String],
    previewWindow: MarkdownDocumentPreview.PreviewWindow
  ): MarkdownLensPlacement =
    val previewRange = MarkdownDocumentPreview.previewRowsForSourceRange(markdownLines, blockRange)
    val lensHeight = math.max(
      blockVisualLines.length,
      previewRange.map(range => range.end - range.start + 1).getOrElse(0)
    )
    val desiredTop = previewRange
      .map(_.start - previewWindow.firstPreviewRow)
      .getOrElse(blockRange.start - previewWindow.firstSourceLine)
    val visibleLensHeight = lensHeight.max(1).min(visibleHeight.max(1))
    MarkdownLensPlacement(
      top = desiredTop.max(0).min(math.max(0, visibleHeight - visibleLensHeight)),
      height = visibleLensHeight
    )
