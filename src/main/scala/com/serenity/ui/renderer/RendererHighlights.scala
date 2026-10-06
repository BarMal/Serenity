package com.serenity.ui.renderer

import com.serenity.spellcheck.SpellChecker
import com.serenity.state.models.*
import com.serenity.ui.color.RenderColor
import com.serenity.ui.layout.*
import com.serenity.ui.theme.*

/** Paints per-character background highlights over an already-drawn text row: selections, document comments, LSP
  * diagnostics and find matches. All of them share [[renderTextRangeBackground]]/[[columnsForRange]] so a highlight
  * looks and clips identically whether the row was drawn through the measured (GUI) or cell-based (TUI) path.
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
    buffer.editing.cursors.toList.foreach { cursor =>
      selectionColumnsForVisualLine(cursor, visualLine).foreach {
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

  /** Find matches on this row, the selected one stronger than the rest. Painted before the selection so a selection
    * over a match still reads as the selection.
    */
  def renderFindMatchHighlights(
    surface: RenderSurface,
    matches: List[FindHighlight],
    visualLine: TextVisualLine,
    rect: LayoutRect,
    screenY: Int,
    lineTopPx: Int,
    theme: Theme,
    context: RenderContext,
    snapshot: TextLayoutSnapshot,
    styledSegments: Option[List[StyledText]] = None
  ): Unit =
    matches.foreach { found =>
      columnsForRange(found.start, found.end, visualLine, markPoint = false).foreach {
        case (matchStart, matchEnd) =>
          renderTextRangeBackground(
            surface,
            visualLine,
            rect,
            screenY,
            lineTopPx,
            theme.foreground,
            if found.current then currentFindMatchBackground(theme) else findMatchBackground(theme),
            context,
            snapshot,
            matchStart,
            matchEnd,
            styledSegments
          )
      }
    }

  def findMatchBackground(theme: Theme): RenderColor =
    theme.accent.mixOver(theme.background, 0.3)

  def currentFindMatchBackground(theme: Theme): RenderColor =
    theme.accent.mixOver(theme.background, 0.6)

  def commentHighlightBackground(theme: Theme): RenderColor =
    theme.warning.background.mixOver(theme.background, 0.45)

  /** Word-level counterpart to the gutter "!" marker `renderDiagnosticIndicator` paints: each diagnostic's own `range`
    * (an unknown word for spell-check, or an LSP diagnostic sharing the same `DiagnosticsState` pipeline) gets a
    * background highlight so the flagged text itself is visible, not just its line. Works in both the measured (GUI)
    * and cell-based (TUI) drawing paths via `renderTextRangeBackground`, the same helper
    * `renderDocumentCommentHighlights` uses.
    *
    * `dimmed` (#1530) is whether focus mode's `RendererPaneContent.focusedTextBodyLines` already muted this line: the
    * highlight then blends its severity colour against the theme's own dimmed tone rather than painting full intensity,
    * so a misspelled word in an out-of-focus paragraph still reads as dimmed overall. `blendWeight` (#1529) is the
    * theme/config-exposed strength of that blend (`SurfaceConfig.diagnosticHighlightBlendWeight`), not a hardcoded
    * literal.
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
    styledSegments: Option[List[StyledText]] = None,
    dimmed: Boolean = false,
    blendWeight: Double = DefaultDiagnosticHighlightBlendWeight
  ): Unit =
    lineDiagnostics.foreach { diagnostic =>
      val start        = CursorPosition(diagnostic.range.start.line, diagnostic.range.start.character)
      val end          = CursorPosition(diagnostic.range.end.line, diagnostic.range.end.character)
      val severityCode = diagnostic.severity.map(_.code)
      columnsForRange(start, end, visualLine, markPoint = false).foreach {
        case (diagStart, diagEnd) if diagnostic.source.contains(SpellChecker.Source) =>
          renderMisspelling(surface, visualLine, rect, screenY, lineTopPx, theme, context, snapshot, diagStart, diagEnd)
        case (diagStart, diagEnd) =>
          renderTextRangeBackground(
            surface,
            visualLine,
            rect,
            screenY,
            lineTopPx,
            diagnosticHighlightForeground(theme, severityCode, dimmed, blendWeight),
            diagnosticHighlightBackground(theme, severityCode, dimmed, blendWeight),
            context,
            snapshot,
            diagStart,
            diagEnd,
            styledSegments,
            extraStyle = TextStyle(isUnderlined = severityThemeColor(theme, severityCode).style.isUnderlined)
          )
      }
    }

  /** A misspelling (#1809) is a squiggle under the word in the theme's error colour, not a recoloured background: prose
    * stays readable and the mark reads as an error rather than as the warning or search highlight the shared diagnostic
    * background used to resemble. A cell surface has no sub-cell pixels, so there the word is underlined in the error
    * colour instead.
    */
  private def renderMisspelling(
    surface: RenderSurface,
    visualLine: TextVisualLine,
    rect: LayoutRect,
    screenY: Int,
    lineTopPx: Int,
    theme: Theme,
    context: RenderContext,
    snapshot: TextLayoutSnapshot,
    rangeStart: Int,
    rangeEnd: Int
  ): Unit =
    val colour = theme.error.foreground
    if RendererPaneSetup.usesMeasuredDrawing(snapshot, context) then
      val lineOriginPx = context.cellMetrics.toPixelX(rect.x).toFloat
      val startXPx     = lineOriginPx + visualLine.xForColumn(rangeStart).getOrElse(visualLine.widthPx)
      val endXPx       = lineOriginPx + visualLine.xForColumn(rangeEnd).getOrElse(visualLine.widthPx)
      RendererCursorGlyphs.measuredRunWidthWithin(rect, context, startXPx, endXPx).foreach { widthPx =>
        val baselinePx = lineTopPx + RendererPaneContent.rowAscentPxFor(visualLine, snapshot)
        val rowBottom  = lineTopPx + RendererPaneContent.rowHeightPxFor(visualLine, snapshot)
        // The squiggle lives in the descent below the baseline, so a font with little of it gets a shallower one.
        val amplitude = math.min(MaxSquiggleAmplitudePx, math.max(1, rowBottom - baselinePx - 1))
        val gap       = if rowBottom - baselinePx - 1 - amplitude >= 1 then 1 else 0
        val topPx     = math.max(lineTopPx, math.min(baselinePx + gap, rowBottom - 1 - amplitude))
        squiggleSegments(startXPx.round, widthPx.round, topPx, amplitude).foreach { (xPx, yPx, segmentWidthPx) =>
          surface.pixels.fillPixelRect(xPx, yPx, segmentWidthPx, 1, colour)
        }
      }
    else
      val underline = TextStyle(isUnderlined = true)
      surface.enableStyle(underline)
      try
        (rangeStart until rangeEnd).foreach { bufferColumn =>
          val charIndex = bufferColumn - visualLine.startColumn
          val screenX   = rect.x + RendererPaneContent.visualLineCellOffset(visualLine, context) + charIndex
          if screenX >= rect.x && screenX < rect.right && charIndex >= 0 && charIndex < visualLine.text.length then
            surface.setForegroundColor(colour)
            surface.setBackgroundColor(theme.background)
            CharacterRenderer.renderChar(surface, screenX, screenY, visualLine.text.charAt(charIndex))
        }
      finally surface.disableStyle(underline)

  private val MaxSquiggleAmplitudePx = 2
  private val SquiggleStepPx         = 2

  /** One-pixel-high segments tracing a zig-zag `amplitude` deep from `topPx`, as (x, y, width). */
  private[renderer] def squiggleSegments(
    startXPx: Int,
    widthPx: Int,
    topPx: Int,
    amplitude: Int
  ): List[(Int, Int, Int)] =
    (0 until math.max(widthPx, 1) by SquiggleStepPx).toList.map { offset =>
      val phase = (offset / SquiggleStepPx) % (amplitude * 2)
      val depth = if phase <= amplitude then phase else amplitude * 2 - phase
      (startXPx + offset, topPx + depth, math.min(SquiggleStepPx, widthPx - offset).max(1))
    }

  /** The default blend weight (#1529): kept here, rather than only as `SurfaceConfig`'s default, so a caller testing
    * these functions directly (or a legacy 2-arg call) gets the same result the app always used to render.
    */
  val DefaultDiagnosticHighlightBlendWeight: Double = 0.45

  private def severityThemeColor(theme: Theme, severityCode: Option[Int]): ThemeColor =
    severityCode match
      case Some(1) => theme.error
      case Some(2) => theme.warning
      case _       => ThemeColor(theme.foreground, theme.muted)

  /** The diagnostic's own foreground: the severity's theme colour (#1529) at full intensity, or -- inside a dimmed,
    * out-of-focus paragraph (#1530) -- blended toward the theme's muted tone so it still reads as part of the dimmed
    * line rather than jumping back to full intensity.
    */
  def diagnosticHighlightForeground(
    theme: Theme,
    severityCode: Option[Int],
    dimmed: Boolean = false,
    blendWeight: Double = DefaultDiagnosticHighlightBlendWeight
  ): RenderColor =
    val severityForeground = severityThemeColor(theme, severityCode).foreground
    if dimmed then severityForeground.mixOver(theme.muted, blendWeight) else severityForeground

  def diagnosticHighlightBackground(
    theme: Theme,
    severityCode: Option[Int],
    dimmed: Boolean = false,
    blendWeight: Double = DefaultDiagnosticHighlightBlendWeight
  ): RenderColor =
    val accent  = severityThemeColor(theme, severityCode).background
    val blended = accent.mixOver(theme.background, blendWeight)
    if dimmed then blended.mixOver(theme.background, blendWeight) else blended

  /** `rangeStart`/`rangeEnd` (buffer columns) split into the contiguous sub-ranges that share one [[TextStyle]]
    * according to `styledSegments` -- the same per-run style [[com.serenity.ui.renderer.CharacterRenderer]] painted the
    * underlying glyphs with. A highlight overlay that skipped this and always painted with whatever style the surface
    * happened to be left at (#1482) silently dropped the run's own font family/size/weight, so a redraw could come out
    * a different size than the glyphs it was covering. `styledSegments` absent, or not covering the whole range, falls
    * back to [[TextStyle.normal]] for the uncovered part -- the same default the underlying text itself falls back to.
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
          val segmentEnd = offset + segment.content.length
          val chunkStart = math.max(localStart, offset)
          val chunkEnd   = math.min(localEnd, segmentEnd)
          val nextAcc    = if chunkStart < chunkEnd then (chunkStart, chunkEnd, segment.style) :: acc else acc
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
    foreground: RenderColor,
    background: RenderColor,
    context: RenderContext,
    snapshot: TextLayoutSnapshot,
    rangeStart: Int,
    rangeEnd: Int,
    styledSegments: Option[List[StyledText]],
    extraStyle: TextStyle = TextStyle.normal
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
                val combinedStyle = style.combine(extraStyle)
                surface.setForegroundColor(foreground)
                surface.setBackgroundColor(background)
                surface.enableStyle(combinedStyle)
                try
                  surface.text.drawRunPx(
                    startXPx,
                    lineTopPx,
                    widthPx,
                    RendererPaneContent.rowHeightPxFor(visualLine, snapshot),
                    RendererPaneContent.rowAscentPxFor(visualLine, snapshot),
                    chunkText,
                    clipGlyphToRun = true
                  )
                finally surface.disableStyle(combinedStyle)
            }
        }
    else
      surface.enableStyle(extraStyle)
      try
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
      finally surface.disableStyle(extraStyle)

  private[renderer] def selectionColumnsForVisualLine(cursor: Cursor, visualLine: TextVisualLine): Option[(Int, Int)] =
    cursor.selection.flatMap(selection =>
      columnsForRange(selection.start, selection.end, visualLine, markPoint = false)
    )

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
