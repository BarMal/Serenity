package com.serenity.ui.renderer

import java.awt.Font
import java.awt.font.FontRenderContext

import com.serenity.state.models.*
import com.serenity.ui.color.RenderColor
import com.serenity.ui.layout.*

/** Paints the line-number column and the pinned status row (`AppState.statusLineText`), and the small chrome-text
  * helpers those two share.
  */
object RendererGutter:

  /** How far, in logical pixels, a label's ink can spill past the band of its row. */
  private val InkSpillPx = 2

  /** Paints the line numbers of the active pane's rows that `rows` names. The other rows' bands are left alone: a
    * persisting surface still holds their numbers, and a number repaints only with its own row.
    */
  def renderLineNumbers(
    state: AppState,
    context: RenderContext,
    renderPlan: EditorPaneRenderPlan,
    rows: Repaint[Int],
    railRows: PaneId => Repaint[Int] = _ => Repaint.all[Int]
  ): Unit =
    if state.chrome.lineNumbers then
      // Multi-column e-reader layout (issue #1338, Phase 2 / slice 2): a column-mode page has no single shared
      // pane-level rail (`LayoutEngine` reserves none in that case); each column carries its own rail on its left edge,
      // painted inline against that column's own chunk of rows.
      if paintsColumnRails(state) then renderPerColumnRails(state, context, renderPlan, railRows)
      else
        context.surface.text.setFont(FontSpec.fromAwt(context.uiFont))
        renderPlan.layoutContract.lineNumberRect.foreach(rect =>
          renderCounterColumn(state, context, renderPlan, rect, dividerOnLeft = false, rows)
        )
        renderPlan.layoutContract.rightLineNumberRect.foreach(rect =>
          renderCounterColumn(state, context, renderPlan, rect, dividerOnLeft = true, rows)
        )

  /** Whether line numbers are drawn as one rail per e-reader column rather than as the pane's single counter. */
  private[renderer] def paintsColumnRails(state: AppState): Boolean =
    val surfaceConfig = state.persisted.config.surfaceConfig
    state.chrome.lineNumbers && surfaceConfig.columnModeEnabled && surfaceConfig.wordWrapEnabled

  /** Paint each e-reader column's own line-number rail (slice 2). Every pane carrying column placements draws a rail on
    * the leftmost `gutterWidthCells` of each column's band, aligned row-for-row against that column's own snapshot -- a
    * buffer line number on the row a buffer line starts, a continuation indicator on a wrapped row -- so the rails read
    * exactly like the single-column gutter, one per column.
    */
  private def renderPerColumnRails(
    state: AppState,
    context: RenderContext,
    renderPlan: EditorPaneRenderPlan,
    railRows: PaneId => Repaint[Int]
  ): Unit =
    context.surface.text.setFont(FontSpec.fromAwt(context.uiFont))
    state.persisted.layout.editorPanes.foreach {
      case (paneId, _) =>
        renderPlan.paneLayouts.get(paneId).foreach { paneLayout =>
          val columns = renderPlan.columnSnapshotsFor(paneId)
          columns.zip(ColumnPaneRows.localRepaints(columns, railRows(paneId))).foreach { (placement, rows) =>
            if placement.gutterWidthCells > 0 then
              renderColumnRail(state, context, paneLayout.contentRect, placement, rows)
          }
        }
    }

  private def renderColumnRail(
    state: AppState,
    context: RenderContext,
    contentRect: LayoutRect,
    placement: ColumnSnapshotPlacement,
    rows: Repaint[Int]
  ): Unit =
    val surface  = context.surface
    val railRect = ColumnRails.railRect(contentRect, placement)
    val snapshot = placement.snapshot
    val kept     = ColumnRails.keptBands(context, railRect, snapshot, rows)

    surface.setBackgroundColor(gutterBackground(state))
    surface.setForegroundColor(state.persisted.theme.muted)
    if kept.isEmpty then surface.fillRect(railRect.x, railRect.y, railRect.width, railRect.height, ' ')
    else
      PixelRect
        .uncoveredWithin(RendererChromePlan.pixelRectOf(context.cellMetrics)(railRect), kept)
        .foreach(fillPixelRect(surface, _))

    val wordWrapEnabled = state.persisted.config.surfaceConfig.wordWrapEnabled
    val rowMetrics      = RendererPaneContent.textRowMetrics(railRect, context, snapshot)
    snapshot.visualLines.zipWithIndex.foreach {
      case (visualLine, rowIndex) if rowMetrics.lineFits(rowIndex) && rows.includes(rowIndex) =>
        val text =
          if shouldRenderLineNumberForVisualLine(visualLine, wordWrapEnabled) then
            val numberWidth = math.max(1, railRect.width - 1)
            (visualLine.bufferLine + 1).toString.reverse.padTo(numberWidth, ' ').reverse + " "
          else continuationIndicatorText(railRect.width)
        if RendererPaneSetup.usesMeasuredDrawing(snapshot, context) then
          surface.text.drawLabelPx(
            context.cellMetrics.toPixelX(railRect.x).toFloat,
            rowMetrics.lineTopPx(rowIndex),
            railRect.width * context.cellMetrics.charWidth.toFloat,
            snapshot.lineHeightPx,
            snapshot.ascentPx,
            text
          )
        else surface.putString(railRect.x, railRect.y + rowIndex, text)
      case _ => ()
    }

  /** Paint one line-number counter. The gutter/body divider sits on the counter's content-facing edge: the last column
    * for a left-placed counter, the first column for a right-placed one (`dividerOnLeft`).
    */
  private def renderCounterColumn(
    state: AppState,
    context: RenderContext,
    renderPlan: EditorPaneRenderPlan,
    lineRect: LayoutRect,
    dividerOnLeft: Boolean,
    rows: Repaint[Int]
  ): Unit =
    val surface    = context.surface
    val background = gutterBackground(state)
    val kept = state.persisted.layout.activeEditorPaneId
      .flatMap(renderPlan.snapshots.get)
      .toList
      .flatMap(keptRowBands(state, context, _, lineRect, rows))
    val toPixels = RendererChromePlan.pixelRectOf(context.cellMetrics)
    val repainted =
      if kept.isEmpty then Nil else PixelRect.uncoveredWithin(toPixels(lineRect), kept)

    surface.setBackgroundColor(background)
    surface.setForegroundColor(state.persisted.theme.muted)

    if kept.isEmpty then surface.fillRect(lineRect.x, lineRect.y, lineRect.width, lineRect.height, ' ')
    else repainted.foreach(fillPixelRect(surface, _))

    // Diagnostic markers share the divider's own column, so they are collected here and repainted afterwards -- once
    // the divider itself has been painted as one continuous stroke -- rather than drawn inline per row, which would
    // have the divider immediately paint back over them.
    val diagnosticRows = renderLineNumberRows(state, context, renderPlan, lineRect, surface, dividerOnLeft, rows)

    // The gutter/body boundary, painted once per frame as a single full-height fill (#1483) rather than implicitly by
    // each row's own background fill: a divider assembled that way is only as continuous as every row's fill agrees
    // with its neighbours, and a row with no content to anchor it (or a diagnostic marker briefly claiming the column,
    // below) leaves a gap -- reading as a dashed line rather than one continuous rule. Recolouring rather than
    // redrawing the text keeps every existing rendered string identical; only the boundary column's background changes.
    // Where rows are kept it is painted over the repainted rows and a little way into their neighbours: the ink of a
    // repainted label can spill a pixel into the band beside it, and only the divider, painted last, covers that part
    // of it. The neighbours' markers are drawn again below, as the divider paints over them.
    val dividerX = if dividerOnLeft then lineRect.x else lineRect.x + lineRect.width - 1
    surface.setBackgroundColor(state.persisted.theme.panelBorder)
    if kept.isEmpty then surface.fillRect(dividerX, lineRect.y, 1, lineRect.height, ' ')
    else
      val divider = toPixels(LayoutRect(dividerX, lineRect.y, 1, lineRect.height))
      repainted
        .flatMap(rect =>
          divider.intersection(rect.copy(yPx = rect.yPx - InkSpillPx, heightPx = rect.heightPx + 2 * InkSpillPx))
        )
        .foreach(fillPixelRect(surface, _))
    surface.setBackgroundColor(background)

    diagnosticRows.foreach(renderDiagnosticIndicator(surface, lineRect, _, state, dividerOnLeft))

  /** The bands of `lineRect` whose rows `rows` leaves alone, each reaching the inset past the counter on both sides: a
    * measured label is drawn shifted by that much and fills its background there.
    */
  private[renderer] def keptRowBands(
    state: AppState,
    context: RenderContext,
    snapshot: TextLayoutSnapshot,
    lineRect: LayoutRect,
    rows: Repaint[Int]
  ): List[PixelRect] =
    val reach = math.ceil(SurfaceTextInset.px(state.persisted.config)).toInt
    if rows.everything then Nil
    else
      counterRowBands(state, context, snapshot, lineRect).toList.zipWithIndex.collect {
        case (band, row) if !rows.includes(row) => band.copy(xPx = band.xPx - reach, widthPx = band.widthPx + 2 * reach)
      }

  /** The pixels each of `snapshot`'s rows owns in the counter: from where its number is drawn to where the next row's
    * is, so the ink a label spills into the gap beside it stays with it. The first row owns the counter above it, the
    * last the counter below.
    *
    * Where a number is drawn is the row's own band when the label is measured and its cell row on the grid when it is
    * not -- the two differ whenever the snapshot's line height is not the cell height.
    */
  private[renderer] def counterRowBands(
    state: AppState,
    context: RenderContext,
    snapshot: TextLayoutSnapshot,
    lineRect: LayoutRect
  ): Vector[PixelRect] =
    val measured =
      state.persisted.layout.activeEditorPaneId
        .flatMap(state.persisted.layout.editorPanes.get)
        .flatMap(_.bufferId)
        .flatMap(state.persisted.buffers.get)
        .exists(useMeasuredLineNumberFont(_, context)) &&
        context.surface.text.fontRenderContext.isDefined && RendererPaneSetup.usesMeasuredDrawing(snapshot, context)
    val bands =
      if measured then RendererPaneSetup.paneRowRects(lineRect, context, snapshot)
      else cellRowBands(context, snapshot, lineRect)
    val bounds = RendererChromePlan.pixelRectOf(context.cellMetrics)(lineRect)
    bands.indices.toVector.map { index =>
      val band   = bands(index)
      val top    = if index == 0 then bounds.yPx.min(band.yPx) else band.yPx
      val bottom = bands.lift(index + 1).fold(bounds.bottomPx.max(band.bottomPx))(_.yPx.max(band.bottomPx))
      band.copy(yPx = top, heightPx = (bottom - top).max(0))
    }

  private def cellRowBands(
    context: RenderContext,
    snapshot: TextLayoutSnapshot,
    lineRect: LayoutRect
  ): Vector[PixelRect] =
    val metrics = context.cellMetrics
    val cellRows = SurfaceFrameLayout
      .contentRowSlotsFor(lineRect, snapshot.visualLines.length, hasHeader = false, hasFooter = false)
      .collect { case SurfaceContentRowSlot(SurfaceContentRowKind.Item(index), rowY) => index -> rowY }
      .toMap
    snapshot.visualLines.indices.toVector.map { index =>
      val rowY   = cellRows.getOrElse(index, lineRect.bottom)
      val height = if cellRows.contains(index) then metrics.lineHeight else 0
      PixelRect(metrics.toPixelX(lineRect.x), metrics.toPixelY(rowY), lineRect.width * metrics.charWidth, height)
    }

  private def fillPixelRect(surface: RenderSurface, rect: PixelRect): Unit =
    surface.text.drawRunPx(rect.xPx.toFloat, rect.yPx, rect.widthPx.toFloat, rect.heightPx, 0, "")

  private def renderLineNumberRows(
    state: AppState,
    context: RenderContext,
    renderPlan: EditorPaneRenderPlan,
    lineRect: LayoutRect,
    surface: RenderSurface,
    dividerOnLeft: Boolean,
    rows: Repaint[Int]
  ): List[DiagnosticMarker] =
    state.persisted.layout.activeEditorPaneId
      .flatMap(state.persisted.layout.editorPanes.get)
      .toList
      .flatMap { pane =>
        val buffer = pane.bufferId.flatMap(state.persisted.buffers.get)
        val snapshot =
          state.persisted.layout.activeEditorPaneId
            .flatMap(renderPlan.snapshots.get)
            .orElse {
              for
                paneLayout <- state.persisted.layout.activeEditorPaneId.flatMap(renderPlan.paneLayouts.get)
                buf        <- buffer
              yield RendererPaneSetup.snapshotForBuffer(buf, paneLayout.contentRect, state, context)
            }
        val rowSlots =
          if dividerOnLeft then
            renderPlan.layoutContract.rightLineNumberRowSlots(snapshot.map(_.visualLines.length).getOrElse(0))
          else renderPlan.layoutContract.lineNumberRowSlots(snapshot.map(_.visualLines.length).getOrElse(0))
        snapshot.toList.flatMap { snapshot =>
          val rowMetrics   = RendererPaneContent.textRowMetrics(lineRect, context, snapshot)
          val measuredFont = measuredLineNumberFont(buffer, snapshot, context, lineRect)
          rowSlots.toList.flatMap {
            case SurfaceContentRowSlot(SurfaceContentRowKind.Item(index), rowY) if rowMetrics.lineFits(index) =>
              snapshot.visualLines.lift(index).toList.flatMap { visualLine =>
                renderLineNumberRow(
                  state,
                  context,
                  renderPlan,
                  lineRect,
                  surface,
                  pane,
                  measuredFont,
                  snapshot,
                  rowMetrics.lineTopPx(index),
                  visualLine,
                  rowY,
                  dividerOnLeft,
                  paintsNumber = rows.includes(index)
                )
              }
            case _ => Nil
          }
        }
      }

  private def renderLineNumberRow(
    state: AppState,
    context: RenderContext,
    renderPlan: EditorPaneRenderPlan,
    lineRect: LayoutRect,
    surface: RenderSurface,
    pane: EditorPane,
    measuredFont: Option[MeasuredLineNumberFont],
    snapshot: TextLayoutSnapshot,
    lineTopPx: Int,
    visualLine: TextVisualLine,
    rowY: Int,
    dividerOnLeft: Boolean,
    paintsNumber: Boolean
  ): List[DiagnosticMarker] =
    val rendersLineNumber =
      shouldRenderLineNumberForVisualLine(visualLine, state.persisted.config.surfaceConfig.wordWrapEnabled)
    val lineNumberText =
      if rendersLineNumber then
        val numberWidth  = math.max(1, lineRect.width - 1)
        val rightAligned = (visualLine.bufferLine + 1).toString.reverse.padTo(numberWidth, ' ').reverse
        // The single spacer cell falls on the divider's own column: trailing for a left counter, leading for a right
        // one, so digits always sit against the panel edge and the divider always against the content.
        if dividerOnLeft then " " + rightAligned else rightAligned + " "
      else continuationIndicatorText(lineRect.width)
    if paintsNumber then
      measuredFont match
        case Some(measured) =>
          surface.text.setFont(FontSpec.fromAwt(measured.font))
          // Issue #1542 (pixel-precision follow-up): nudge the digits a sub-cell amount away from the pane's outer edge
          // -- right for a left counter, left for a right one -- the same `SurfaceTextInset` value every other piece of
          // framed chrome already insets by. Purely a paint-time refinement: `LayoutEngine`'s cell grid (hit-testing,
          // drag-resize, the TUI's own rendering) is untouched.
          val insetPx       = SurfaceTextInset.px(state.persisted.config)
          val marginInsetPx = if dividerOnLeft then -insetPx else insetPx
          val run =
            if rendersLineNumber then
              val digits        = (visualLine.bufferLine + 1).toString
              val digitsWidthPx = measured.widthPx(digits)
              val startPx       = measuredDigitsStartPx(digitsWidthPx, lineRect, context, insetPx, dividerOnLeft)
              // The run is drawn inside the inset translation, so its anchor is given in the untranslated frame.
              MeasuredLineNumberRun((startPx - marginInsetPx).toFloat, digitsWidthPx, digits)
            else
              MeasuredLineNumberRun(
                context.cellMetrics.toPixelX(lineRect.x).toFloat,
                lineRect.width * context.cellMetrics.charWidth.toFloat,
                lineNumberText
              )
          surface.pixels.withPixelTranslation(marginInsetPx, 0.0) {
            surface.text.drawLabelPx(
              run.xPx,
              lineTopPx,
              run.widthPx,
              snapshot.lineHeightPx,
              snapshot.ascentPx,
              run.text
            )
          }
          surface.text.setFont(FontSpec.fromAwt(context.uiFont))
        case None => surface.putString(lineRect.x, rowY, lineNumberText)
    if rendersLineNumber then
      for
        bufferId   <- pane.bufferId.toList
        annotation <- renderPlan.annotations.get(bufferId).toList
        diagnostics = annotation.diagnosticsByLine.getOrElse(visualLine.bufferLine, Nil)
        if diagnostics.nonEmpty
      yield DiagnosticMarker(rowY, Option.when(measuredFont.isDefined)(lineTopPx), diagnostics)
    else Nil

  private def useMeasuredLineNumberFont(buffer: Buffer, context: RenderContext): Boolean =
    buffer.typographyRole != TypographyRole.Code && context.fontForBuffer(buffer) != context.codeFont

  final private case class MeasuredLineNumberFont(font: Font, fontRenderContext: FontRenderContext):
    def widthPx(text: String): Float = TextAlignment.measureTextWidth(text, font, fontRenderContext)

  final private case class MeasuredLineNumberRun(xPx: Float, widthPx: Float, text: String)

  /** The buffer's own font for a measured counter, shrunk just enough for the widest visible number to fit the digit
    * cells (#1979). `LayoutEngine` sizes the counter by digit count on the code-font cell grid it shares with the TUI,
    * and a proportional font's digits can be wider than a code cell -- unshrunk, the last digit runs under the divider.
    */
  private def measuredLineNumberFont(
    buffer: Option[Buffer],
    snapshot: TextLayoutSnapshot,
    context: RenderContext,
    lineRect: LayoutRect
  ): Option[MeasuredLineNumberFont] =
    for
      measuredBuffer    <- buffer.filter(useMeasuredLineNumberFont(_, context))
      fontRenderContext <- context.surface.text.fontRenderContext
      if RendererPaneSetup.usesMeasuredDrawing(snapshot, context)
    yield
      val bufferFont  = MeasuredLineNumberFont(context.fontForBuffer(measuredBuffer), fontRenderContext)
      val widestPx    = snapshot.visualLines.map(line => bufferFont.widthPx((line.bufferLine + 1).toString)).maxOption
      val digitAreaPx = math.max(1, lineRect.width - 1) * context.cellMetrics.charWidth.toFloat
      widestPx.filter(_ > digitAreaPx) match
        case Some(overflowingPx) =>
          bufferFont.copy(font = bufferFont.font.deriveFont(bufferFont.font.getSize2D * digitAreaPx / overflowingPx))
        case None => bufferFont

  /** Where a measured number's digits start on screen: flush against the divider for a left counter, and against the
    * outer edge less the margin inset for a right one, but never across the divider -- it is painted after the digits
    * and would cover any that overran it (#1979).
    */
  private def measuredDigitsStartPx(
    digitsWidthPx: Float,
    lineRect: LayoutRect,
    context: RenderContext,
    insetPx: Double,
    dividerOnLeft: Boolean
  ): Double =
    val cellWidthPx = context.cellMetrics.charWidth.toDouble
    val rectLeftPx  = context.cellMetrics.toPixelX(lineRect.x).toDouble
    val rectRightPx = context.cellMetrics.toPixelX(lineRect.right).toDouble
    if dividerOnLeft then math.max(rectRightPx - insetPx - digitsWidthPx, rectLeftPx + cellWidthPx)
    else rectRightPx - cellWidthPx - digitsWidthPx

  private def shouldRenderLineNumberForVisualLine(visualLine: TextVisualLine, wordWrapEnabled: Boolean): Boolean =
    !wordWrapEnabled || visualLine.startColumn == 0

  private def continuationIndicatorText(width: Int): String =
    val safeWidth = math.max(1, width)
    val leftWidth = (safeWidth - 1) / 2
    " " * leftWidth + "│" + " " * (safeWidth - leftWidth - 1)

  /** A row's diagnostic marker, and where its row is when the counter is measured: the marker sits on that row's own
    * band, not on the cell grid, which a measured row's band does not follow.
    */
  final private case class DiagnosticMarker(
      rowY: Int,
      lineTopPx: Option[Int],
      diagnostics: List[com.serenity.lsp.model.Diagnostic]
  )

  private def renderDiagnosticIndicator(
    surface: RenderSurface,
    lineRect: LayoutRect,
    marker: DiagnosticMarker,
    state: AppState,
    dividerOnLeft: Boolean
  ): Unit =
    if marker.diagnostics.nonEmpty then
      val worstCode = marker.diagnostics.flatMap(_.severity).map(_.code).minOption
      val color = worstCode match
        case Some(1) => state.persisted.theme.error.foreground
        case Some(2) => state.persisted.theme.warning.foreground
        case _       => state.persisted.theme.muted
      surface.setForegroundColor(color)
      surface.setBackgroundColor(gutterBackground(state))
      val markerX      = if dividerOnLeft then lineRect.x else lineRect.x + lineRect.width - 1
      def draw(): Unit = surface.putString(markerX, marker.rowY, "!")
      marker.lineTopPx.fold(draw())(top => surface.text.withLogicalPixelRow(marker.rowY, top)(draw()))

  private def gutterBackground(state: AppState): RenderColor =
    state.persisted.theme.panel.background

  def renderGutter(state: AppState, context: RenderContext, contract: EditorLayoutContract): Unit =
    contract.gutterRect.foreach { gutterRect =>
      context.surface.text.setFont(FontSpec.fromAwt(context.uiFont))
      val surface = context.surface
      val colors  = state.persisted.config.statusLine.colors

      surface.setBackgroundColor(colors.background.fold(gutterBackground(state))(RenderColor.fromAwt))
      surface.setForegroundColor(colors.foreground.fold(state.persisted.theme.panel.foreground)(RenderColor.fromAwt))
      surface.fillRect(gutterRect.x, gutterRect.y, gutterRect.width, gutterRect.height, ' ')

      val content = s" ${state.statusLineText.getOrElse(emptyWorkspaceStatus(state))} "
      val displayContent =
        if content.length > gutterRect.width then content.take(gutterRect.width - 3) + "..."
        else content

      drawUiTextInCellRect(surface, context, gutterRect, displayContent)
    }

  private def emptyWorkspaceStatus(state: AppState): String =
    state.persisted.layout.activeEditorPaneId.flatMap(state.persisted.layout.editorPanes.get) match
      case Some(pane) if pane.bufferId.flatMap(state.persisted.buffers.get).isEmpty => "No active buffer"
      case Some(_)                                                                  => ""
      case None                                                                     => "No active editor pane"

  private def drawUiTextInCellRect(
    surface: RenderSurface,
    context: RenderContext,
    rect: LayoutRect,
    text: String
  ): Unit =
    surface.text.fontRenderContext match
      case Some(frc) =>
        val rowHeightPx  = math.max(1, rect.height * context.cellMetrics.lineHeight)
        val lineHeightPx = math.max(1, math.min(context.uiMetrics.lineHeight, rowHeightPx - 2))
        val ascentPx     = math.max(1, math.min(context.uiMetrics.ascent, lineHeightPx))
        val placement = TextAlignment.placeLine(
          text = text,
          area = TextAreaPx(
            xPx = context.cellMetrics.toPixelX(rect.x).toFloat,
            yPx = context.cellMetrics.toPixelY(rect.y),
            widthPx = rect.width * context.cellMetrics.charWidth.toFloat,
            heightPx = rowHeightPx
          ),
          font = context.uiFont,
          lineHeightPx = lineHeightPx,
          ascentPx = ascentPx,
          horizontal = TextHorizontalAlignment.Left,
          vertical = TextVerticalAlignment.Middle,
          fontRenderContext = frc
        )

        surface.text.drawRunPx(
          xPx = placement.xPx,
          yPx = placement.yPx,
          bgWidthPx = placement.widthPx,
          lineHeightPx = placement.lineHeightPx,
          ascentPx = placement.ascentPx,
          s = text
        )
      case None =>
        val middleRow = rect.y + math.max(0, (rect.height - 1) / 2)
        CharacterRenderer.renderString(surface, rect.x, middleRow, text.take(math.max(0, rect.width)))
