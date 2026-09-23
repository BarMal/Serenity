package com.serenity.ui.renderer

import com.serenity.state.models.*
import com.serenity.ui.layout.*

/** Paints the line-number column and the pinned status row (`AppState.statusLineText`), and the small chrome-text
  * helpers those two share.
  */
object RendererGutter:

  def renderLineNumbers(state: AppState, context: RenderContext, renderPlan: EditorPaneRenderPlan): Unit =
    if state.persisted.config.surfaceConfig.showLineNumbers then
      // Multi-column e-reader layout (issue #1338, Phase 2 / slice 2): a column-mode page has no single shared
      // pane-level rail (`LayoutEngine` reserves none in that case); each column carries its own rail on its left edge,
      // painted inline against that column's own chunk of rows.
      val columnModeActive =
        state.persisted.config.surfaceConfig.columnModeEnabled && state.persisted.config.surfaceConfig.wordWrapEnabled
      if columnModeActive then renderPerColumnRails(state, context, renderPlan)
      else
        context.surface.text.setFont(context.uiFont)
        renderPlan.layoutContract.lineNumberRect.foreach(rect =>
          renderCounterColumn(state, context, renderPlan, rect, dividerOnLeft = false)
        )
        renderPlan.layoutContract.rightLineNumberRect.foreach(rect =>
          renderCounterColumn(state, context, renderPlan, rect, dividerOnLeft = true)
        )

  /** Paint each e-reader column's own line-number rail (slice 2). Every pane carrying column placements draws a rail on
    * the leftmost `gutterWidthCells` of each column's band, aligned row-for-row against that column's own snapshot -- a
    * buffer line number on the row a buffer line starts, a continuation indicator on a wrapped row -- so the rails read
    * exactly like the single-column gutter, one per column.
    */
  private def renderPerColumnRails(
    state: AppState,
    context: RenderContext,
    renderPlan: EditorPaneRenderPlan
  ): Unit =
    context.surface.text.setFont(context.uiFont)
    state.persisted.layout.editorPanes.foreach {
      case (paneId, _) =>
        renderPlan.paneLayouts.get(paneId).foreach { paneLayout =>
          renderPlan
            .columnSnapshotsFor(paneId)
            .filter(_.gutterWidthCells > 0)
            .foreach(placement => renderColumnRail(state, context, paneLayout.contentRect, placement))
        }
    }

  private def renderColumnRail(
    state: AppState,
    context: RenderContext,
    contentRect: LayoutRect,
    placement: ColumnSnapshotPlacement
  ): Unit =
    val surface  = context.surface
    val railX    = contentRect.x + placement.xOffsetCells
    val railRect = LayoutRect(railX, contentRect.y, placement.gutterWidthCells, contentRect.height)
    val snapshot = placement.snapshot

    surface.setBackgroundColor(state.persisted.theme.panel.background)
    surface.setForegroundColor(state.persisted.theme.muted)
    surface.fillRect(railRect.x, railRect.y, railRect.width, railRect.height, ' ')

    val wordWrapEnabled = state.persisted.config.surfaceConfig.wordWrapEnabled
    snapshot.visualLines.zipWithIndex.foreach {
      case (visualLine, rowIndex) if RendererPaneContent.visualLineFits(railRect, rowIndex, context, snapshot) =>
        val text =
          if shouldRenderLineNumberForVisualLine(visualLine, wordWrapEnabled) then
            val numberWidth = math.max(1, railRect.width - 1)
            (visualLine.bufferLine + 1).toString.reverse.padTo(numberWidth, ' ').reverse + " "
          else continuationIndicatorText(railRect.width)
        if RendererPaneSetup.usesMeasuredDrawing(snapshot, context) then
          surface.text.drawRunPx(
            context.cellMetrics.toPixelX(railRect.x).toFloat,
            RendererPaneContent.visualLineTopPx(railRect, rowIndex, context, snapshot),
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
    dividerOnLeft: Boolean
  ): Unit =
    val surface = context.surface

    surface.setBackgroundColor(state.persisted.theme.panel.background)
    surface.setForegroundColor(state.persisted.theme.muted)

    surface.fillRect(lineRect.x, lineRect.y, lineRect.width, lineRect.height, ' ')

    // Diagnostic markers share the divider's own column, so they are collected here and repainted afterwards -- once
    // the divider itself has been painted as one continuous stroke -- rather than drawn inline per row, which would
    // have the divider immediately paint back over them.
    val diagnosticRows = renderLineNumberRows(state, context, renderPlan, lineRect, surface, dividerOnLeft)

    // The gutter/body boundary, painted once per frame as a single full-height fill (#1483) rather than implicitly by
    // each row's own background fill: a divider assembled that way is only as continuous as every row's fill agrees
    // with its neighbours, and a row with no content to anchor it (or a diagnostic marker briefly claiming the column,
    // below) leaves a gap -- reading as a dashed line rather than one continuous rule. Recolouring rather than
    // redrawing the text keeps every existing rendered string identical; only the boundary column's background changes.
    val dividerX = if dividerOnLeft then lineRect.x else lineRect.x + lineRect.width - 1
    surface.setBackgroundColor(state.persisted.theme.panelBorder)
    surface.fillRect(dividerX, lineRect.y, 1, lineRect.height, ' ')
    surface.setBackgroundColor(state.persisted.theme.panel.background)

    diagnosticRows.foreach {
      case (rowY, diagnostics) =>
        renderDiagnosticIndicator(surface, lineRect, rowY, diagnostics, state, dividerOnLeft)
    }

  private def renderLineNumberRows(
    state: AppState,
    context: RenderContext,
    renderPlan: EditorPaneRenderPlan,
    lineRect: LayoutRect,
    surface: RenderSurface,
    dividerOnLeft: Boolean
  ): List[(Int, List[com.serenity.lsp.model.Diagnostic])] =
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
          rowSlots.toList.flatMap {
            case SurfaceContentRowSlot(SurfaceContentRowKind.Item(index), rowY)
                if RendererPaneContent.visualLineFits(lineRect, index, context, snapshot) =>
              snapshot.visualLines.lift(index).toList.flatMap { visualLine =>
                renderLineNumberRow(
                  state,
                  context,
                  renderPlan,
                  lineRect,
                  surface,
                  pane,
                  buffer,
                  snapshot,
                  index,
                  visualLine,
                  rowY,
                  dividerOnLeft
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
    buffer: Option[Buffer],
    snapshot: TextLayoutSnapshot,
    index: Int,
    visualLine: TextVisualLine,
    rowY: Int,
    dividerOnLeft: Boolean
  ): List[(Int, List[com.serenity.lsp.model.Diagnostic])] =
    val lineTopPx = RendererPaneContent.visualLineTopPx(lineRect, index, context, snapshot)
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
    val measuredLineNumberFont = buffer.filter(useMeasuredLineNumberFont(_, context))
    if RendererPaneSetup.usesMeasuredDrawing(snapshot, context) && measuredLineNumberFont.nonEmpty then
      measuredLineNumberFont.foreach(buf => surface.text.setFont(context.fontForBuffer(buf)))
      // Issue #1542 (pixel-precision follow-up): nudge the digits a sub-cell amount away from the pane's outer edge --
      // right for a left counter, left for a right one -- the same `SurfaceTextInset` value every other piece of framed
      // chrome already insets by. Purely a paint-time refinement, entirely inside the whole cell `lineRect` already
      // reserves: `LayoutEngine`'s cell grid (hit-testing, drag-resize, the TUI's own rendering) is untouched.
      val insetPx       = SurfaceTextInset.px(state.persisted.config)
      val marginInsetPx = if dividerOnLeft then -insetPx else insetPx
      surface.pixels.withPixelTranslation(marginInsetPx, 0.0) {
        surface.text.drawRunPx(
          context.cellMetrics.toPixelX(lineRect.x).toFloat,
          lineTopPx,
          lineRect.width * context.cellMetrics.charWidth.toFloat,
          snapshot.lineHeightPx,
          snapshot.ascentPx,
          lineNumberText
        )
      }
      surface.text.setFont(context.uiFont)
    else surface.putString(lineRect.x, rowY, lineNumberText)
    if rendersLineNumber then
      for
        bufferId   <- pane.bufferId.toList
        annotation <- renderPlan.annotations.get(bufferId).toList
        diagnostics = annotation.diagnosticsByLine.getOrElse(visualLine.bufferLine, Nil)
        if diagnostics.nonEmpty
      yield rowY -> diagnostics
    else Nil

  private def useMeasuredLineNumberFont(buffer: Buffer, context: RenderContext): Boolean =
    buffer.typographyRole != TypographyRole.Code && context.fontForBuffer(buffer) != context.codeFont

  private def shouldRenderLineNumberForVisualLine(visualLine: TextVisualLine, wordWrapEnabled: Boolean): Boolean =
    !wordWrapEnabled || visualLine.startColumn == 0

  private def continuationIndicatorText(width: Int): String =
    val safeWidth = math.max(1, width)
    val leftWidth = (safeWidth - 1) / 2
    " " * leftWidth + "│" + " " * (safeWidth - leftWidth - 1)

  private def renderDiagnosticIndicator(
    surface: RenderSurface,
    lineRect: LayoutRect,
    screenY: Int,
    lineDiags: List[com.serenity.lsp.model.Diagnostic],
    state: AppState,
    dividerOnLeft: Boolean
  ): Unit =
    if lineDiags.nonEmpty then
      val worstCode = lineDiags.flatMap(_.severity).map(_.code).minOption
      val color = worstCode match
        case Some(1) => state.persisted.theme.error.foreground
        case Some(2) => state.persisted.theme.warning.foreground
        case _       => state.persisted.theme.muted
      surface.setForegroundColor(color)
      surface.setBackgroundColor(state.persisted.theme.panel.background)
      val markerX = if dividerOnLeft then lineRect.x else lineRect.x + lineRect.width - 1
      surface.putString(markerX, screenY, "!")

  def renderGutter(state: AppState, context: RenderContext, contract: EditorLayoutContract): Unit =
    contract.gutterRect.foreach { gutterRect =>
      context.surface.text.setFont(context.uiFont)
      val surface = context.surface
      val colors  = state.persisted.config.statusLine.colors

      surface.setBackgroundColor(colors.backgroundOr(state.persisted.theme.panel.background))
      surface.setForegroundColor(colors.foregroundOr(state.persisted.theme.panel.foreground))
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
