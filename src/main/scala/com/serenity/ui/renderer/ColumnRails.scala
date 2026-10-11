package com.serenity.ui.renderer

import com.serenity.ui.layout.*

/** The pixels of a multi-column pane's line-number rails, which a frame repaints a row band at a time.
  *
  * A row owns its rail band from where its label starts to where the next row's does, so the ink a label spills into
  * the gap beside it stays with it. The band under the last row is owned by no row and is repainted every frame: it is
  * the one place a column that gained or lost a row changes without a row of it being dirty.
  */
private[renderer] object ColumnRails:

  def railRect(contentRect: LayoutRect, placement: ColumnSnapshotPlacement): LayoutRect =
    LayoutRect(
      contentRect.x + placement.xOffsetCells,
      contentRect.y,
      placement.gutterWidthCells,
      contentRect.height
    )

  def rowBands(context: RenderContext, rail: LayoutRect, snapshot: TextLayoutSnapshot): Vector[PixelRect] =
    val rows = RendererPaneSetup.paneRowRects(rail, context, snapshot)
    val top  = RendererChromePlan.pixelRectOf(context.cellMetrics)(rail).yPx
    rows.indices.toVector.map { index =>
      val row   = rows(index)
      val from  = if index == 0 then top.min(row.yPx) else row.yPx
      val until = rows.lift(index + 1).fold(row.bottomPx)(_.yPx.max(row.bottomPx))
      row.copy(yPx = from, heightPx = (until - from).max(0))
    }

  /** The bands of the rows `rows` leaves alone. */
  def keptBands(
    context: RenderContext,
    rail: LayoutRect,
    snapshot: TextLayoutSnapshot,
    rows: Repaint[Int]
  ): List[PixelRect] =
    if rows.everything then Nil
    else
      rowBands(context, rail, snapshot).toList.zipWithIndex.collect { case (band, row) if !rows.includes(row) => band }

  /** The rail pixels no row owns. */
  def unownedBands(context: RenderContext, rail: LayoutRect, snapshot: TextLayoutSnapshot): List[PixelRect] =
    PixelRect.uncoveredWithin(
      RendererChromePlan.pixelRectOf(context.cellMetrics)(rail),
      rowBands(context, rail, snapshot).toList
    )
