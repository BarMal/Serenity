package com.serenity.ui.renderer

import com.serenity.state.models.*
import com.serenity.ui.layout.*

/** Which pixels a bounded repaint has to publish, once [[RendererFramePlanner]] has decided a frame may be bounded at
  * all. The frame redraws the dirty rows and whatever chrome its damage names ([[ChromeRepaint]]), and keeps the rest,
  * so these rects only decide which of its pixels reach the screen.
  */
private[renderer] object RendererRepaintRegion:

  /** Each dirty row's band, the line-number cells beside the active pane's dirty rows (a number changes only with its
    * row, so it repaints with it), the pinned status row when `damage` names [[Damage.Chrome]], and the header of each
    * pane a [[Damage.PaneChrome]] names, and the tab strip when `damage` names it -- coalesced into a few disjoint
    * rects, so a caret row and the status row far below it stay two narrow rects rather than one spanning every row
    * between them.
    */
  def rects(
    state: AppState,
    context: RenderContext,
    renderPlan: EditorPaneRenderPlan,
    panes: Map[PaneId, PaneFrameRecord],
    damage: Damage,
    dirtyRows: (PaneId, PaneFrameRecord) => Set[Int]
  ): List[PixelRect] =
    val contract = renderPlan.layoutContract
    val rowRects = panes.toList.flatMap {
      case (paneId, record) =>
        val lineNumberRows =
          if state.persisted.layout.activeEditorPaneId.contains(paneId) then
            (contract.lineNumberRect.toList ++ contract.rightLineNumberRect.toList)
              .map(RendererPaneSetup.paneRowRects(_, context, record.snapshot))
          else Nil
        val dirty = dirtyRows(paneId, record)
        dirty.toList.flatMap { row =>
          record.rowRects.lift(row).toList ++ lineNumberRows.flatMap(_.lift(row))
        } ++ columnRailRects(context, renderPlan, paneId, dirty)
    }
    val statusRow = contract.gutterRect.filter(_ => Damage.touchesChrome(damage))
    val headers   = Damage.paneChromeIds(damage).toList.flatMap(contract.paneHeaderRect)
    val tabBar    = contract.tabBarRect.filter(_ => Damage.touchesTabBar(damage))
    PixelRect.coalesced(
      rowRects ++ (statusRow.toList ++ headers ++ tabBar.toList)
        .map(RendererChromePlan.pixelRectOf(context.cellMetrics)),
      PixelRect.RepaintRectLimit
    )

  /** The rail bands of a multi-column pane's dirty rows, and the band under each column's last row. */
  private def columnRailRects(
    context: RenderContext,
    renderPlan: EditorPaneRenderPlan,
    paneId: PaneId,
    dirtyRows: Set[Int]
  ): List[PixelRect] =
    val columns = renderPlan.columnSnapshotsFor(paneId)
    for
      layout            <- renderPlan.paneLayouts.get(paneId).toList
      (placement, rows) <- columns.zip(ColumnPaneRows.localRows(columns, Some(dirtyRows)))
      if placement.gutterWidthCells > 0
      rail  = ColumnRails.railRect(layout.contentRect, placement)
      bands = ColumnRails.rowBands(context, rail, placement.snapshot)
      band <- rows.toList.flatMap(_.toList).flatMap(bands.lift) ++
        ColumnRails.unownedBands(context, rail, placement.snapshot)
    yield band
