package com.serenity.ui.renderer

import com.serenity.state.models.*
import com.serenity.ui.layout.*

/** Which pixels a bounded repaint has to publish, once [[RendererFramePlanner]] has decided a frame may be bounded at
  * all. The frame itself is painted in full either way -- the chrome layer repaints every pixel these rects name each
  * frame -- so these rects only decide which of its pixels reach the screen.
  */
private[renderer] object RendererRepaintRegion:

  /** Each dirty row's band, the line-number cells beside the active pane's dirty rows (a number changes only with its
    * row, so it repaints with it), and the pinned status row when `damage` names [[Damage.Chrome]] -- coalesced into a
    * few disjoint rects, so a caret row and the status row far below it stay two narrow rects rather than one spanning
    * every row between them.
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
        dirtyRows(paneId, record).toList.flatMap { row =>
          record.rowRects.lift(row).toList ++ lineNumberRows.flatMap(_.lift(row))
        }
    }
    val statusRow = contract.gutterRect.filter(_ => Damage.touchesChrome(damage)).map(pixelRectOf(context.cellMetrics))
    PixelRect.coalesced(rowRects ++ statusRow.toList, PixelRect.RepaintRectLimit)

  private def pixelRectOf(cellMetrics: CellMetrics)(rect: LayoutRect): PixelRect =
    val leftPx = cellMetrics.toPixelX(rect.x)
    val topPx  = cellMetrics.toPixelY(rect.y)
    PixelRect(leftPx, topPx, cellMetrics.toPixelX(rect.right) - leftPx, cellMetrics.toPixelY(rect.bottom) - topPx)
