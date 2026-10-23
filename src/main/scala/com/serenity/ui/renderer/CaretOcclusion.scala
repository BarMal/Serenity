package com.serenity.ui.renderer

import java.awt.Rectangle

import com.serenity.ui.layout.{CellMetrics, PixelRect}
import com.serenity.ui.terminal.SwingWindow.CaretPaint

/** The Swing window fills the caret over the presented frame, above everything already in it, so a surface floating
  * over the text would have the caret drawn across it. The caret keeps only the pixels no such surface covers.
  */
object CaretOcclusion:

  def visible(carets: List[CaretPaint], obstructions: List[PixelRect]): List[CaretPaint] =
    if obstructions.isEmpty then carets
    else
      carets.flatMap { caret =>
        PixelRect
          .uncoveredWithin(pixelRectOf(caret.rect), obstructions)
          .map(piece => caret.copy(rect = new Rectangle(piece.xPx, piece.yPx, piece.widthPx, piece.heightPx)))
      }

  /** What the frame's layout puts above the text: the surfaces floating at, above and below the cursor, and the ones
    * expanded over the workspace.
    */
  def obstructions(renderPlan: EditorPaneRenderPlan, cellMetrics: CellMetrics): List[PixelRect] =
    val contract = renderPlan.layoutContract
    val floating = contract.aboveCursorOverlayRects ++ contract.belowCursorOverlayRects ++ contract.floatingOverlayRects
    (floating.map(_._2) ++ contract.expandedSurfaceRects.values).map(RendererChromePlan.pixelRectOf(cellMetrics))

  private def pixelRectOf(rect: Rectangle): PixelRect = PixelRect(rect.x, rect.y, rect.width, rect.height)
