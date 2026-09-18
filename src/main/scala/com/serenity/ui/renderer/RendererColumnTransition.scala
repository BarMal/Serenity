package com.serenity.ui.renderer

import com.serenity.animation.TransitionDirection
import com.serenity.state.models.*
import com.serenity.ui.layout.*

/** Column-based document layout (issue #1338, Phase 1 animation): paints the OUTGOING column's plain text into the
  * sliver of the pane the sweep hasn't yet covered with the INCOMING column's content -- which the ordinary paint path
  * ([[RendererPaneContent.renderEditorPane]]) has already drawn, in full, underneath this, before this runs.
  *
  * Deliberately plain text only: no styled segments, semantic tokens, comment/diagnostic highlights, selection, or
  * per-character animations. Those all describe the buffer's CURRENT edit state, which the receding column is not -- it
  * is a still frame of the content right before the move -- and painting all of that for a transition lasting a couple
  * of animation frames would add a lot of machinery for a difference nobody would see.
  *
  * Direction mapping (a scope decision, not implied by anything upstream -- see the Phase 1 plan's note that either
  * reading is defensible): `RightToLeft` ([[com.serenity.state.manager.CursorViewport]] seeds it for `ColumnRight`,
  * paging forward) clips the OUTGOING column's right edge, shrinking as `progress` grows, so the page reads as sliding
  * off toward the left while the incoming page (already fully painted underneath) shows through on the right.
  * `LeftToRight` (`ColumnLeft`, paging backward) mirrors it, clipping the left edge instead. Either way only the
  * outgoing column ever needs clipping -- the incoming one needs none, since the ordinary path already painted it for
  * the whole pane before this runs.
  */
object RendererColumnTransition:

  def render(
    buffer: Buffer,
    contentRect: LayoutRect,
    state: AppState,
    context: RenderContext,
    activeSnapshot: TextLayoutSnapshot,
    transition: ColumnTransitionState
  ): Unit =
    val outgoingSnapshot = RendererPaneSetup.snapshotForBufferColumnAt(
      buffer,
      transition.previousTopLine,
      transition.previousTopVisualLine,
      contentRect,
      state,
      context
    )
    val measured = RendererPaneSetup.usesMeasuredDrawing(outgoingSnapshot, context)
    val panelWidthPx =
      if measured then activeSnapshot.panelWidthPx.toFloat
      else contentRect.width.toFloat * context.cellMetrics.charWidth.toFloat
    val sweptPx = (panelWidthPx * transition.progress.toFloat).max(0.0f).min(panelWidthPx)
    val (clipMinPx, clipMaxPx) = transition.direction match
      case TransitionDirection.RightToLeft => (0.0f, panelWidthPx - sweptPx)
      case TransitionDirection.LeftToRight => (sweptPx, panelWidthPx)
      // A column transition is only ever seeded with one of the two cases above (see `CursorViewport
      // .seedColumnTransition`) -- `TransitionDirection`'s other cases exist for unrelated transitions
      // (`ElementTransitionPlanner`'s ordered reveals) and are not reachable here.
      case _ => (0.0f, panelWidthPx)

    if clipMaxPx <= clipMinPx then ()
    else
      val theme = state.persisted.theme
      context.surface.setForegroundColor(theme.foreground)
      context.surface.setBackgroundColor(theme.background)
      if measured then paintMeasured(outgoingSnapshot, contentRect, context, clipMinPx, clipMaxPx)
      else paintCells(outgoingSnapshot, contentRect, context, clipMinPx, clipMaxPx)

  private def paintMeasured(
    snapshot: TextLayoutSnapshot,
    contentRect: LayoutRect,
    context: RenderContext,
    clipMinPx: Float,
    clipMaxPx: Float
  ): Unit =
    val xOriginPx  = context.cellMetrics.toPixelX(contentRect.x).toFloat
    val localMinPx = clipMinPx - xOriginPx
    val localMaxPx = clipMaxPx - xOriginPx
    snapshot.visualLines.zipWithIndex.foreach {
      case (visualLine, row) =>
        if RendererPaneContent.visualLineFits(contentRect, row, context, snapshot) &&
            RendererPaneContent.visualLineVisible(contentRect, row, context, snapshot)
        then
          visualLine.visibleSlice(localMinPx, localMaxPx).foreach {
            case (text, localStartXPx, widthPx) =>
              if text.nonEmpty && widthPx > 0.0f then
                val yPx      = RendererPaneContent.visualLineTopPx(contentRect, row, context, snapshot)
                val heightPx = RendererPaneContent.rowHeightPxFor(visualLine, snapshot)
                val ascentPx = RendererPaneContent.rowAscentPxFor(visualLine, snapshot)
                context.surface.text.drawRunPx(xOriginPx + localStartXPx, yPx, widthPx, heightPx, ascentPx, text)
          }
    }

  private def paintCells(
    snapshot: TextLayoutSnapshot,
    contentRect: LayoutRect,
    context: RenderContext,
    clipMinPx: Float,
    clipMaxPx: Float
  ): Unit =
    val charWidthPx = math.max(1, context.cellMetrics.charWidth).toFloat
    val xOriginPx   = context.cellMetrics.toPixelX(contentRect.x).toFloat
    val localMinPx  = clipMinPx - xOriginPx
    val localMaxPx  = clipMaxPx - xOriginPx
    snapshot.visualLines.zipWithIndex.foreach {
      case (visualLine, row) =>
        if RendererPaneContent.visualLineFits(contentRect, row, context, snapshot) &&
            RendererPaneContent.visualLineVisible(contentRect, row, context, snapshot)
        then
          visualLine.visibleSlice(localMinPx, localMaxPx).foreach {
            case (text, localStartXPx, _) =>
              if text.nonEmpty then
                val screenY = contentRect.y + row
                val screenX = contentRect.x + math.round(localStartXPx / charWidthPx)
                if screenY >= 0 && screenX >= contentRect.x && screenX < contentRect.right then
                  context.surface.putString(screenX, screenY, text)
          }
    }
