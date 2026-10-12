package com.serenity.ui.renderer

import com.serenity.state.models.*
import com.serenity.ui.color.RenderColor
import com.serenity.ui.layout.*

/** A pane's scrollbar as it is painted: the strip it lies in, the thumb within it, and the thumb's colour. Two visuals
  * being equal is what lets a screen keep the bar it already shows.
  */
final case class ScrollbarVisual(strip: PixelRect, thumb: PixelRect, color: RenderColor)

/** Paints each editor pane's scrollbar over the right edge of its text ([[ScrollbarLayout]]).
  *
  * The bar is an overlay, so the rows never know it is there: a frame puts back the pixels the last one covered
  * ([[putBack]]) before it draws, and covers them again once every row, chrome and panel is in place ([[paint]]). What
  * a frame builds on is therefore always the text alone, and the bar costs one small lift and fill however little or
  * much else the frame repaints.
  */
private[renderer] object RendererScrollbars:

  /** The bars of the panes whose strip no overlay or panel covers; a modal covers every one. */
  def visualsFor(
    state: AppState,
    context: RenderContext,
    renderPlan: EditorPaneRenderPlan,
    panels: FramePanels
  ): Map[PaneId, ScrollbarVisual] =
    if state.runtime.modalStack.nonEmpty || renderPlan.scrolls.isEmpty then Map.empty
    else
      val colors      = state.persisted.theme.scrollbar
      val obstruction = obstructions(context, renderPlan, panels)
      for
        (paneId, extent) <- renderPlan.scrolls
        layout           <- renderPlan.paneLayouts.get(paneId).toList
        strip = ScrollbarLayout.strip(layout.contentRect, context.cellMetrics)
        if !obstruction.exists(_.intersects(strip))
        thumb <- ScrollbarGeometry.thumb(extent, strip.heightPx).toList
        held = state.runtime.pointerGesture.scrollbarGrab.exists(_.paneId == paneId)
      yield paneId -> ScrollbarVisual(
        strip,
        PixelRect(strip.xPx, strip.yPx + thumb.offsetPx, strip.widthPx, thumb.lengthPx),
        if held then colors.thumbHeld else colors.thumb
      )

  /** The panes whose bar differs between `current` and `shown`, which includes one that has just lost its thumb. */
  def changed(current: Map[PaneId, ScrollbarVisual], shown: Map[PaneId, ScrollbarVisual]): Set[PaneId] =
    (current.keySet ++ shown.keySet).filter(paneId => current.get(paneId) != shown.get(paneId))

  /** The strip of each of `paneIds` that has a bar to show. */
  def strips(context: RenderContext, renderPlan: EditorPaneRenderPlan, paneIds: Set[PaneId]): List[PixelRect] =
    paneIds.toList
      .flatMap(renderPlan.paneLayouts.get)
      .map(layout => ScrollbarLayout.strip(layout.contentRect, context.cellMetrics))

  /** The thumb is held in bands, so a frame that repaints a few rows beneath a tall thumb takes off and redraws only
    * the bands those rows reach.
    */
  private val BandHeightPx: Int = 32

  /** One band of a thumb on the buffer, and the pixels it covers. */
  final case class Band(rect: PixelRect, beneath: LiftedPixels)

  /** The bars a pixel buffer still holds from earlier frames. */
  final case class Held(visual: ScrollbarVisual, bands: Vector[Band])

  /** A held bar as a frame finds it once [[release]] has taken off what the frame reaches: `intact` bands still on the
    * buffer, and the `redraw` bands to cover again, with the storage their pixels were lifted into. `sameBar` says the
    * bar is drawn as before; if not, the storage in `spare` is all that is left of it.
    */
  final case class Settled(
      sameBar: Boolean,
      intact: Vector[Band],
      redraw: Vector[(PixelRect, LiftedPixels)],
      spare: Vector[LiftedPixels]
  )

  /** Settles the bars the buffer holds before the frame builds on it. A band stays where it is while its bar would be
    * drawn unchanged and nothing the frame clears, copies or repaints reaches it; any other is taken off, giving the
    * pixels it covered back, so that what the frame builds on is the text alone.
    */
  def release(
    context: RenderContext,
    visuals: Map[PaneId, ScrollbarVisual],
    plan: Option[FramePlan]
  ): Map[PaneId, Settled] =
    val held    = context.surface.persistentContentKey.fold(Map.empty[PaneId, Held])(context.caches.frameState.heldBars)
    val lifting = context.surface.pixelLifting
    held.map { (paneId, bar) =>
      val sameBar = visuals.get(paneId).contains(bar.visual)
      val (intact, off) =
        if sameBar then bar.bands.partition(band => plan.exists(untouched(band.rect, _)))
        else (Vector.empty[Band], bar.bands)
      lifting.foreach(support => off.foreach(band => support.putBack(band.beneath)))
      paneId -> Settled(sameBar, intact, off.map(band => band.rect -> band.beneath), off.map(_.beneath))
    }

  /** Draws the bars over what the frame has painted, covering again only what `settled` did not leave intact. A surface
    * that keeps its pixels from frame to frame draws them only if it can lift what lies beneath, as it must put it
    * back.
    */
  def paint(context: RenderContext, visuals: Map[PaneId, ScrollbarVisual], settled: Map[PaneId, Settled]): Unit =
    val persistentKey = context.surface.persistentContentKey
    val lifting       = context.surface.pixelLifting
    if persistentKey.isEmpty then visuals.values.foreach(visual => fill(context, visual.thumb, visual))
    else
      lifting.foreach { support =>
        val held = visuals.map { (paneId, visual) =>
          val bands = settled.get(paneId) match
            case Some(found) if found.sameBar =>
              found.intact ++ found.redraw.map((rect, spare) => cover(context, support, rect, visual, Some(spare)))
            case other =>
              bandsOf(visual.thumb, banded(context)).zipWithIndex.map((rect, index) =>
                cover(context, support, rect, visual, other.flatMap(_.spare.lift(index)))
              )
          paneId -> Held(visual, bands)
        }
        persistentKey.foreach(context.caches.frameState.rememberHeldBars(_, held))
      }

  /** Bands share edges only where a device pixel is a whole number of logical ones; elsewhere a thumb is a single band.
    */
  private def banded(context: RenderContext): Boolean =
    LabelAnchor.isWhole(context.surface.devicePixelScaleX) && LabelAnchor.isWhole(context.surface.devicePixelScaleY)

  private def bandsOf(thumb: PixelRect, banded: Boolean): Vector[PixelRect] =
    if !banded then Vector(thumb)
    else
      (thumb.yPx until thumb.bottomPx by BandHeightPx).toVector.map(top =>
        PixelRect(thumb.xPx, top, thumb.widthPx, BandHeightPx.min(thumb.bottomPx - top))
      )

  private def cover(
    context: RenderContext,
    support: PixelLifting,
    rect: PixelRect,
    visual: ScrollbarVisual,
    spare: Option[LiftedPixels]
  ): Band =
    val beneath = support.lift(rect, spare)
    fill(context, rect, visual)
    Band(rect, beneath)

  private def fill(context: RenderContext, rect: PixelRect, visual: ScrollbarVisual): Unit =
    context.surface.pixels.fillPixelRect(rect.xPx, rect.yPx, rect.widthPx, rect.heightPx, visual.color)

  /** Whether nothing the frame does reaches `thumb`: no pixels copied, none cleared or repainted beneath it. */
  private def untouched(thumb: PixelRect, plan: FramePlan): Boolean =
    plan.blits.isEmpty && PixelRect.uncoveredWithin(thumb, plan.preserved).isEmpty

  private def obstructions(
    context: RenderContext,
    renderPlan: EditorPaneRenderPlan,
    panels: FramePanels
  ): List[PixelRect] =
    val contract = renderPlan.layoutContract
    val overlays =
      (contract.aboveCursorOverlayRects ++ contract.belowCursorOverlayRects ++ contract.floatingOverlayRects)
        .map(_._2) ++ contract.expandedSurfaceRects.values
    overlays.map(RendererChromePlan.pixelRectOf(context.cellMetrics)) ++ panels.sites.values.map(_.painted)
