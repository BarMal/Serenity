package com.serenity.ui.renderer

import java.util.concurrent.atomic.AtomicInteger

import com.serenity.state.models.*
import com.serenity.ui.layout.*

/** A panel painted straight into a persisting surface: where it sat and what it showed. One is kept for each of a
  * surface's contents, since a surface alternating between two images holds the panel as it was on each image's last
  * frame.
  */
final case class PaintedPanel(
    viewportWidth: Int,
    viewportHeight: Int,
    cursorVisible: Boolean,
    frameRect: LayoutRect,
    bounds: PixelRect,
    content: Any
):

  def matches(
    viewportWidth: Int,
    viewportHeight: Int,
    cursorVisible: Boolean,
    frameRect: LayoutRect,
    layerBounds: PixelRect
  ): Boolean =
    this.viewportWidth == viewportWidth && this.viewportHeight == viewportHeight &&
      this.cursorVisible == cursorVisible && this.frameRect == frameRect && bounds == layerBounds

/** The panels a frame painted, per surface owner and per the surface contents each was painted into.
  *
  * Keyed by [[RenderSurface.layerCacheOwner]] rather than the surface itself: a GUI frame is a fresh surface every
  * time, so a surface key never hit. Still per owner, never one JVM-wide slot, so one window's records can't leak into
  * another's frame, and two surfaces that reuse a [[SurfaceId]] (unremarkable in tests) can't stomp on each other's.
  */
final private[renderer] class PaintedPanelRecords(cacheCapacity: AtomicInteger):

  private type Records = Map[(SurfaceId, SurfaceContentIdentity), PaintedPanel]

  private val records = new BoundedRefCache[ScreenIdentity, Records](cacheCapacity)

  def of(surface: RenderSurface): Records = records.get(surface.layerCacheOwner).getOrElse(Map.empty)

  def remember(surface: RenderSurface, surfaceId: SurfaceId, key: SurfaceContentIdentity, panel: PaintedPanel): Unit =
    val owner = surface.layerCacheOwner
    records.replaceAll(tracked =>
      tracked.updated(owner, tracked.getOrElse(owner, Map.empty).updated(surfaceId -> key, panel))
    )

  /** Drops the records of panels no longer on screen. */
  def prune(surface: RenderSurface, activeIds: Set[SurfaceId]): Unit =
    val owner = surface.layerCacheOwner
    records.replaceAll { tracked =>
      tracked
        .get(owner)
        .fold(tracked)(current =>
          tracked.updated(owner, current.filter { case ((id, _), _) => activeIds.contains(id) })
        )
    }

  /** `key`'s pixels no longer hold what these panels painted, because they were cleared or are about to be repainted.
    */
  def forget(key: SurfaceContentIdentity): Unit =
    records.replaceAll(tracked =>
      tracked.map((owner, panels) => owner -> panels.filterNot { case ((_, held), _) => held == key })
    )
