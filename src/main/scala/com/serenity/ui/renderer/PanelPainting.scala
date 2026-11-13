package com.serenity.ui.renderer

import com.serenity.state.manager.RenderCaches
import com.serenity.state.models.*

/** Paints each pinned, expanded or floating panel straight onto the frame, and records what it painted against the
  * pixels it went into, so a frame that keeps those pixels can leave the panel alone.
  */
private[renderer] object PanelPainting:

  /** Whether a frame can leave a panel as its surface already holds it: only when the surface keeps its own pixels from
    * frame to frame.
    */
  def canKeep(context: RenderContext): Boolean =
    context.surface.addressesPixels && context.surface.persistentContentKey.isDefined

  /** What tells [[PanelReuse.kept]] that the pixels `key` holds are this panel as the frame would paint it. */
  def holds(
    context: RenderContext,
    key: SurfaceContentIdentity,
    frame: PanelReuse.Frame
  ): PanelSite => Boolean =
    val painted = context.caches.frameState.paintedPanels.of(context.surface)
    site =>
      painted.get(site.id -> key).exists { panel =>
        panel.content == site.content &&
        panel.matches(frame.viewportWidth, frame.viewportHeight, frame.cursorVisible, site.rect, site.bounds)
      }

  /** A panel is painted into the frame, unless the frame keeps what its surface holds. What was painted is recorded
    * against the pixels it went into, with the content it showed: a surface that alternates between two images has a
    * record for each, and an image keeps the panel only while the content it holds is the content now.
    */
  def paint(context: RenderContext, site: PanelSite, kept: Boolean)(paintPanel: RenderContext => Unit): Unit =
    if !kept then
      paintPanel(context)
      val surface = context.surface
      surface.persistentContentKey.foreach { key =>
        context.caches.frameState.paintedPanels.remember(
          surface,
          site.id,
          key,
          PaintedPanel(
            surface.viewportWidth,
            surface.viewportHeight,
            context.cursorVisible,
            site.rect,
            site.bounds,
            site.content
          )
        )
      }

  /** Drops the records of panels no longer on screen this frame, scoped to `surface`'s own entry: a dismissed panel's
    * record would otherwise sit in its inner `Map[SurfaceId, _]` forever ([[SurfaceId]] is a plain value, not an object
    * [[RendererFrameState]] can bound the lifetime of any other way).
    */
  def forgetStale(state: AppState, panels: FramePanels, surface: RenderSurface, caches: RenderCaches): Unit =
    caches.frameState.paintedPanels.prune(surface, panels.activeIds(state))
