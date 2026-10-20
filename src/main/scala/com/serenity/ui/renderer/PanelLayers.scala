package com.serenity.ui.renderer

import com.serenity.state.manager.RenderCaches
import com.serenity.state.models.*

/** Paints each pinned/expanded/floating panel through a layer buffer cut to the panel's own bounds (#1100 stage 3,
  * #1869), and keeps those buffers in step with the panels on screen.
  */
private[renderer] object PanelLayers:

  /** Layer buffers exist only to be cached; with caching off, painting straight onto the frame is the same picture
    * without allocating a layer image per panel per frame (#1798).
    */
  def supportFor(state: AppState, context: RenderContext): Option[LayerBufferSupport] =
    context.surface.layerBuffers.filter(_ => state.persisted.config.surfaceConfig.layerCachingEnabled)

  /** Paint one panel's own content into an isolated layer buffer, reusing its last-painted pixels instead of repainting
    * when [[RendererFramePlanner.panelDirtyCheck]] says it's safe to -- the panel generalisation of the modal layer.
    *
    * The layer starts transparent, so everything the panel didn't paint stays transparent and compositing it back over
    * a frame whose content has since changed is still correct. `kept` says the frame already holds the panel's pixels
    * ([[PanelReuse.kept]] decided that when it planned), so there is nothing to paint or composite.
    *
    * No `layerBuffers` capability (TUI's `TerminalRenderSurface`), or layer caching switched off -> falls straight
    * through to `paintPanel`, painting directly into the shared surface every frame.
    */
  def paint(
    state: AppState,
    context: RenderContext,
    site: PanelSite,
    isDirty: Boolean,
    kept: Boolean
  )(paintPanel: RenderContext => Unit): Unit =
    supportFor(state, context) match
      case None => paintPanel(context)
      case Some(support) =>
        val surface = context.surface
        val cached  = context.caches.frameState.cachedPanelLayersFor(surface).get(site.id)
        val reusable = !isDirty && cached.exists(
          _.matches(surface.viewportWidth, surface.viewportHeight, context.cursorVisible, site.rect, site.bounds)
        )
        val key = surface.persistentContentKey
        if reusable && kept then ()
        else if reusable then
          cached.foreach(c => surface.pixels.compositeLayer(c.image, site.bounds))
          key.foreach(context.caches.frameState.panelCompositedOnto(surface, site.id, _))
        else
          val capture      = LayerCapture()
          val layerSurface = support.newBoundedLayerSurface(site.bounds, capture.onFlush, cached.map(_.image))
          paintPanel(context.copy(surface = layerSurface))
          layerSurface.flush()
          capture.image.foreach { image =>
            val newlyCached = CachedPanelLayer(
              image,
              surface.viewportWidth,
              surface.viewportHeight,
              context.cursorVisible,
              site.rect,
              Some(site.bounds),
              key.toSet
            )
            context.caches.frameState.rememberPanelLayer(surface, site.id, newlyCached)
            surface.pixels.compositeLayer(image, site.bounds)
          }

  /** Drop cached panel buffers for surfaces no longer on screen this frame, scoped to `surface`'s own entry -- a
    * dismissed panel's cache would otherwise sit in that inner `Map[SurfaceId, _]` forever ([[SurfaceId]] is a plain
    * value, not an object [[RendererFrameState]] can bound the lifetime of any other way).
    */
  def forgetStale(state: AppState, panels: FramePanels, surface: RenderSurface, caches: RenderCaches): Unit =
    if state.persisted.config.surfaceConfig.layerCachingEnabled then
      caches.frameState.pruneStalePanelLayers(surface, panels.activeIds(state))
    else caches.frameState.forgetLayerBuffers(surface)
