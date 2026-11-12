package com.serenity.ui.skia

import com.serenity.ui.layout.CellMetrics
import com.serenity.ui.renderer.{FontSpec, RenderSurface, StyledFontCache, SurfaceGeometry}
import org.jetbrains.skia.Surface

/** The Skia surface a caret recorder measures against: the canvas's real logical geometry over a raster of one pixel
  * that nothing reads. [[release]] frees that raster.
  */
final class SkiaCaretCanvas private (val surface: RenderSurface, raster: Surface):

  def release(): Unit =
    if !raster.isClosed then raster.close()

object SkiaCaretCanvas:

  def forGeometry(metrics: CellMetrics, font: FontSpec, geometry: SurfaceGeometry): SkiaCaretCanvas =
    val raster = Surface.Companion.makeRasterN32Premul(1, 1)
    val surface = new SkiaRenderSurface(
      raster.getCanvas,
      metrics,
      font,
      () => (),
      geometry.logicalWidthPx,
      geometry.logicalHeightPx,
      geometry.deviceScale.x,
      geometry.deviceScale.y,
      retainedSurface = Some(raster),
      caches = SkiaTextCaches.fresh(),
      styledFonts = new StyledFontCache()
    )
    new SkiaCaretCanvas(surface, raster)
