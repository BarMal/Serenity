package com.serenity.ui.skia

import com.serenity.ui.layout.CellMetrics
import com.serenity.ui.renderer.{CaretCanvas, CaretCanvases, FontSpec, StyledFontCache, SurfaceGeometry}
import org.jetbrains.skia.Surface

/** The caret canvases of a window painted by Skia: the canvas's real logical geometry over a raster of one pixel that
  * nothing reads, freed when the recorder is flushed.
  */
object SkiaCaretCanvases extends CaretCanvases:

  def forGeometry(metrics: CellMetrics, font: FontSpec, geometry: SurfaceGeometry): CaretCanvas =
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
    CaretCanvas(surface, () => if !raster.isClosed then raster.close())
