package com.serenity.ui.skia

import java.util.concurrent.atomic.AtomicReference

import com.serenity.ui.layout.CellMetrics
import com.serenity.ui.renderer.{CaretCanvases, FontSpec, RenderSurface, StyledFontCache, SurfaceGeometry}
import org.jetbrains.skia.Surface

/** The caret canvases of one window painted by Skia: the canvas's real logical geometry over a raster of one pixel that
  * nothing reads, freed by Skiko when the canvas is replaced. A recorder is made for every cursor frame, and the
  * cursor-only frame is the cheapest the renderer draws, so the window keeps its canvas for as long as the geometry,
  * metrics and font it measures against do not change. Its frames are painted one at a time, so one canvas serves them.
  */
final class SkiaCaretCanvases extends CaretCanvases:

  final private case class Key(metrics: CellMetrics, font: FontSpec, geometry: SurfaceGeometry)

  private val kept = new AtomicReference(Option.empty[(Key, RenderSurface)])

  def forGeometry(metrics: CellMetrics, font: FontSpec, geometry: SurfaceGeometry): RenderSurface =
    val key = Key(metrics, font, geometry)
    kept.get.filter(_._1 == key).map(_._2).getOrElse {
      val made = make(metrics, font, geometry)
      kept.set(Some(key -> made))
      made
    }

  private def make(metrics: CellMetrics, font: FontSpec, geometry: SurfaceGeometry): RenderSurface =
    val raster = Surface.Companion.makeRasterN32Premul(1, 1)
    new SkiaRenderSurface(
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
