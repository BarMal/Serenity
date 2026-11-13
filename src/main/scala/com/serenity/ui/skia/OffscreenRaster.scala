package com.serenity.ui.skia

import com.serenity.ui.layout.CellMetrics
import com.serenity.ui.renderer.{FontSpec, ScreenIdentity, StyledFontCache}
import org.jetbrains.skia.Surface

/** A Skia raster nothing presents, held for as long as the warm-up that draws on it, so the rest of the code never
  * names Skiko's surface type.
  */
final class OffscreenRaster private (raster: Surface):

  def isClosed: Boolean = raster.isClosed

  def close(): Unit =
    if !raster.isClosed then raster.close()

  /** A surface that draws on this raster and keeps what it drew, as the window's raster frames do. */
  def surface(
    metrics: CellMetrics,
    font: FontSpec,
    logicalWidthPx: Int,
    logicalHeightPx: Int,
    deviceScale: Double,
    caches: SkiaTextCaches,
    styledFonts: StyledFontCache,
    screen: ScreenIdentity
  ): SkiaRenderSurface =
    SkiaRenderSurface.retaining(
      raster,
      metrics,
      font,
      logicalWidthPx,
      logicalHeightPx,
      deviceScale,
      caches,
      styledFonts,
      screen = Some(screen)
    )

object OffscreenRaster:

  def make(widthPx: Int, heightPx: Int): OffscreenRaster =
    new OffscreenRaster(SkiaRasterSurfaceFactory.newSurface(widthPx, heightPx))
