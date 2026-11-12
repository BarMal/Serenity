package com.serenity.perf

import java.awt.Font
import java.awt.image.BufferedImage

import com.serenity.SkiaFrames
import com.serenity.state.manager.RenderCaches
import com.serenity.ui.layout.CellMetrics
import com.serenity.ui.renderer.{RenderSurface, ScreenIdentity, SurfaceSizing}
import com.serenity.ui.skia.SkiaTestFonts

/** An image a Skia surface persists into, with caches that resolve the fallback test font, which Skia's font manager
  * does not hold. The fixture also has to be registered with AWT, which measures the text, as the specs do.
  */
final class SkiaFallbackFrame(
    metrics: CellMetrics,
    font: Font,
    logicalWidthPx: Int,
    logicalHeightPx: Int,
    scale: Double
):
  val caches: RenderCaches = RenderCaches.create()

  require(
    SkiaTestFonts.install(caches.backendCaches, SkiaFallbackFrame.FontResource).isRight,
    s"the fallback test font ${SkiaFallbackFrame.FontResource} did not install"
  )

  private val frame = SkiaFrames.newFrame(
    SurfaceSizing.deviceImageDimension(logicalWidthPx, scale),
    SurfaceSizing.deviceImageDimension(logicalHeightPx, scale)
  )

  def surface(screen: ScreenIdentity): RenderSurface =
    SkiaFrames.surfaceOver(
      frame,
      metrics,
      font,
      logicalWidthPx,
      logicalHeightPx,
      scale,
      persists = true,
      caches,
      Some(screen)
    )

  def image: BufferedImage = frame.toImage

object SkiaFallbackFrame:
  val FontResource = "/fonts/SerenityFallbackTest.ttf"
