package com.serenity.perf

import java.awt.image.BufferedImage
import java.util.concurrent.atomic.AtomicInteger

import com.serenity.ui.renderer.Java2DRenderSurface
import com.serenity.ui.renderer.Java2DRenderSurface.DeviceScale
import com.serenity.ui.terminal.SwingWindow

/** Frames for the present benchmarks, shaped like the renderer's: device-size and opaque, taken from the window's own
  * two-image pool, and changed before every present.
  *
  * The change is what matters. Java2D keeps an image that is drawn unchanged as a cached X pixmap, so presenting the
  * same untouched frame again skips the upload every real frame pays and leaves only a server-side copy to time.
  */
final private[perf] class PresentedFrames(
    acquire: (Int, Int, Int) => BufferedImage,
    logicalWidthPx: Int,
    logicalHeightPx: Int,
    scale: DeviceScale
):
  private val widthPx   = Java2DRenderSurface.deviceImageDimension(logicalWidthPx, scale.x)
  private val heightPx  = Java2DRenderSurface.deviceImageDimension(logicalHeightPx, scale.y)
  private val presented = new AtomicInteger(0)

  def next(): BufferedImage =
    val frame = acquire(widthPx, heightPx, BufferedImage.TYPE_INT_RGB)
    frame.setRGB(0, 0, presented.incrementAndGet())
    frame

private[perf] object PresentedFrames:

  def forWindow(window: SwingWindow): PresentedFrames =
    val canvas = window.canvas
    PresentedFrames(
      window.acquireBaseImage,
      canvas.getWidth.max(1),
      canvas.getHeight.max(1),
      Java2DRenderSurface.deviceScaleFor(canvas)
    )
