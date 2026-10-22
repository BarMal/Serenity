package com.serenity.ui.skia

import java.awt.image.BufferedImage

import com.serenity.ui.renderer.{
  BackendCacheKey,
  BackendCaches,
  DeviceArea,
  FontSpec,
  FrameSurfaceFactory,
  FrameSurfaceRequest,
  Java2DRenderSurface,
  RenderSurface,
  ScreenIdentity
}
import org.jetbrains.skia.Surface

/** Paints a window's frames with Skia's CPU rasteriser and hands the Swing canvas an ordinary image to present, so the
  * window keeps its Java2D canvas for input, input methods and accessibility.
  *
  * The owner's [[BackendCaches]] hold the raster surface, which is how a frame starts from the pixels the last one left.
  */
object SkiaRasterSurfaceFactory extends FrameSurfaceFactory:

  private val FramesKey = BackendCacheKey(classOf[RasterFrames])

  def create(request: FrameSurfaceRequest): RenderSurface =
    val canvas        = request.canvas
    val logicalWidth  = Java2DRenderSurface.logicalCanvasDimension(canvas.getWidth, canvas.getPreferredSize.width)
    val logicalHeight = Java2DRenderSurface.logicalCanvasDimension(canvas.getHeight, canvas.getPreferredSize.height)
    val scale         = Java2DRenderSurface.deviceScaleFor(canvas)
    val widthPx       = Java2DRenderSurface.deviceImageDimension(logicalWidth, scale.x)
    val heightPx      = Java2DRenderSurface.deviceImageDimension(logicalHeight, scale.y)
    val image         = request.acquireImage(widthPx, heightPx, BufferedImage.TYPE_INT_RGB)
    val frames        = request.backendCaches.getOrCreate(FramesKey)(new RasterFrames)
    val surface =
      if request.contentPersists then frames.surfaceFor(widthPx, heightPx, scale.x)
      else Surface.Companion.makeRasterN32Premul(widthPx, heightPx)
    SkiaRenderSurface.retaining(
      surface,
      request.metrics,
      FontSpec.fromAwt(request.font),
      logicalWidth,
      logicalHeight,
      scale.x,
      SkiaTextCaches.of(request.backendCaches),
      request.textCaches.styledFonts,
      onFlush = () =>
        if request.contentPersists then
          val damage = request.changedRegion().map(_.map(DeviceArea.around(_, scale.x, scale.y, widthPx, heightPx)))
          frames.present(surface, image, damage)
        else
          RasterFrames.copyWhole(surface, image)
          surface.close()
        request.onFlush(image)
      ,
      layerCacheOwner = Some(ScreenIdentity(canvas))
    )

  /** How many device pixels the last frame `owner` presented had to copy, if it has presented one. */
  def lastCopiedPixels(owner: BackendCaches): Option[Long] =
    Option.when(owner.holds(FramesKey))(owner.getOrCreate(FramesKey)(new RasterFrames).copiedPixels)
