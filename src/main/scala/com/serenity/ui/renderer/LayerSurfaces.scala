package com.serenity.ui.renderer

import java.awt.image.BufferedImage
import java.awt.{AlphaComposite, Font}

import com.serenity.ui.layout.{CellMetrics, PixelRect}

/** Builds the transparent surfaces layers paint into; see [[Java2DRenderSurface.forLayer]]. */
private[renderer] object LayerSurfaces:

  def build(
    metrics: CellMetrics,
    font: Font,
    logicalWidthPx: Int,
    logicalHeightPx: Int,
    deviceScaleX: Double,
    deviceScaleY: Double,
    onFlush: BufferedImage => Unit,
    textCaches: SurfaceTextCaches,
    recycled: Option[BufferedImage],
    activity: SurfaceActivity,
    bounds: Option[PixelRect]
  ): Java2DRenderSurface =
    val origin = bounds.map(DeviceOrigin.of(_, deviceScaleX, deviceScaleY)).getOrElse(DeviceOrigin.Zero)
    val width = bounds.fold(Java2DRenderSurface.deviceImageDimension(logicalWidthPx, deviceScaleX))(
      DeviceOrigin.widthOf(_, deviceScaleX)
    )
    val height = bounds.fold(Java2DRenderSurface.deviceImageDimension(logicalHeightPx, deviceScaleY))(
      DeviceOrigin.heightOf(_, deviceScaleY)
    )
    val image = recycled
      .filter(candidate => candidate.getWidth == width && candidate.getHeight == height)
      .map(clearedForReuse)
      .getOrElse {
        activity.layerAllocated(width.toLong * height)
        new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
      }
    new Java2DRenderSurface(
      image,
      metrics,
      font,
      onFlush,
      logicalWidthPx = logicalWidthPx,
      logicalHeightPx = logicalHeightPx,
      deviceScaleX = deviceScaleX,
      deviceScaleY = deviceScaleY,
      contentPersists = false,
      imageOriginPx = origin,
      textCaches = textCaches,
      activity = activity
    )

  private def clearedForReuse(image: BufferedImage): BufferedImage =
    val graphics = image.createGraphics()
    try
      graphics.setComposite(AlphaComposite.Clear)
      graphics.fillRect(0, 0, image.getWidth, image.getHeight)
    finally graphics.dispose()
    image
