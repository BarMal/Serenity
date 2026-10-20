package com.serenity

import java.awt.Font
import java.awt.image.BufferedImage
import javax.swing.JPanel

import com.serenity.ui.layout.CellMetrics
import com.serenity.ui.renderer.{Java2DRenderSurface, SurfaceTextCaches}

/** A frame surface over a freshly allocated image, as a test wants one: nothing persists between frames. */
object SingleUseFrameSurface:

  def apply(
    metrics: CellMetrics,
    font: Font,
    canvas: JPanel,
    onFlush: BufferedImage => Unit
  ): Java2DRenderSurface =
    Java2DRenderSurface.forFrame(
      metrics,
      font,
      canvas,
      onFlush,
      (width, height, imageType) => new BufferedImage(width, height, imageType),
      SurfaceTextCaches.fresh(),
      contentPersists = false
    )
