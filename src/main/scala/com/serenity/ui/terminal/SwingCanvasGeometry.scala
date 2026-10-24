package com.serenity.ui.terminal

import javax.swing.JPanel

import com.serenity.ui.renderer.Java2DRenderSurface.DeviceScale
import com.serenity.ui.renderer.{Java2DRenderSurface, ScreenIdentity, SurfaceGeometry}

/** The size, scale and identity of a Swing canvas, read the way the renderer needs them. */
object SwingCanvasGeometry:

  def of(canvas: JPanel): SurfaceGeometry =
    SurfaceGeometry(
      Java2DRenderSurface.logicalCanvasDimension(canvas.getWidth, canvas.getPreferredSize.width),
      Java2DRenderSurface.logicalCanvasDimension(canvas.getHeight, canvas.getPreferredSize.height),
      deviceScaleOf(canvas),
      ScreenIdentity(canvas)
    )

  def deviceScaleOf(canvas: JPanel): DeviceScale =
    Option(canvas.getGraphicsConfiguration)
      .map(_.getDefaultTransform)
      .map(transform => DeviceScale(transform.getScaleX.max(1.0), transform.getScaleY.max(1.0)))
      .getOrElse(DeviceScale(1.0, 1.0))
