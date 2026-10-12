package com.serenity.ui.renderer

import java.awt.Font
import java.awt.image.BufferedImage
import javax.swing.JPanel

import com.serenity.ui.layout.CellMetrics

/** What the Swing frontend knows when it needs a surface for one frame of the window canvas. */
final case class FrameSurfaceRequest(
    metrics: CellMetrics,
    font: Font,
    canvas: JPanel,
    onFlush: BufferedImage => Unit,
    acquireImage: (Int, Int, Int) => BufferedImage,
    contentPersists: Boolean = true
)

/** The seam the frontend draws a frame through. The backend chosen at startup supplies the implementation. */
trait FrameSurfaceFactory:
  def create(request: FrameSurfaceRequest): RenderSurface

object FrameSurfaceFactory:

  val Java2D: FrameSurfaceFactory = request =>
    Java2DRenderSurface.forFrame(
      request.metrics,
      request.font,
      request.canvas,
      request.onFlush,
      request.acquireImage,
      request.contentPersists
    )
