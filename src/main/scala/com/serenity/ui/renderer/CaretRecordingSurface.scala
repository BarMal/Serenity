package com.serenity.ui.renderer

import java.awt.image.BufferedImage
import java.awt.{Color, Font}
import java.util.concurrent.atomic.AtomicReference

import com.serenity.ui.layout.{CellMetrics, PixelRect}

/** A canvas-shaped [[Java2DRenderSurface]] that records caret fills instead of painting them, so the Swing window can
  * fill carets straight over its presented frame rather than compositing a full-window overlay image. Carets are only
  * ever drawn with `fillPixelRect`; the 1x1 backing image just satisfies the parent's constructor.
  */
final class CaretRecordingSurface private (
    metrics: CellMetrics,
    font: Font,
    logicalWidthPx: Int,
    logicalHeightPx: Int,
    deviceScaleX: Double,
    deviceScaleY: Double
) extends Java2DRenderSurface(
      new BufferedImage(1, 1, BufferedImage.TYPE_INT_ARGB),
      metrics,
      font,
      _ => (),
      logicalWidthPx = logicalWidthPx,
      logicalHeightPx = logicalHeightPx,
      deviceScaleX = deviceScaleX,
      deviceScaleY = deviceScaleY
    ):
  private val fills = new AtomicReference(Vector.empty[CaretRecordingSurface.CaretFill])

  override def fillPixelRect(xPx: Int, yPx: Int, widthPx: Int, heightPx: Int, color: RenderColor): Unit =
    val _ = fills.updateAndGet(
      _ :+ CaretRecordingSurface.CaretFill(PixelRect(xPx, yPx, widthPx.max(1), heightPx.max(1)), color.toAwt)
    )

  def recordedFills: List[CaretRecordingSurface.CaretFill] = fills.get().toList

object CaretRecordingSurface:

  final case class CaretFill(rect: PixelRect, color: Color)

  def forCanvas(metrics: CellMetrics, font: Font, canvas: javax.swing.JPanel): CaretRecordingSurface =
    val scale = Java2DRenderSurface.deviceScaleFor(canvas)
    new CaretRecordingSurface(
      metrics,
      font,
      Java2DRenderSurface.logicalCanvasDimension(canvas.getWidth, canvas.getPreferredSize.width),
      Java2DRenderSurface.logicalCanvasDimension(canvas.getHeight, canvas.getPreferredSize.height),
      scale.x,
      scale.y
    )
