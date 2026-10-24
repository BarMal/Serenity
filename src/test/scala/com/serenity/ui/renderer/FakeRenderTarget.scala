package com.serenity.ui.renderer

import java.awt.Rectangle
import java.awt.image.BufferedImage
import java.util.concurrent.atomic.AtomicReference

import com.serenity.ui.layout.{CellMetrics, ViewportSize}
import com.serenity.ui.renderer.Java2DRenderSurface.DeviceScale

/** A [[RenderTarget]] with no window system behind it: it hands out images, records what the renderer publishes, and
  * answers the caret overlay the way a window does once a base frame has been published.
  */
final class FakeRenderTarget(
    val metrics: CellMetrics,
    val viewportSize: ViewportSize,
    logicalWidthPx: Int,
    logicalHeightPx: Int,
    scale: DeviceScale
) extends RenderTarget:

  final case class Published(image: BufferedImage, dirtyRects: Option[List[Rectangle]])
  final case class Acquired(width: Int, height: Int, imageType: Int, image: BufferedImage)

  private val publishedFrames = new AtomicReference(Vector.empty[Published])
  private val baseFrames      = new AtomicReference(Vector.empty[BufferedImage])
  private val acquired        = new AtomicReference(Vector.empty[Acquired])
  private val overlays        = new AtomicReference(Vector.empty[List[CaretPaint]])

  val identityToken: Object = new Object

  def published: Vector[Published]            = publishedFrames.get()
  def baseOnly: Vector[BufferedImage]         = baseFrames.get()
  def acquisitions: Vector[Acquired]          = acquired.get()
  def overlayCarets: Vector[List[CaretPaint]] = overlays.get()

  def geometry: SurfaceGeometry =
    SurfaceGeometry(logicalWidthPx, logicalHeightPx, scale, ScreenIdentity(identityToken))

  def acquireBaseImage(width: Int, height: Int, imageType: Int): BufferedImage =
    val image = new BufferedImage(width, height, imageType)
    val _     = acquired.updateAndGet(_ :+ Acquired(width, height, imageType, image))
    image

  def onImageReady(image: BufferedImage, dirtyRects: Option[List[Rectangle]]): Unit =
    val _ = publishedFrames.updateAndGet(_ :+ Published(image, dirtyRects))

  def onBaseImageReady(image: BufferedImage): Unit =
    val _ = baseFrames.updateAndGet(_ :+ image)

  def onCursorOverlayReady(baseDirtyRects: Option[List[Rectangle]])(paintCarets: => List[CaretPaint]): Boolean =
    if publishedFrames.get().isEmpty && baseFrames.get().isEmpty then false
    else
      val _ = overlays.updateAndGet(_ :+ paintCarets)
      true
