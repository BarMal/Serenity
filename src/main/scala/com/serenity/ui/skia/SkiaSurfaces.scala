package com.serenity.ui.skia

import java.awt.image.{BufferedImage, DataBufferInt}
import java.nio.{ByteBuffer, ByteOrder, IntBuffer}
import java.util.concurrent.atomic.AtomicReference
import javax.swing.JPanel

import com.serenity.ui.renderer.{
  FontSpec,
  FrameSurfaceFactory,
  FrameSurfaceRequest,
  HostedCanvas,
  Java2DRenderSurface,
  RenderSurface,
  ScreenIdentity
}
import org.jetbrains.skia.{Bitmap, ImageInfo, Surface}

/** The two Skia backends as surface factories. Reached only through the thunks in the startup catalogue, so building
  * either is what first loads Skiko.
  */
object SkiaSurfaces:

  /** Frames drawn on the GPU inside a [[SkiaWindowCanvas]] that replaces the window's Java2D canvas. */
  def gpu(): FrameSurfaceFactory = new GpuFrames

  /** Frames drawn by Skia's CPU rasteriser into a retained surface, then copied into the image the Java2D canvas
    * presents. This is Skia on a toolkit that cannot host a Skia layer, such as native Wayland.
    */
  def raster(): FrameSurfaceFactory = new RasterFrames

/** [[hostedCanvas]] is built on first use, which is when the window is. A caller that asks for a surface of an image
  * anyway gets Java2D's, since this backend has no image to draw into.
  */
final private class GpuFrames extends FrameSurfaceFactory:
  private lazy val canvas = SkiaWindowCanvas.create()

  def create(request: FrameSurfaceRequest): RenderSurface = FrameSurfaceFactory.Java2D.create(request)

  override def hostedCanvas: Option[HostedCanvas] = Some(canvas)

final private class RasterFrames extends FrameSurfaceFactory:
  import RasterFrames.{ImageType, Retained}

  private val retained = new AtomicReference[Option[Retained]](None)

  def create(request: FrameSurfaceRequest): RenderSurface =
    val canvas        = request.canvas
    val logicalWidth  = Java2DRenderSurface.logicalCanvasDimension(canvas.getWidth, canvas.getPreferredSize.width)
    val logicalHeight = Java2DRenderSurface.logicalCanvasDimension(canvas.getHeight, canvas.getPreferredSize.height)
    val scale         = Java2DRenderSurface.deviceScaleFor(canvas)
    val widthPx       = Java2DRenderSurface.deviceImageDimension(logicalWidth, scale.x)
    val heightPx      = Java2DRenderSurface.deviceImageDimension(logicalHeight, scale.y)
    val kept          = retainedFor(canvas, widthPx, heightPx)
    SkiaRenderSurface.retaining(
      kept.surface,
      request.metrics,
      FontSpec.fromAwt(request.font),
      logicalWidth,
      logicalHeight,
      scale.x,
      onFlush = () => request.onFlush(kept.copyInto(request.acquireImage(widthPx, heightPx, ImageType))),
      layerCacheOwner = Some(ScreenIdentity(canvas))
    )

  /** The same surface for as long as the canvas keeps its size, which is what lets the renderer skip unchanged rows. */
  private def retainedFor(canvas: JPanel, widthPx: Int, heightPx: Int): Retained =
    retained.get().filter(_.fits(canvas, widthPx, heightPx)) match
      case Some(kept) => kept
      case None =>
        val fresh = Retained(canvas, widthPx, heightPx)
        retained.getAndSet(Some(fresh)).foreach(_.close())
        fresh

private object RasterFrames:

  val ImageType: Int = BufferedImage.TYPE_INT_RGB

  /** A raster surface with the bitmap its pixels are read back into, both kept for as long as the canvas keeps its
    * size.
    */
  final class Retained(canvas: JPanel, widthPx: Int, heightPx: Int):
    val surface: Surface = Surface.Companion.makeRasterN32Premul(widthPx, heightPx)

    private val info     = ImageInfo.Companion.makeN32Premul(widthPx, heightPx)
    private val readback = new Bitmap()
    require(readback.allocPixels(info), s"cannot allocate a ${widthPx}x$heightPx read-back bitmap")

    def fits(other: JPanel, otherWidthPx: Int, otherHeightPx: Int): Boolean =
      (canvas eq other) && widthPx == otherWidthPx && heightPx == otherHeightPx

    /** Skia's N32 pixels are `0xAARRGGBB` ints in native byte order, which is the layout of a `TYPE_INT_RGB` image. */
    def copyInto(image: BufferedImage): BufferedImage =
      require(surface.readPixels(readback, 0, 0), "cannot read back the Skia surface")
      val pixels =
        ByteBuffer.wrap(readback.readPixels(info, widthPx * 4, 0, 0)).order(ByteOrder.nativeOrder()).asIntBuffer()
      image.getRaster.getDataBuffer match
        case target: DataBufferInt => pixels.get(target.getData, 0, pixels.remaining.min(target.getData.length))
        case _                     => image.setRGB(0, 0, widthPx, heightPx, intsOf(pixels), 0, widthPx)
      image

    private def intsOf(pixels: IntBuffer): Array[Int] =
      val ints = new Array[Int](pixels.remaining)
      pixels.get(ints)
      ints

    def close(): Unit =
      readback.close()
      surface.close()
