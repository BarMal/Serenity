package com.serenity.ui.skia

import java.awt.image.{BufferedImage, DataBufferInt}
import java.nio.{ByteBuffer, ByteOrder}
import java.util.concurrent.atomic.AtomicReference

import com.serenity.ui.renderer.DeviceArea
import org.jetbrains.skia.{Bitmap, ColorAlphaType, ColorType, ImageInfo, Surface}

/** The raster surface one window's frames are painted on, and what is known about the images it was copied into.
  *
  * The surface lives as long as the window keeps its size and scale, so a frame starts from the pixels the last one
  * left. One thread draws a frame at a time: [[surfaceFor]] opens it and [[present]] closes it.
  */
final private[ui] class RasterFrames:
  import RasterFrames.*

  private val state = new AtomicReference(State.Empty)

  /** The surface for a frame of `widthPx` x `heightPx` device pixels. A frame that was opened and never presented
    * leaves the surface ahead of every image, so it is treated like a new size.
    */
  def surfaceFor(widthPx: Int, heightPx: Int, scale: Double): Surface =
    val current = state.get()
    current.held.filter(_.fits(widthPx, heightPx, scale)) match
      case Some(held) if !current.inFlight =>
        state.set(current.copy(inFlight = true))
        held.surface
      case Some(held) =>
        state.set(current.copy(last = None, beforeLast = None, inFlight = true))
        held.surface
      case None =>
        current.held.foreach(_.surface.close())
        val held = Held(Surface.Companion.makeRasterN32Premul(widthPx, heightPx), widthPx, heightPx, scale)
        state.set(State(Some(held), None, None, inFlight = true, current.copiedPixels))
        held.surface

  /** Copies what `image` lacks of the frame just painted on `surface`, then remembers the image. */
  def present(surface: Surface, image: BufferedImage, damage: Option[List[DeviceArea]]): Unit =
    val current = state.get()
    val whole   = DeviceArea(0, 0, image.getWidth, image.getHeight)
    val areas   = RasterCopyPlan.areas(image, current.last, current.beforeLast, damage, whole)
    areas.foreach(copyInto(surface, image, _))
    state.set(
      current.copy(
        last = Some(RasterCopyPlan.Filled(image, damage)),
        beforeLast = current.last,
        inFlight = false,
        copiedPixels = areas.map(area => area.width.toLong * area.height).sum
      )
    )

  /** How many device pixels the last presented frame copied. */
  def copiedPixels: Long = state.get().copiedPixels

object RasterFrames:

  final private case class Held(surface: Surface, widthPx: Int, heightPx: Int, scale: Double):
    def fits(width: Int, height: Int, at: Double): Boolean = widthPx == width && heightPx == height && scale == at

  final private case class State(
      held: Option[Held],
      last: Option[RasterCopyPlan.Filled],
      beforeLast: Option[RasterCopyPlan.Filled],
      inFlight: Boolean,
      copiedPixels: Long
  )

  private object State:
    val Empty: State = State(None, None, None, inFlight = false, 0L)

  /** A frame that keeps nothing: the whole surface goes into `image`. */
  def copyWhole(surface: Surface, image: BufferedImage): Unit =
    copyInto(surface, image, DeviceArea(0, 0, image.getWidth, image.getHeight))

  private def copyInto(surface: Surface, image: BufferedImage, area: DeviceArea): Unit =
    // BGRA whatever the platform's native order, so the bytes read as the little-endian ARGB ints an image holds
    val info   = new ImageInfo(area.width, area.height, ColorType.BGRA_8888, ColorAlphaType.PREMUL)
    val bitmap = Bitmap()
    val read   = bitmap.allocPixels(info) && surface.readPixels(bitmap, area.left, area.top)
    val bytes  = if read then Option(bitmap.readPixels(info, area.width * 4, 0, 0)) else None
    bytes.foreach { raw =>
      val pixels = new Array[Int](area.width * area.height)
      ByteBuffer.wrap(raw).order(ByteOrder.LITTLE_ENDIAN).asIntBuffer().get(pixels)
      image.getRaster.getDataBuffer match
        case target: DataBufferInt =>
          val data = target.getData
          (0 until area.height).foreach(row =>
            System.arraycopy(pixels, row * area.width, data, (area.top + row) * image.getWidth + area.left, area.width)
          )
        case _ => image.setRGB(area.left, area.top, area.width, area.height, pixels, 0, area.width)
    }
