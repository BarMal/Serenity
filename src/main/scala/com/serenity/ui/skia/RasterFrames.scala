package com.serenity.ui.skia

import java.awt.image.{BufferedImage, DataBufferInt}
import java.nio.{ByteBuffer, ByteOrder}
import java.util.concurrent.atomic.AtomicReference

import com.serenity.ui.renderer.DeviceArea
import org.jetbrains.skia.{ColorType, ImageInfo, Pixmap, Surface}

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
        val held = Held(Surface.Companion.makeRaster(frameInfo(widthPx, heightPx)), widthPx, heightPx, scale)
        state.set(State(Some(held), None, None, inFlight = true, current.copiedPixels))
        held.surface

  /** Brings `image` up to the frame just painted on `surface`, then remembers the image. A frame that scrolled has the
    * image move its rows as the surface did, so only what the frame painted over them, or changed beyond them, is
    * copied out of Skia.
    */
  def present(
    surface: Surface,
    image: BufferedImage,
    damage: Option[List[DeviceArea]],
    moves: List[RasterCopyPlan.Move] = Nil,
    writes: Option[List[DeviceArea]] = None
  ): Unit =
    val current = state.get()
    val whole   = DeviceArea(0, 0, image.getWidth, image.getHeight)
    val now     = RasterCopyPlan.Filled(image, damage, moves, writes)
    val plan    = RasterCopyPlan.plan(image, current.last, current.beforeLast, now, whole)
    plan.shifts.foreach(move => ImageMove(image, move.area, move.dyPx))
    plan.copies.foreach(copyInto(surface, image, _))
    state.set(
      current.copy(
        last = Some(now),
        beforeLast = current.last,
        inFlight = false,
        copiedPixels = plan.copies.map(area => area.width.toLong * area.height).sum
      )
    )

  /** The surface frames are painted on, for a test to compare with what was presented. */
  private[serenity] def retainedSurface: Option[Surface] = state.get().held.map(_.surface)

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

  /** BGRA whatever Skia's native 32-bit order is, so a surface's ints are the image's ARGB ints on every platform. */
  private[skia] def frameInfo(widthPx: Int, heightPx: Int): ImageInfo =
    ImageInfo.Companion.makeN32Premul(widthPx, heightPx).withColorType(ColorType.BGRA_8888)

  /** A frame that keeps nothing: the whole surface goes into `image`. */
  def copyWhole(surface: Surface, image: BufferedImage): Unit =
    copyInto(surface, image, DeviceArea(0, 0, image.getWidth, image.getHeight))

  /** A read is a chunk of rows of the surface, copied into one array of about this size: a frame is copied out in many
    * small allocations that G1 never has to treat as humongous, instead of two arrays the size of the frame.
    */
  private val ChunkBytes = 256 * 1024

  /** A wide area reads several full rows at a time; a narrow one reads its own columns a row at a time, so a read never
    * carries many more bytes than the area has.
    */
  private[skia] def rowsPerRead(areaWidthPx: Int, surfaceWidthPx: Int): Int =
    if areaWidthPx * 2 >= surfaceWidthPx then (ChunkBytes / (surfaceWidthPx * 4)).max(1) else 1

  /** A surface of any other 32-bit order than BGRA holds its pixels as ABGR ints. */
  @annotation.tailrec
  private def swapRedBlue(ints: Array[Int], index: Int, end: Int): Unit =
    if index < end then
      val argb = ints(index)
      ints(index) = (argb & 0xff00ff00) | ((argb & 0xff) << 16) | ((argb >>> 16) & 0xff)
      swapRedBlue(ints, index + 1, end)

  private[skia] def copyInto(surface: Surface, image: BufferedImage, area: DeviceArea): Unit =
    val pixmap = Pixmap()
    try
      if surface.peekPixels(pixmap) then
        val pixels = pixmap.getBuffer
        try
          val rowBytes = pixmap.getRowBytes
          val rows     = rowsPerRead(area.width, surface.getWidth)
          val ints     = rowBytes / 4
          val target = image.getRaster.getDataBuffer match
            case int: DataBufferInt => Some(int.getData)
            case _                  => None
          val line = new Array[Int](if target.isEmpty then area.width else 0)
          val rgba = pixmap.getInfo.getColorType == ColorType.RGBA_8888
          (0 until area.height by rows).foreach { first =>
            val count = rows.min(area.height - first)
            val raw =
              pixels.getBytes((area.top + first) * rowBytes + area.left * 4, (count - 1) * rowBytes + area.width * 4)
            val read = ByteBuffer.wrap(raw).order(ByteOrder.LITTLE_ENDIAN).asIntBuffer()
            (0 until count).foreach { row =>
              val y = area.top + first + row
              target match
                case Some(data) =>
                  val start = y * image.getWidth + area.left
                  read.get(row * ints, data, start, area.width)
                  if rgba then swapRedBlue(data, start, start + area.width)
                case None =>
                  read.get(row * ints, line, 0, area.width)
                  if rgba then swapRedBlue(line, 0, area.width)
                  image.setRGB(area.left, y, area.width, 1, line, 0, area.width)
            }
          }
        finally pixels.close()
    finally pixmap.close()
