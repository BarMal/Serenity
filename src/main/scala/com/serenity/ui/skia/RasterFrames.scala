package com.serenity.ui.skia

import java.awt.image.{BufferedImage, DataBufferInt}
import java.nio.ByteOrder
import java.util.concurrent.atomic.AtomicReference

import com.serenity.ui.renderer.DeviceArea
import org.jetbrains.skia.impl.BufferUtil
import org.jetbrains.skia.{ColorType, ImageInfo, Pixmap, Surface}

/** The raster surface one window's frames are painted on, and what is known about the images it was copied into.
  *
  * The surface lives as long as the window keeps its size and scale, so a frame starts from the pixels the last one
  * left. One thread draws a frame at a time: [[surfaceFor]] opens it and [[present]] closes it.
  */
final private[ui] class RasterFrames:
  import RasterFrames.*

  private val state = new AtomicReference(State.Empty)

  private[serenity] val phases = new PresentPhases

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
    val plan = phases.timed(PresentPhase.Plan)(RasterCopyPlan.plan(image, current.last, current.beforeLast, now, whole))
    if plan.shifts.nonEmpty then
      phases.timed(PresentPhase.ImageShift)(plan.shifts.foreach(move => ImageMove(image, move.area, move.dyPx)))
    if plan.copies.nonEmpty then phases.timed(PresentPhase.CopyOut)(plan.copies.foreach(copyInto(surface, image, _)))
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

  /** A surface of any other 32-bit order than BGRA holds its pixels as ABGR ints. */
  @annotation.tailrec
  private def swapRedBlue(ints: Array[Int], index: Int, end: Int): Unit =
    if index < end then
      val argb = ints(index)
      ints(index) = (argb & 0xff00ff00) | ((argb & 0xff) << 16) | ((argb >>> 16) & 0xff)
      swapRedBlue(ints, index + 1, end)

  /** The surface's own memory is viewed as a direct buffer and read straight into the image's array, so a copy
    * allocates nothing the size of what it copies: no array a read of Skia's `Data` fills, and none to put it in the
    * image from.
    */
  private[skia] def copyInto(surface: Surface, image: BufferedImage, area: DeviceArea): Unit =
    val pixmap = Pixmap()
    try
      if surface.peekPixels(pixmap) then
        val rowBytes = pixmap.getRowBytes
        val view = BufferUtil.INSTANCE
          .getByteBufferFromPointer(pixmap.getAddr, rowBytes * surface.getHeight)
          .order(ByteOrder.LITTLE_ENDIAN)
          .asIntBuffer()
        val rgba = pixmap.getInfo.getColorType == ColorType.RGBA_8888
        image.getRaster.getDataBuffer match
          case target: DataBufferInt =>
            (area.top until area.top + area.height).foreach { y =>
              val start = y * image.getWidth + area.left
              view.get(y * (rowBytes / 4) + area.left, target.getData, start, area.width)
              if rgba then swapRedBlue(target.getData, start, start + area.width)
            }
          case _ =>
            val line = new Array[Int](area.width)
            (area.top until area.top + area.height).foreach { y =>
              view.get(y * (rowBytes / 4) + area.left, line, 0, area.width)
              if rgba then swapRedBlue(line, 0, area.width)
              image.setRGB(area.left, y, area.width, 1, line, 0, area.width)
            }
    finally pixmap.close()
