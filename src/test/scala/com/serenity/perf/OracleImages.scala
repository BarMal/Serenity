package com.serenity.perf

import java.awt.image.BufferedImage
import java.awt.{AlphaComposite, Color}
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{ConcurrentHashMap, ConcurrentLinkedQueue}

import com.serenity.{FrameBackend, FramePixels, SkiaFrames}

/** The images one frame is drawn into: `drawn` are the ones a surface paints, taken in turn as a window's pool is, and
  * `screen` is the one the painted rects are copied onto.
  */
final class FrameImages(widthPx: Int, heightPx: Int, poolSize: Int, backend: FrameBackend = SkiaFrames):

  val drawn: Vector[FramePixels] = Vector.fill(poolSize)(FrameImages.blank(backend, widthPx, heightPx))

  val screen: FramePixels = FrameImages.blank(backend, widthPx, heightPx)

  def clear(): Unit = (screen +: drawn).foreach(_.fill(0x000000))

object FrameImages:

  /** A frame of `backend`, black as a new image is. */
  def blank(backend: FrameBackend, widthPx: Int, heightPx: Int): FramePixels =
    val frame = backend.newFrame(widthPx, heightPx)
    frame.fill(0x000000)
    frame

/** Images for the frames a cold repaint draws, lent and taken back. A cold frame is the oracle of an incremental frame
  * spec and is drawn after every step, so allocating its images afresh each time put two full-window images per step on
  * the heap: 8 MB each at 1x and 33 MB each at 2x on a 1080p window, all of them humongous allocations. The images hold
  * no state a frame can depend on, because each is cleared to what a new one holds before it is lent; what keeps a cold
  * frame cold is that its `RenderCaches` and surface identity are created for that frame alone.
  *
  * An image is valid only inside `lend`, and two lends never share one, so frames drawn at the same time stay apart.
  */
final class OracleImages(backend: FrameBackend = SkiaFrames):

  private val idle     = ConcurrentHashMap[(Int, Int, Int), ConcurrentLinkedQueue[FrameImages]]()
  private val createdN = AtomicInteger(0)

  /** How many images were allocated, as opposed to lent again. */
  def created: Int = createdN.get

  def lend[A](widthPx: Int, heightPx: Int, poolSize: Int)(use: FrameImages => A): A =
    val queue = idle.computeIfAbsent((widthPx, heightPx, poolSize), _ => ConcurrentLinkedQueue())
    val images = Option(queue.poll()) match
      case Some(returned) =>
        returned.clear()
        returned
      case None =>
        val _ = createdN.incrementAndGet()
        FrameImages(widthPx, heightPx, poolSize, backend)
    try use(images)
    finally
      val _ = queue.offer(images)

/** The Java2D twin of [[FrameImages]] and [[OracleImages]], for the specs that instrument `Java2DRenderSurface` itself.
  */
/** The images one frame is drawn into (Java2D): `drawn` are the ones a surface paints, taken in turn as a window's pool
  * is, and `screen` is the one the painted rects are copied onto.
  */
final class Java2DFrameImages(widthPx: Int, heightPx: Int, poolSize: Int):

  val drawn: Vector[BufferedImage] = Vector.fill(poolSize)(Java2DFrameImages.blank(widthPx, heightPx))

  val screen: BufferedImage = Java2DFrameImages.blank(widthPx, heightPx)

  def clear(): Unit =
    (screen +: drawn).foreach { image =>
      val graphics = image.createGraphics()
      try
        graphics.setComposite(AlphaComposite.Src)
        graphics.setColor(Color.BLACK)
        graphics.fillRect(0, 0, image.getWidth, image.getHeight)
      finally graphics.dispose()
    }

object Java2DFrameImages:

  def blank(widthPx: Int, heightPx: Int): BufferedImage =
    BufferedImage(widthPx, heightPx, BufferedImage.TYPE_INT_RGB)

/** Images for the frames a cold repaint draws, lent and taken back. A cold frame is the oracle of an incremental frame
  * spec and is drawn after every step, so allocating its images afresh each time put two full-window images per step on
  * the heap: 8 MB each at 1x and 33 MB each at 2x on a 1080p window, all of them humongous allocations. The images hold
  * no state a frame can depend on, because each is cleared to what a new one holds before it is lent; what keeps a cold
  * frame cold is that its `RenderCaches` and surface identity are created for that frame alone.
  *
  * An image is valid only inside `lend`, and two lends never share one, so frames drawn at the same time stay apart.
  */
final class Java2DOracleImages:

  private val idle     = ConcurrentHashMap[(Int, Int, Int), ConcurrentLinkedQueue[Java2DFrameImages]]()
  private val createdN = AtomicInteger(0)

  /** How many images were allocated, as opposed to lent again. */
  def created: Int = createdN.get

  def lend[A](widthPx: Int, heightPx: Int, poolSize: Int)(use: Java2DFrameImages => A): A =
    val queue = idle.computeIfAbsent((widthPx, heightPx, poolSize), _ => ConcurrentLinkedQueue())
    val images = Option(queue.poll()) match
      case Some(returned) =>
        returned.clear()
        returned
      case None =>
        val _ = createdN.incrementAndGet()
        Java2DFrameImages(widthPx, heightPx, poolSize)
    try use(images)
    finally
      val _ = queue.offer(images)
