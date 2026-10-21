package com.serenity.perf

import java.awt.image.BufferedImage
import java.awt.{AlphaComposite, Color}
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{ConcurrentHashMap, ConcurrentLinkedQueue}

/** The images one frame is drawn into: `drawn` are the ones a surface paints, taken in turn as a window's pool is, and
  * `screen` is the one the painted rects are copied onto.
  */
final class FrameImages(widthPx: Int, heightPx: Int, poolSize: Int):

  val drawn: Vector[BufferedImage] = Vector.fill(poolSize)(FrameImages.blank(widthPx, heightPx))

  val screen: BufferedImage = FrameImages.blank(widthPx, heightPx)

  def clear(): Unit =
    (screen +: drawn).foreach { image =>
      val graphics = image.createGraphics()
      try
        graphics.setComposite(AlphaComposite.Src)
        graphics.setColor(Color.BLACK)
        graphics.fillRect(0, 0, image.getWidth, image.getHeight)
      finally graphics.dispose()
    }

object FrameImages:

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
final class OracleImages:

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
        FrameImages(widthPx, heightPx, poolSize)
    try use(images)
    finally
      val _ = queue.offer(images)
