package com.serenity.ui.renderer

import java.awt.image.BufferedImage
import java.util.concurrent.atomic.AtomicReference

/** Pixel buffers a window's frames borrow for per-frame effects (region blur) instead of allocating fresh ones every
  * frame. Frame-sized arrays are large enough that the JVM allocates them straight into the old generation, so
  * per-frame allocation alone was enough to keep the GC thrashing (#1798).
  *
  * Borrowing takes a buffer out of the pool and returning puts it back, so two frames rendering at once never share
  * one; the loser of that race simply allocates. Contents are whatever the last borrower left behind.
  */
final class Java2DScratchBuffers:
  private val images = new AtomicReference[Map[(Int, Int), BufferedImage]](Map.empty)
  private val shadows =
    new AtomicReference[Map[Java2DPanelChrome.ShadowSpriteKey, BufferedImage]](Map.empty)

  def withImage[A](width: Int, height: Int)(use: BufferedImage => A): A =
    val key = (width, height)
    val image =
      take(images)(_.get(key), _ - key).getOrElse(new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB))
    try use(image)
    finally
      // A size not seen before (an animating panel) evicts an arbitrary one rather than being refused, so the pool
      // follows the sizes currently on screen instead of keeping whichever came first.
      val _ = images.updateAndGet { pooled =>
        val room =
          if pooled.size < Java2DScratchBuffers.MaxPooledImageSizes || pooled.contains(key) then pooled
          else pooled.drop(1)
        room.updated(key, image)
      }

  /** A shadow sprite is only ever read once rendered, so frames share it rather than borrowing it. */
  def shadowSprite(key: Java2DPanelChrome.ShadowSpriteKey)(render: => BufferedImage): BufferedImage =
    shadows.get().get(key).getOrElse {
      val rendered = render
      val _ = shadows.updateAndGet { cached =>
        val room = if cached.size < Java2DScratchBuffers.MaxCachedShadowSprites then cached else cached.drop(1)
        room.updated(key, rendered)
      }
      rendered
    }

  @annotation.tailrec
  private def take[S, B](ref: AtomicReference[S])(find: S => Option[B], without: S => S): Option[B] =
    val current = ref.get()
    find(current) match
      case None                                                  => None
      case found if ref.compareAndSet(current, without(current)) => found
      case _                                                     => take(ref)(find, without)

object Java2DScratchBuffers:
  /** Blur regions are panel-sized, and a window shows a handful of panels at once. */
  private val MaxPooledImageSizes = 8

  /** One per panel geometry on screen, with room for a resize or scale-in animation to churn through. */
  private val MaxCachedShadowSprites = 8
