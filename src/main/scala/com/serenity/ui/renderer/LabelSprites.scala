package com.serenity.ui.renderer

import java.awt.font.FontRenderContext
import java.awt.image.BufferedImage
import java.awt.{Color, Font, RenderingHints}
import java.util.concurrent.atomic.{AtomicInteger, AtomicLong}

/** The ink of short labels that repeat verbatim frame after frame -- line numbers -- rendered once and blitted.
  *
  * Laying a label out costs a `TextLayout` per draw, and a gutter redraws a few dozen of them whenever its rows change.
  * What a label looks like is fixed by its font, device scale, colour, text and the sub-pixel phase it starts at, so
  * that tuple keys a cache of its rendered ink. The cache holds at most `capacity` sprites, because the keys of a
  * scroll through a long document are unbounded.
  */
final class LabelSprites(capacity: Int):
  import LabelSprites.*

  private val cache       = new BoundedRefCache[Key, Sprite](new AtomicInteger(capacity))
  private val rasterisedN = new AtomicLong(0)

  def size: Int = cache.snapshot.size

  /** How many sprites have been rendered, as opposed to served from the cache. */
  def rasterised: Long = rasterisedN.get()

  def spriteFor(key: Key, fontRenderContext: FontRenderContext): Sprite =
    cache.get(key).getOrElse {
      val sprite = rasterise(key, fontRenderContext)
      cache.put(key, sprite)
      sprite
    }

  /** Room for an em of ink on every side of the text, so accents, italics and descenders are never clipped. */
  private def rasterise(key: Key, fontRenderContext: FontRenderContext): Sprite =
    val _        = rasterisedN.incrementAndGet()
    val emPx     = math.ceil(key.font.getSize2D.toDouble * math.max(key.scaleX, key.scaleY)).toInt.max(1)
    val inkWidth = math.ceil(key.font.getStringBounds(key.text, fontRenderContext).getWidth * key.scaleX).toInt
    val image    = new BufferedImage(inkWidth + 2 * emPx + 1, 3 * emPx, BufferedImage.TYPE_INT_ARGB)
    val g        = image.createGraphics()
    try
      g.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON)
      g.setRenderingHint(RenderingHints.KEY_FRACTIONALMETRICS, RenderingHints.VALUE_FRACTIONALMETRICS_ON)
      g.scale(key.scaleX.toDouble, key.scaleY.toDouble)
      g.setFont(key.font)
      g.setColor(new Color(key.argb, true))
      g.drawString(
        key.text,
        ((emPx + key.phaseSixteenths / 16.0) / key.scaleX).toFloat,
        (2.0 * emPx / key.scaleY).toFloat
      )
    finally g.dispose()
    Sprite(image, originX = emPx, baseline = 2 * emPx)

object LabelSprites:

  final case class Key(font: Font, scaleX: Int, scaleY: Int, argb: Int, text: String, phaseSixteenths: Int)

  /** `image` holds the ink; the label's origin lies `originX` pixels in and its baseline `baseline` pixels down. */
  final case class Sprite(image: BufferedImage, originX: Int, baseline: Int)

  val DefaultCapacity: Int = 256
