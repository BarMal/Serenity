package com.serenity.ui.skia

import java.awt.Font

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The counters a scaling benchmark reads to say how well the shape cache serves many buffers (#2478): every lookup,
  * and every stretch a rotation drops. Followed by hand with a capacity of one stretch a generation, as the promotion
  * counts are in [[SkiaShapedTextPromotionsSpec]].
  */
class SkiaShapedTextLookupsSpec extends AnyFlatSpec with Matchers:

  private val awtFont = new Font(Font.MONOSPACED, Font.PLAIN, 12)
  private val font    = new SkiaFontCache().faceFor(awtFont).font

  private def lookups(cache: SkiaShapedTextCache, texts: String*): Unit =
    texts.foreach(text => cache.stretchOf(text, font))

  "A Skia shaped text cache" should "count every lookup, whichever generation answers it" in {
    val cache = new SkiaShapedTextCache(capacityPerGeneration = 1)

    lookups(cache, "a", "b", "a", "a")

    cache.lookups shouldBe 4L
    (cache.shapings, cache.promotions) shouldBe (2L, 1L)
  }

  it should "count the stretches a rotation drops, none while both generations have room" in {
    val roomy = new SkiaShapedTextCache(capacityPerGeneration = 8)
    lookups(roomy, "a", "b", "c", "a")
    roomy.evictions shouldBe 0L

    val tight = new SkiaShapedTextCache(capacityPerGeneration = 1)
    lookups(tight, "a", "b", "a")
    tight.evictions shouldBe 1L
  }

  it should "count a stretch dropped for good, not one promoted to the newer generation first" in {
    val cache = new SkiaShapedTextCache(capacityPerGeneration = 1)

    lookups(cache, "a", "b", "a", "c", "d", "a")

    (cache.lookups, cache.shapings, cache.promotions, cache.evictions) shouldBe (6L, 5L, 1L, 4L)
  }

  it should "leave a lookup the newer generation answers out of the evictions" in {
    val cache = new SkiaShapedTextCache(capacityPerGeneration = 1)
    lookups(cache, "a", "b", "a")

    lookups(cache, "a", "a", "a")

    (cache.lookups, cache.evictions) shouldBe (6L, 1L)
  }
