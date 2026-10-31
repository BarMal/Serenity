package com.serenity.ui.renderer

import java.awt.Font

import com.serenity.ui.layout.TextLayoutSnapshot
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** B1 of #2447: a cache says how often a lookup was answered by the older generation, which is the one figure that
  * tells a stretch shaped again after rotation from text that was never shaped.
  *
  * With room for one stretch per generation the rule is easy to follow by hand: a lookup that misses shapes and, the
  * newer generation being full, rotates it to the older; a lookup found in the older generation moves back to the
  * newer, rotating the other way.
  */
class ShapedTextPromotionsSpec extends AnyFlatSpec with Matchers:

  private val font = new Font(Font.SERIF, Font.PLAIN, 14)
  private val frc  = TextLayoutSnapshot.defaultFontRenderContext()

  private def lookups(cache: ShapedTextCache, texts: String*): Unit =
    texts.foreach(text => cache.stretchOf(text, font, frc))

  "A shaped text cache" should "count a lookup found in the older generation as a promotion, not a shaping" in {
    val cache = ShapedTextCache(capacityPerGeneration = 1)

    lookups(cache, "a", "b", "a")

    cache.shapings shouldBe 2L
    cache.promotions shouldBe 1L
  }

  it should "count neither for a lookup the newer generation answers" in {
    val cache = ShapedTextCache(capacityPerGeneration = 1)
    lookups(cache, "a", "b", "a")

    lookups(cache, "a")

    cache.shapings shouldBe 2L
    cache.promotions shouldBe 1L
  }

  it should "shape again a stretch two rotations have dropped" in {
    val cache = ShapedTextCache(capacityPerGeneration = 1)
    lookups(cache, "a", "b", "a")

    lookups(cache, "c", "d", "a")

    cache.shapings shouldBe 5L
    cache.promotions shouldBe 1L
  }

  it should "report the same figures through the traffic it exposes to a surface" in {
    val cache = ShapedTextCache(capacityPerGeneration = 1)
    lookups(cache, "a", "b", "a")

    val traffic: ShapeTraffic = cache

    (traffic.shapings, traffic.promotions) shouldBe (2L, 1L)
  }
