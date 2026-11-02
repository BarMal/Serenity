package com.serenity.ui.skia

import java.awt.Font

import com.serenity.diagnostics.{FrameCounter, FrameCounters}
import com.serenity.ui.layout.CellMetrics
import com.serenity.ui.renderer.{FontSpec, ShapeTraffic, StyledFontCache, SurfaceActivity}
import org.jetbrains.skia.Surface
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** B1 of #2447 on the Skia cache: the same two-generation rule as [[com.serenity.ui.renderer.ShapedTextCache]], so the
  * same hand-followed sequences give the same figures, and a surface reports them in the frame's counters.
  */
class SkiaShapedTextPromotionsSpec extends AnyFlatSpec with Matchers:

  private val awtFont = new Font(Font.MONOSPACED, Font.PLAIN, 12)
  private val font    = new SkiaFontCache().faceFor(awtFont).font

  private def lookups(cache: SkiaShapedTextCache, texts: String*): Unit =
    texts.foreach(text => cache.stretchOf(text, font))

  "A Skia shaped text cache" should "count a lookup found in the older generation as a promotion, not a shaping" in {
    val cache = new SkiaShapedTextCache(capacityPerGeneration = 1)

    lookups(cache, "a", "b", "a")

    cache.shapings shouldBe 2L
    cache.promotions shouldBe 1L
  }

  it should "count neither for a lookup the newer generation answers" in {
    val cache = new SkiaShapedTextCache(capacityPerGeneration = 1)
    lookups(cache, "a", "b", "a")

    lookups(cache, "a")

    cache.shapings shouldBe 2L
    cache.promotions shouldBe 1L
  }

  it should "shape again a stretch two rotations have dropped" in {
    val cache = new SkiaShapedTextCache(capacityPerGeneration = 1)
    lookups(cache, "a", "b", "a")

    lookups(cache, "c", "d", "a")

    cache.shapings shouldBe 5L
    cache.promotions shouldBe 1L
  }

  it should "report the same figures through the traffic it exposes to a surface" in {
    val cache = new SkiaShapedTextCache(capacityPerGeneration = 1)
    lookups(cache, "a", "b", "a")

    val traffic: ShapeTraffic = cache

    (traffic.shapings, traffic.promotions) shouldBe (2L, 1L)
  }

  "A Skia surface" should "report a promotion in the frame's counters, and none for a repeat" in {
    val metrics = CellMetrics(charWidth = 7, lineHeight = 13, ascent = 10)
    val caches = SkiaTextCaches(
      new SkiaFontCache(),
      new SkiaShapedTextCache(capacityPerGeneration = 1),
      new SkiaImageCache()
    )
    val activity = SurfaceActivity()
    val target   = Surface.Companion.makeRasterN32Premul(210, 130)
    val surface = new SkiaRenderSurface(
      target.getCanvas,
      metrics,
      FontSpec.fromAwt(awtFont),
      () => (),
      210,
      130,
      caches = caches,
      styledFonts = StyledFontCache(),
      activity = activity
    )
    def run(text: String): Unit =
      surface.text.withFallbackFamily(Font.SERIF, ligaturesOn = true) {
        surface.text.drawRunPx(14f, 26, 56f, metrics.lineHeight, metrics.ascent, text)
      }
    def frame(texts: String*): (Long, Long) =
      texts.foreach(run)
      val counters = FrameCounters()
      counters.setEnabled(true)
      activity.drainInto(counters)
      counters.frameFinished()
      (counters.lastFrame(FrameCounter.ShapingCalls), counters.lastFrame(FrameCounter.ShapePromotions))

    frame("office", "affix") shouldBe (2L, 0L)
    frame("office") shouldBe (0L, 1L)
    frame("office") shouldBe (0L, 0L)
  }
