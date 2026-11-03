package com.serenity.ui.renderer

import com.serenity.diagnostics.{FrameCounter, FrameCounters}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** B1 of #2447: promotions travel from a surface to the frame's counters beside the shapings, and are logged by name.
  */
class ShapePromotionsCounterSpec extends AnyFlatSpec with Matchers:

  "A surface's activity" should "drain the promotions it was told of into the frame, and start again" in {
    val activity = SurfaceActivity()
    val counters = FrameCounters()
    counters.setEnabled(true)

    activity.promoted(3L)
    activity.promoted(2L)
    activity.drainInto(counters)
    counters.frameFinished()
    activity.drainInto(counters)
    counters.frameFinished()

    counters.lastFrame(FrameCounter.ShapePromotions) shouldBe 0L
    counters.drain().max(FrameCounter.ShapePromotions) shouldBe 5L
  }

  "The frame log" should "name promotions with their mean and largest frame" in {
    val activity = SurfaceActivity()
    val counters = FrameCounters()
    counters.setEnabled(true)
    activity.promoted(4L)
    activity.drainInto(counters)
    counters.frameFinished()
    activity.drainInto(counters)
    counters.frameFinished()

    counters.drain().logParts should contain("promotions avg=2.0 max=4")
  }

  "Counting a body's cache traffic" should "report only what happened while the body ran" in {
    val activity = SurfaceActivity()
    val cache    = ShapedTextCache(capacityPerGeneration = 1)
    val font     = new java.awt.Font(java.awt.Font.SERIF, java.awt.Font.PLAIN, 14)
    val frc      = com.serenity.ui.layout.TextLayoutSnapshot.defaultFontRenderContext()
    val _        = cache.stretchOf("a", font, frc)
    val _        = cache.stretchOf("b", font, frc)

    val _        = activity.counting(cache)(cache.stretchOf("a", font, frc))
    val counters = FrameCounters()
    counters.setEnabled(true)
    activity.drainInto(counters)
    counters.frameFinished()

    counters.lastFrame(FrameCounter.ShapingCalls) shouldBe 0L
    counters.lastFrame(FrameCounter.ShapePromotions) shouldBe 1L
  }
