package com.serenity.perf

import cats.effect.unsafe.implicits.global
import com.serenity.diagnostics.FrameCounter
import com.serenity.rope.Balance
import com.serenity.ui.skia.SkiaRasterSurfaceFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The profile's chrome variants must each remove something a typing frame paints, or the bisect they feed measures
  * nothing.
  */
class TypingPaintProfileSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val Frames = 6

  private def typingDraws(variant: TypingPaintProfile.Variant): (Double, Long) =
    val counts = new DrawCounts
    val cycle =
      TypingPaintProfile.typingCycle(CountingFactory(SkiaRasterSurfaceFactory, counts), 900, 600, 1.0, variant.config)
    counts.reset()
    val rowsDrawn = (0 until Frames).map { _ =>
      val _ = cycle.next()
      cycle.lastFrame(FrameCounter.RowsDrawn)
    }.sum
    (counts.total.toDouble / Frames, rowsDrawn)

  "The profile's variants" should "start from the full chrome and have distinct labels" in {
    TypingPaintProfile.variants.head.label shouldBe "full"
    TypingPaintProfile.variants.map(_.label).distinct should have size TypingPaintProfile.variants.size.toLong
  }

  it should "each draw rows on a typing frame" in
    TypingPaintProfile.variants.foreach { variant =>
      withClue(s"${variant.label}: ")(typingDraws(variant)._2 should be > 0L)
    }

  it should "paint fewer things per typing frame with no chrome than with all of it" in {
    val full    = typingDraws(TypingPaintProfile.variants.head)._1
    val minimal = typingDraws(TypingPaintProfile.variants.last)._1
    TypingPaintProfile.variants.last.label shouldBe "minimal"
    minimal should be > 0.0
    minimal should be < full
  }

  it should "show that the spell-check squiggles make most of a typing frame's draw calls" in {
    def draws(label: String): Double =
      typingDraws(TypingPaintProfile.variants.find(_.label == label).getOrElse(fail(s"no $label variant")))._1
    draws("no_spellcheck") should be < draws("full") / 2
  }
