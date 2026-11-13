package com.serenity.perf

import cats.effect.unsafe.implicits.global
import com.serenity.diagnostics.FrameCounter
import com.serenity.rope.Balance
import com.serenity.ui.skia.{DiscardingImageLayer, SkiaRasterSurfaceFactory}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The `scroll_frame` row of [[PresentBenchmarks]] must scroll on every backend. The window's pool hands out two images
  * in turn and keeps what an image last showed, so a row that alternates between two states could give each image the
  * same state every time and draw nothing: the row would time an empty frame.
  */
class PresentScrollFrameSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val backends = List(
    "Skia raster"               -> SkiaRasterSurfaceFactory,
    "Skia layer (image frames)" -> DiscardingImageLayer.factory
  )

  for (name, factory) <- backends do
    s"The scroll_frame row on $name" should "draw rows in every frame it measures" in {
      val cycle = PresentBenchmarks.scrollCycle(factory, 900, 600, 1.0)
      val drawn = (0 until 12).map { _ =>
        val _ = cycle.next()
        cycle.lastFrame(FrameCounter.RowsDrawn)
      }
      drawn.filter(_ == 0L) shouldBe empty
    }
