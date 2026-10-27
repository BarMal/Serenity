package com.serenity.perf

import cats.effect.unsafe.implicits.global
import com.serenity.diagnostics.FrameCounter
import com.serenity.rope.Balance
import com.serenity.ui.renderer.FrameSurfaceFactory
import com.serenity.ui.skia.SkiaRasterSurfaceFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The `scroll_frame` row of [[PresentBenchmarks]] must scroll on every backend. The window's pool hands out two
  * images in turn, and Java2D keeps what an image last showed, so a row that alternates between two states gives each
  * image the same state every time and Java2D draws nothing: the row would time an empty frame against a real one.
  */
class PresentScrollFrameSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val backends = List("Java2D" -> FrameSurfaceFactory.Java2D, "Skia raster" -> SkiaRasterSurfaceFactory)

  for (name, factory) <- backends do
    s"The scroll_frame row on $name" should "draw rows in every frame it measures" in {
      val cycle = PresentBenchmarks.scrollCycle(factory, 900, 600, 1.0)
      val drawn = (0 until 12).map { _ =>
        val _ = cycle.next()
        cycle.lastFrame(FrameCounter.RowsDrawn)
      }
      drawn.filter(_ == 0L) shouldBe empty
    }
