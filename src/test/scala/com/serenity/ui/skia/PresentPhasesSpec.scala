package com.serenity.ui.skia

import java.awt.Font
import java.awt.image.BufferedImage

import com.serenity.ui.layout.{CellMetrics, PixelRect}
import com.serenity.ui.renderer.{DeviceArea, FontSpec, StyledFontCache}
import org.jetbrains.skia.Surface
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The phases of presenting a raster frame are timed where they run, so a benchmark can say where a frame's time went
  * without re-running the phases or guessing at them.
  */
class PresentPhasesSpec extends AnyFlatSpec with Matchers:

  private val width  = 300
  private val height = 200
  private val whole  = DeviceArea(0, 0, width, height)

  @annotation.tailrec
  private def spinUntil(deadline: Long): Unit =
    if System.nanoTime() < deadline then spinUntil(deadline)

  private def busyFor(nanos: Long): Unit = spinUntil(System.nanoTime() + nanos)

  "Presenting phases" should "charge the time of a timed body to its phase and nothing else" in {
    val phases  = new PresentPhases
    val _       = phases.timed(PresentPhase.Plan)(busyFor(3_000_000L))
    val drained = phases.drain()
    drained(PresentPhase.Plan) should be >= 3_000_000L
    PresentPhase.values.filter(_ != PresentPhase.Plan).map(drained) should contain only 0L
  }

  it should "return the body's value" in {
    new PresentPhases().timed(PresentPhase.CopyOut)(42) shouldBe 42
  }

  it should "be empty once drained" in {
    val phases = new PresentPhases
    phases.timed(PresentPhase.ImageShift)(busyFor(1_000_000L))
    val _ = phases.drain()
    PresentPhase.values.map(phases.drain()) should contain only 0L
  }

  it should "still charge a body that throws" in {
    val phases = new PresentPhases
    intercept[IllegalStateException](phases.timed(PresentPhase.Plan) {
      busyFor(2_000_000L); throw IllegalStateException("x")
    })
    phases.drain()(PresentPhase.Plan) should be >= 2_000_000L
  }

  private def image() = BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)

  "A raster frame's present" should "time its plan and its copy-out" in {
    val frames  = new RasterFrames
    val surface = frames.surfaceFor(width, height, 1.0)
    frames.present(surface, image(), Some(List(whole)))
    val phases = frames.phases.drain()
    phases(PresentPhase.Plan) should be > 0L
    phases(PresentPhase.CopyOut) should be > 0L
    phases(PresentPhase.ImageShift) shouldBe 0L
  }

  it should "time the rows its image shifts when the frame scrolled" in {
    val frames  = new RasterFrames
    val shown   = image()
    val surface = frames.surfaceFor(width, height, 1.0)
    frames.present(surface, shown, Some(Nil))
    val _        = frames.phases.drain()
    val surface2 = frames.surfaceFor(width, height, 1.0)
    frames.present(surface2, shown, Some(Nil), List(RasterCopyPlan.Move(whole, 20)), Some(Nil))
    frames.phases.drain()(PresentPhase.ImageShift) should be > 0L
  }

  "A surface that moves rows" should "time the move of its own pixels" in {
    val phases   = new PresentPhases
    val recorder = new ChangeRecorder(1.0, 1.0, width, height, phases)
    val metrics  = CellMetrics(charWidth = 7, lineHeight = 13, ascent = 10)
    val surface = SkiaRenderSurface.retaining(
      Surface.Companion.makeRasterN32Premul(width, height),
      metrics,
      FontSpec.fromAwt(new Font(Font.MONOSPACED, Font.PLAIN, 12)),
      width,
      height,
      1.0,
      SkiaTextCaches.fresh(),
      StyledFontCache(),
      changes = Some(recorder)
    )
    surface.pixelCopying.getOrElse(fail("expected pixel copying")).copyPixels(PixelRect(0, 0, width, 100), 20)
    phases.drain()(PresentPhase.SurfaceMove) should be > 0L
  }
