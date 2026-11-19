package com.serenity.perf

import java.util.concurrent.atomic.AtomicInteger

import cats.effect.IO
import com.serenity.state.manager.RenderCaches
import com.serenity.testkit.SdlNativeTest
import com.serenity.ui.color.RenderColor
import com.serenity.ui.layout.CellMetrics
import com.serenity.ui.renderer.{BackendCaches, FontSpec, RenderSurface, ScreenIdentity}
import com.serenity.ui.sdl.SdlGlWindow
import com.serenity.ui.skia.{GlFrameSettings, SkiaGlWindowFixture}
import org.scalatest.DoNotDiscover
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The present benchmark's stamping on a GL window on the `offscreen` driver, through `sbt sdlNativeTest`. */
@DoNotDiscover
class SdlPresentedFramesNativeSpec extends AnyFlatSpec with Matchers with SkiaGlWindowFixture:

  private val settings =
    GlFrameSettings(
      CellMetrics(charWidth = 7, lineHeight = 13, ascent = 10),
      FontSpec.fromAwt(new java.awt.Font(java.awt.Font.MONOSPACED, java.awt.Font.PLAIN, 12)),
      96,
      64,
      1.0,
      RenderCaches.create().surfaceText,
      new BackendCaches,
      ScreenIdentity(new Object)
    )

  private val red = RenderColor.fromArgb(0xffff0000)

  private def fill(surface: RenderSurface): Unit = surface.pixels.fillPixelRect(0, 0, 20, 20, red)

  private def withFrames[A](finish: Option[() => Unit])(use: (SdlGlWindow, SdlPresentedFrames) => A): A =
    onWindow { window =>
      SdlPresentedFrames.open(window, settings, finish).use {
        case Right(frames) => IO(use(window, frames))
        case Left(error)   => IO.raiseError(new AssertionError(s"no presenter: ${error.describe}"))
      }
    }

  "SdlPresentedFrames" should "stamp one ordered set of readings for each frame it presents" taggedAs SdlNativeTest in {
    val (stamps, presented) = withFrames(None) { (_, frames) =>
      val measured = frames.measure(warmup = 2, frames = 5)(() => frames.frame(fill))
      (measured, frames.presentedFrames)
    }
    stamps should have size 5
    all(stamps.map(_.isOrdered)) shouldBe true
    presented shouldBe 7
  }

  it should "run the finish step once after each draw and once before each swap" taggedAs SdlNativeTest in {
    val finishes = new AtomicInteger(0)
    val stamps = withFrames(Some(() =>
      val _ = finishes.incrementAndGet()
    ))((_, frames) => frames.measure(warmup = 0, frames = 3)(() => frames.frame(fill)))
    stamps should have size 3
    finishes.get shouldBe 6
  }

  it should "say which swap interval the window accepted" taggedAs SdlNativeTest in {
    onWindow(window => SdlPresentedFrames.setSwapInterval(window, 0)) shouldBe Right(())
  }
