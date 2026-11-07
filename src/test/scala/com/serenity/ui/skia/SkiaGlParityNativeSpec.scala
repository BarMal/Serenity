package com.serenity.ui.skia

import com.serenity.state.manager.RenderCaches
import com.serenity.testkit.SdlNativeTest
import com.serenity.ui.color.RenderColor
import com.serenity.ui.layout.CellMetrics
import com.serenity.ui.renderer.{BackendCaches, FontSpec, RenderSurface, ScreenIdentity}
import org.jetbrains.skia.{ImageInfo, Surface}
import org.scalatest.DoNotDiscover
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The same renderer calls on Skia's CPU raster surface and on the GL window's offscreen surface (Mesa's software GL,
  * on the `offscreen` driver, through `sbt sdlNativeTest`). Axis-aligned fills have to agree to the pixel. Text is
  * measured and reported, not asserted: the two backends antialias glyphs differently.
  */
@DoNotDiscover
class SkiaGlParityNativeSpec extends AnyFlatSpec with Matchers with SkiaGlWindowFixture:

  private val Width  = 120
  private val Height = 80

  private val metrics = CellMetrics(charWidth = 7, lineHeight = 13, ascent = 10)
  private val font    = FontSpec.fromAwt(new java.awt.Font(java.awt.Font.MONOSPACED, java.awt.Font.PLAIN, 12))

  private def settings =
    GlFrameSettings(
      metrics,
      font,
      Width,
      Height,
      1.0,
      RenderCaches.create().surfaceText,
      new BackendCaches,
      ScreenIdentity(new Object)
    )

  private def onCpu(render: RenderSurface => Unit): Vector[Int] =
    val frame = settings
    val cpu   = Surface.Companion.makeRaster(ImageInfo.Companion.makeN32Premul(Width, Height))
    val surface = SkiaRenderSurface.retaining(
      cpu,
      frame.metrics,
      frame.font,
      Width,
      Height,
      1.0,
      SkiaTextCaches.of(frame.backendCaches),
      frame.textCaches.styledFonts,
      layerCacheOwner = Some(frame.screen)
    )
    try
      render(surface)
      surface.flush()
      allPixels(cpu)
    finally cpu.close()

  private def onGl(render: RenderSurface => Unit): Vector[Int] =
    onWindow(_.onSdlThread { owner =>
      SkiaGlContext.open(owner) match
        case Right(context) =>
          val targets = SkiaGlTargets(context)
          try
            SkiaGlFrames(context, targets).draw(settings)(render) shouldBe Right(())
            allPixels(targets.ensure(Width, Height, 1.0).fold(reason => throw new AssertionError(reason), _.offscreen))
          finally
            targets.close()
            context.close()
        case Left(error) => throw new AssertionError(s"no Skia GL context: ${error.describe}")
    })

  private def black = RenderColor.fromArgb(0xff000000)

  private def fills(surface: RenderSurface): Unit =
    surface.pixels.fillPixelRect(0, 0, Width, Height, black)
    surface.pixels.fillPixelRect(3, 4, 40, 20, RenderColor.fromArgb(0xffff0000))
    surface.pixels.fillPixelRect(30, 10, 50, 30, RenderColor.fromArgb(0xff00ff00))
    surface.pixels.fillPixelRect(70, 50, 45, 25, RenderColor.fromArgb(0xff3366cc))
    surface.pixels.fillPixelRect(0, 70, Width, 10, RenderColor.fromArgb(0xfff0f0f0))

  private def text(surface: RenderSurface): Unit =
    surface.pixels.fillPixelRect(0, 0, Width, Height, RenderColor.fromArgb(0xff202020))
    surface.setForegroundColor(RenderColor.fromArgb(0xffe0e0e0))
    surface.setBackgroundColor(RenderColor.fromArgb(0xff202020))
    surface.text.drawRunPx(4f, 4, 100f, metrics.lineHeight, metrics.ascent, "Hello, GL parity")

  "A frame drawn on the GL surface" should "agree with the CPU raster surface, pixel for pixel, for fills" taggedAs SdlNativeTest in {
    val (cpu, gl) = (onCpu(fills), onGl(fills))
    cpu.zip(gl).count((a, b) => a != b) shouldBe 0
    cpu.distinct.size should be > 3
  }

  it should "draw text the CPU surface also draws, and say how far the glyphs differ" taggedAs SdlNativeTest in {
    val (cpu, gl) = (onCpu(text), onGl(text))
    val ink       = (pixels: Vector[Int]) => pixels.count(_ != 0xff202020.toInt)
    ink(cpu) should be > 0
    ink(gl) should be > 0
    info(
      s"text parity: ${cpu.zip(gl).count((a, b) => a != b)} of ${cpu.size} pixels differ (cpu ink ${ink(cpu)}, gl ink ${ink(gl)})"
    )
  }
