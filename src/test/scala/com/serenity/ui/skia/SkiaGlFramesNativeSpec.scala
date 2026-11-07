package com.serenity.ui.skia

import java.util.concurrent.atomic.AtomicReference

import com.serenity.state.manager.RenderCaches
import com.serenity.testkit.SdlNativeTest
import com.serenity.ui.color.RenderColor
import com.serenity.ui.layout.CellMetrics
import com.serenity.ui.renderer.{BackendCaches, FontSpec, RenderSurface, ScreenIdentity, SurfaceContentIdentity}
import org.scalatest.DoNotDiscover
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The renderer's own drawing, run over the persistent offscreen surface of a GL window. On the `offscreen` driver
  * (Mesa's software GL), through `sbt sdlNativeTest`.
  */
@DoNotDiscover
class SkiaGlFramesNativeSpec extends AnyFlatSpec with Matchers with SkiaGlWindowFixture:

  private val red  = RenderColor.fromArgb(0xffff0000)
  private val blue = RenderColor.fromArgb(0xff0000ff)

  private val metrics = CellMetrics(charWidth = 7, lineHeight = 13, ascent = 10)
  private val font    = FontSpec.fromAwt(new java.awt.Font(java.awt.Font.MONOSPACED, java.awt.Font.PLAIN, 12))

  private def settings(width: Int, height: Int, backend: BackendCaches, screen: ScreenIdentity) =
    GlFrameSettings(metrics, font, width, height, 1.0, RenderCaches.create().surfaceText, backend, screen)

  private def withFrames[A](use: (SkiaGlFrames, SkiaGlTargets, GlFrameSettings) => A): A =
    onWindow(_.onSdlThread { owner =>
      SkiaGlContext.open(owner) match
        case Right(context) =>
          val targets = SkiaGlTargets(context)
          try
            use(
              SkiaGlFrames(context, targets),
              targets,
              settings(96, 64, new BackendCaches, ScreenIdentity(new Object))
            )
          finally
            targets.close()
            context.close()
        case Left(error) => throw new AssertionError(s"no Skia GL context: ${error.describe}")
    })

  private def pixel(targets: SkiaGlTargets, settings: GlFrameSettings, x: Int, y: Int): Int =
    val made = targets
      .ensure(settings.logicalWidthPx, settings.logicalHeightPx, settings.deviceScale)
      .fold(reason => throw new AssertionError(reason), identity)
    pixelAt(made.offscreen, x, y)

  private def fill(x: Int, y: Int, color: RenderColor)(surface: RenderSurface): Unit =
    surface.pixels.fillPixelRect(x, y, 20, 20, color)

  "A direct-drawn frame" should "land on the persistent offscreen surface" taggedAs SdlNativeTest in {
    val drawn = withFrames { (frames, targets, frame) =>
      frames.draw(frame)(fill(0, 0, red)) shouldBe Right(())
      pixel(targets, frame, 5, 5)
    }
    drawn shouldBe 0xffff0000.toInt
  }

  it should "leave the pixels of the frame before it where it does not draw" taggedAs SdlNativeTest in {
    val (first, second) = withFrames { (frames, targets, frame) =>
      frames.draw(frame)(fill(0, 0, red)) shouldBe Right(())
      frames.draw(frame)(fill(40, 0, blue)) shouldBe Right(())
      (pixel(targets, frame, 5, 5), pixel(targets, frame, 45, 5))
    }
    first shouldBe 0xffff0000.toInt
    second shouldBe 0xff0000ff
  }

  it should "show the renderer the same content key until the size changes" taggedAs SdlNativeTest in {
    // The renderer compares the key it kept (a closed surface after a resize) with the live one, as this does.
    val (present, same, changed) = withFrames { (frames, _, frame) =>
      def keyOf(settings: GlFrameSettings): Option[SurfaceContentIdentity] =
        val seen = new AtomicReference(Option.empty[SurfaceContentIdentity])
        frames.draw(settings)(surface => seen.set(surface.persistentContentKey)) shouldBe Right(())
        seen.get()
      val first   = keyOf(frame)
      val again   = keyOf(frame)
      val resized = keyOf(frame.copy(logicalWidthPx = 120, logicalHeightPx = 80))
      (first.nonEmpty && resized.nonEmpty, again == first, resized != first)
    }
    present shouldBe true
    same shouldBe true
    changed shouldBe true
  }

  it should "report a renderer that throws, and draw the next frame" taggedAs SdlNativeTest in {
    val (failure, drawn) = withFrames { (frames, targets, frame) =>
      val failure = frames.draw(frame)(_ => throw new IllegalStateException("boom"))
      frames.draw(frame)(fill(0, 0, red)) shouldBe Right(())
      (failure, pixel(targets, frame, 5, 5))
    }
    failure.left.map(_.contains("boom")) shouldBe Left(true)
    drawn shouldBe 0xffff0000.toInt
  }
