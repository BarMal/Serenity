package com.serenity.ui.skia

import com.serenity.testkit.SdlNativeTest
import com.serenity.ui.sdl.GlProbe
import org.scalatest.DoNotDiscover
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The two surfaces a window draws on: one that keeps the frame between draws, and the window's own framebuffer, which
  * a swap leaves undefined. On the `offscreen` driver (Mesa's software GL), through `sbt sdlNativeTest`.
  */
@DoNotDiscover
class SkiaGlTargetsNativeSpec extends AnyFlatSpec with Matchers with SkiaGlWindowFixture:

  private def withTargets[A](use: (SkiaGlContext, SkiaGlTargets, com.serenity.ui.sdl.Sdl) => A): A =
    onWindow(_.onSdlThread { owner =>
      SkiaGlContext.open(owner) match
        case Right(context) =>
          val targets = SkiaGlTargets(context)
          try use(context, targets, owner)
          finally
            targets.close()
            context.close()
        case Left(error) => throw new AssertionError(s"no Skia GL context: ${error.describe}")
    })

  private def ensured(targets: SkiaGlTargets, widthPx: Int, heightPx: Int, scale: Double): SkiaGlTargets.Made =
    targets.ensure(widthPx, heightPx, scale).fold(reason => throw new AssertionError(reason), identity)

  "The targets of a window" should "be the same surfaces while the size and scale stay" taggedAs SdlNativeTest in {
    val (first, second) = withTargets((_, targets, _) => (ensured(targets, 96, 64, 1.0), ensured(targets, 96, 64, 1.0)))
    (second.offscreen eq first.offscreen) shouldBe true
    (second.window eq first.window) shouldBe true
  }

  it should "be at the size asked for" taggedAs SdlNativeTest in {
    val sizes = withTargets { (_, targets, _) =>
      val made = ensured(targets, 96, 64, 1.0)
      (made.offscreen.getWidth, made.offscreen.getHeight, made.window.getWidth, made.window.getHeight)
    }
    sizes shouldBe (96, 64, 96, 64)
  }

  it should "keep what was drawn on the offscreen surface between draws" taggedAs SdlNativeTest in {
    val kept = withTargets { (context, targets, _) =>
      val drawn = ensured(targets, 96, 64, 1.0)
      drawn.offscreen.getCanvas.clear(0xff00ff00.toInt)
      context.direct.flushAndSubmit(drawn.offscreen, true)
      pixelAt(ensured(targets, 96, 64, 1.0).offscreen, 10, 10)
    }
    kept shouldBe 0xff00ff00.toInt
  }

  it should "be new surfaces, with the old ones closed, after a resize" taggedAs SdlNativeTest in {
    val (replaced, closed, sizes) = withTargets { (_, targets, _) =>
      val before = ensured(targets, 96, 64, 1.0)
      val after  = ensured(targets, 120, 80, 1.0)
      (
        (after.offscreen ne before.offscreen) && (after.window ne before.window),
        (before.offscreen.isClosed, before.window.isClosed),
        (after.offscreen.getWidth, after.offscreen.getHeight, after.window.getWidth, after.window.getHeight)
      )
    }
    replaced shouldBe true
    closed shouldBe (true, true)
    sizes shouldBe (120, 80, 120, 80)
  }

  it should "be new surfaces after a scale change at the same pixel size" taggedAs SdlNativeTest in {
    val (replaced, closed) = withTargets { (_, targets, _) =>
      val before = ensured(targets, 96, 64, 1.0)
      val after  = ensured(targets, 96, 64, 2.0)
      (after.offscreen ne before.offscreen, before.offscreen.isClosed)
    }
    replaced shouldBe true
    closed shouldBe true
  }

  it should "put what is drawn on the window surface on the window's framebuffer" taggedAs SdlNativeTest in {
    val pixel = withTargets { (context, targets, owner) =>
      val made = ensured(targets, 96, 64, 1.0)
      made.window.getCanvas.clear(0xff0000ff.toInt)
      context.direct.flushAndSubmit(made.window, true)
      new GlProbe(owner).pixel(10, 10)
    }
    pixel shouldBe Vector(0, 0, 255, 255)
  }

  it should "be refused, with a reason, for a size Skia cannot make a surface of" taggedAs SdlNativeTest in {
    val refused = withTargets((_, targets, _) => targets.ensure(0, 0, 1.0))
    refused.left.map(_.nonEmpty) shouldBe Left(true)
  }

  it should "close both surfaces when closed" taggedAs SdlNativeTest in {
    val made = onWindow { window =>
      window.onSdlThread { owner =>
        SkiaGlContext.open(owner).map { context =>
          val targets = SkiaGlTargets(context)
          val made    = ensured(targets, 96, 64, 1.0)
          targets.close()
          context.close()
          made
        }
      }
    }
    made.map(m => (m.offscreen.isClosed, m.window.isClosed)) shouldBe Right((true, true))
  }
