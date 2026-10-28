package com.serenity.ui.skia

import java.util.concurrent.{ConcurrentLinkedQueue, CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.AtomicInteger

import scala.concurrent.Await
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.state.manager.RenderCaches
import com.serenity.testkit.SdlNativeTest
import com.serenity.ui.color.RenderColor
import com.serenity.ui.layout.CellMetrics
import com.serenity.ui.renderer.{BackendCaches, FontSpec, RenderSurface, ScreenIdentity}
import com.serenity.ui.sdl.{GlProbe, Sdl, SdlGlWindow}
import org.scalatest.DoNotDiscover
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The presenter on a GL window on the `offscreen` driver (Mesa's software GL), through `sbt sdlNativeTest`. The
  * window's framebuffer is read just before each swap, since a swap leaves it undefined.
  */
@DoNotDiscover
class SkiaGlPresenterNativeSpec extends AnyFlatSpec with Matchers with SkiaGlWindowFixture:

  private val red   = RenderColor.fromArgb(0xffff0000)
  private val green = RenderColor.fromArgb(0xff00ff00)
  private val blue  = RenderColor.fromArgb(0xff0000ff)

  private val Red   = Vector(255, 0, 0, 255)
  private val Green = Vector(0, 255, 0, 255)
  private val Blue  = Vector(0, 0, 255, 255)

  private val Width  = 96
  private val Height = 64

  private val settings =
    GlFrameSettings(
      CellMetrics(charWidth = 7, lineHeight = 13, ascent = 10),
      FontSpec.fromAwt(new java.awt.Font(java.awt.Font.MONOSPACED, java.awt.Font.PLAIN, 12)),
      Width,
      Height,
      1.0,
      RenderCaches.create().surfaceText,
      new BackendCaches,
      ScreenIdentity(new Object)
    )

  private def fill(x: Int, y: Int, color: RenderColor)(surface: RenderSurface): Unit =
    surface.pixels.fillPixelRect(x, y, 20, 20, color)

  /** What the window's framebuffer holds at each point (from the top left), read before each swap, in swap order. */
  final private class Shown(points: List[(Int, Int)]):
    private val seen = new ConcurrentLinkedQueue[List[Vector[Int]]]()

    def read(sdl: Sdl): Unit =
      val probe = new GlProbe(sdl)
      val _     = seen.add(points.map((x, y) => probe.pixel(x, Height - 1 - y)))

    def swaps: List[List[Vector[Int]]] = seen.asScala.toList

  private def withPresenter[A](points: List[(Int, Int)])(use: (SdlGlWindow, SkiaGlPresenter, Shown) => A): A =
    val shown = new Shown(points)
    onWindow { window =>
      SkiaGlPresenter.open(window, shown.read).use {
        case Right(presenter) => IO(use(window, presenter, shown))
        case Left(error)      => IO.raiseError(new AssertionError(s"no presenter: ${error.describe}"))
      }
    }

  private def await[A](future: scala.concurrent.Future[A]): A = Await.result(future, 10.seconds)

  "A presenter" should "show the persistent frame on the window" taggedAs SdlNativeTest in {
    val swaps = withPresenter(List((5, 5))) { (_, presenter, shown) =>
      await(presenter.draw(settings)(fill(0, 0, red))) shouldBe Right(())
      presenter.requestPresent()
      await(presenter.drained())
      shown.swaps
    }
    swaps shouldBe List(List(Red))
  }

  it should "fill carets over the frame without putting them in the persistent frame" taggedAs SdlNativeTest in {
    val (swaps, persistent) = withPresenter(List((5, 5), (41, 2))) { (_, presenter, shown) =>
      await(presenter.draw(settings)(fill(0, 0, red))) shouldBe Right(())
      presenter.showCarets(List(GlCaret(40, 0, 4, 10, blue)))
      await(presenter.drained())
      (shown.swaps, await(presenter.withTargets(_.map(made => pixelAt(made.offscreen, 41, 2)))))
    }
    swaps shouldBe List(List(Red, Blue))
    persistent should not be Some(0xff0000ff)
  }

  it should "show the whole persistent frame at every swap, since the back buffer is undefined" taggedAs SdlNativeTest in {
    val swaps = withPresenter(List((5, 5))) { (_, presenter, shown) =>
      await(presenter.draw(settings)(fill(0, 0, red))) shouldBe Right(())
      presenter.requestPresent()
      await(presenter.drained())
      presenter.requestPresent()
      await(presenter.drained())
      shown.swaps
    }
    swaps shouldBe List(List(Red), List(Red))
  }

  it should "draw every frame it is given, in order, and present once for a run of requests" taggedAs SdlNativeTest in {
    val (order, swaps, presented) = withPresenter(List((5, 5), (25, 5), (45, 5))) { (window, presenter, shown) =>
      val blocked = new CountDownLatch(1)
      val running = new CountDownLatch(1)
      window.post { _ =>
        running.countDown()
        blocked.await(10, TimeUnit.SECONDS)
      }
      running.await(10, TimeUnit.SECONDS) shouldBe true
      val order = new ConcurrentLinkedQueue[Int]()
      def frame(index: Int, x: Int, color: RenderColor)(surface: RenderSurface): Unit =
        val _ = order.add(index)
        fill(x, 0, color)(surface)
      val drawn = List(
        presenter.draw(settings)(frame(1, 0, red)),
        { presenter.requestPresent(); presenter.draw(settings)(frame(2, 20, green)) },
        { presenter.requestPresent(); presenter.draw(settings)(frame(3, 40, blue)) }
      )
      presenter.requestPresent()
      blocked.countDown()
      drawn.foreach(await)
      await(presenter.drained())
      (order.asScala.toList, shown.swaps, presenter.presentedFrames)
    }
    order shouldBe List(1, 2, 3)
    presented shouldBe 1
    swaps shouldBe List(List(Red, Green, Blue))
  }

  it should "call back after each present" taggedAs SdlNativeTest in {
    val calls = withPresenter(Nil) { (_, presenter, _) =>
      val count = new AtomicInteger(0)
      presenter.onRendered(() => count.incrementAndGet(): Unit)
      await(presenter.draw(settings)(fill(0, 0, red))) shouldBe Right(())
      presenter.requestPresent()
      await(presenter.drained())
      presenter.requestPresent()
      await(presenter.drained())
      count.get
    }
    calls shouldBe 2
  }

  it should "swap nothing before any frame has been drawn" taggedAs SdlNativeTest in {
    val presented = withPresenter(Nil) { (_, presenter, _) =>
      presenter.requestPresent()
      await(presenter.drained())
      presenter.presentedFrames
    }
    presented shouldBe 0
  }

  // SDL's offscreen driver does not resize the framebuffer behind the window, so what the window shows after a resize
  // is left to a compositor; what the presenter owns, the persistent surface, is checked here.
  it should "draw the next frame in full, at the new size, after the window is resized" taggedAs SdlNativeTest in {
    val (resized, presented, pixels, sizes) = withPresenter(Nil) { (window, presenter, _) =>
      await(presenter.draw(settings)(fill(40, 0, red))) shouldBe Right(())
      presenter.requestPresent()
      await(presenter.drained())
      await(window.onSdlThread(owner => owner.setWindowSize(window.handle, 120, 80)).unsafeToFuture())
      Thread.sleep(300)
      val resized = await(window.takeResize.unsafeToFuture())
      await(presenter.draw(settings.copy(logicalWidthPx = 120, logicalHeightPx = 80))(fill(0, 0, green))) shouldBe
        Right(())
      presenter.requestPresent()
      await(presenter.drained())
      val (pixels, sizes) = await(
        presenter.withTargets(
          _.map(made =>
            (
              (pixelAt(made.offscreen, 5, 5), pixelAt(made.offscreen, 45, 5)),
              (made.offscreen.getWidth, made.offscreen.getHeight, made.window.getWidth, made.window.getHeight)
            )
          ).get
        )
      )
      (resized, presenter.presentedFrames, pixels, sizes)
    }
    resized shouldBe Some((120, 80))
    presented shouldBe 2
    pixels shouldBe (0xff00ff00.toInt, 0)
    sizes shouldBe (120, 80, 120, 80)
  }

  it should "draw the next frame in full after a scale change at the same pixel size" taggedAs SdlNativeTest in {
    val swaps = withPresenter(List((5, 5), (45, 5))) { (_, presenter, shown) =>
      await(presenter.draw(settings)(fill(40, 0, red))) shouldBe Right(())
      presenter.requestPresent()
      await(presenter.drained())
      val doubled = settings.copy(logicalWidthPx = Width / 2, logicalHeightPx = Height / 2, deviceScale = 2.0)
      await(presenter.draw(doubled)(fill(0, 0, green))) shouldBe Right(())
      presenter.requestPresent()
      await(presenter.drained())
      shown.swaps
    }
    swaps(1).head shouldBe Green
    swaps(1)(1) should not be Red
  }

  "A released presenter" should "have closed its surfaces, and refuse a frame without touching GL" taggedAs SdlNativeTest in {
    val (made, refused, presented) = onWindow { window =>
      for
        kept <- SkiaGlPresenter.open(window).use {
          case Right(presenter) =>
            IO {
              await(presenter.draw(settings)(fill(0, 0, red))) shouldBe Right(())
              (presenter, await(presenter.withTargets(identity)))
            }
          case Left(error) => IO.raiseError(new AssertionError(s"no presenter: ${error.describe}"))
        }
        (presenter, made) = kept
        refused <- IO.fromFuture(IO(presenter.draw(settings)(fill(0, 0, blue))))
        _       <- IO(presenter.requestPresent())
        _       <- IO(presenter.showCarets(List(GlCaret(0, 0, 4, 4, blue))))
        _       <- window.onSdlThread(_ => ())
      yield (made, refused, presenter.presentedFrames)
    }
    made.map(m => (m.offscreen.isClosed, m.window.isClosed)) shouldBe Some((true, true))
    refused.left.map(_.nonEmpty) shouldBe Left(true)
    presented shouldBe 0
  }
