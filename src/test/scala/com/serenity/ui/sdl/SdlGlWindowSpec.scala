package com.serenity.ui.sdl

import scala.concurrent.duration.*

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import com.serenity.testkit.SdlNativeTest
import org.scalatest.DoNotDiscover
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The GL window host against the real library. On the `offscreen` driver it gets an EGL context with no compositor
  * (Mesa's software GL is enough); on `dummy` there is no GL, which is the machine the fallback is for. Run with
  * `sbt sdlNativeTest -Dserenity.sdl3.library.path=<directory holding libSDL3.so.0>`.
  */
@DoNotDiscover
class SdlGlWindowSpec extends AnyFlatSpec with Matchers:

  private lazy val sdl: Sdl = Sdl.load(sys.props.toMap, java.nio.file.Files.exists(_)) match
    case Right(loaded) => loaded
    case Left(error)   => fail(s"libSDL3 did not load: ${error.describe}")

  private def settings(driver: String) = SdlGlWindow.Settings("gl window spec", 96, 64, Some(driver))

  private def onOffscreen[A](use: SdlGlWindow => IO[A]): A =
    SdlGlWindow
      .open(sdl, settings("offscreen"))
      .use {
        case Right(window) => use(window)
        case Left(error)   => IO.raiseError(new AssertionError(s"no GL window: ${error.describe}"))
      }
      .unsafeRunSync()

  "An SDL GL window" should "open with a current context at the size asked for" taggedAs SdlNativeTest in {
    val (size, renderer) = onOffscreen(window => (window.pixelSize, window.renderer).tupled)
    size shouldBe (96, 64)
    renderer should not be empty
  }

  it should "show the colour it was cleared to" taggedAs SdlNativeTest in {
    val pixel = onOffscreen { window =>
      window.clear(0.0f, 1.0f, 0.0f) *> window.onSdlThread(owner => new GlProbe(owner).pixel(10, 10))
    }
    pixel shouldBe Vector(0, 255, 0, 255)
  }

  it should "swap the cleared frame to the screen" taggedAs SdlNativeTest in {
    onOffscreen(window => window.clearAndSwap(0.2f, 0.4f, 0.6f)) shouldBe Right(())
  }

  it should "complete awaitClose when the window is asked to close" taggedAs SdlNativeTest in {
    onOffscreen { window =>
      window.awaitClose.timeout(50.millis).attempt.map(_.isLeft) <*
        window.onSdlThread(_.pushQuit()) <*
        window.awaitClose.timeout(5.seconds)
    } shouldBe true
  }

  it should "report a resize once after the window is resized" taggedAs SdlNativeTest in {
    val (first, second) = onOffscreen { window =>
      for
        _      <- window.onSdlThread(owner => owner.setWindowSize(window.handle, 120, 80))
        _      <- IO.sleep(200.millis)
        first  <- window.takeResize
        second <- window.takeResize
      yield (first, second)
    }
    first shouldBe Some((120, 80))
    second shouldBe None
  }

  "An SDL GL window on a Wayland compositor" should "draw and swap on the compositor's surface" taggedAs SdlNativeTest in {
    assume(sys.env.contains("WAYLAND_DISPLAY"), "no Wayland compositor to open a window on")
    val (driver, pixel, swapped) = SdlGlWindow
      .open(sdl, settings("wayland"))
      .use {
        case Right(window) =>
          for
            _       <- window.clear(1.0f, 0.0f, 0.0f)
            pixel   <- window.onSdlThread(owner => new GlProbe(owner).pixel(10, 10))
            swapped <- window.swap
            driver  <- window.onSdlThread(_.currentVideoDriver)
          yield (driver, pixel, swapped)
        case Left(error) => IO.raiseError(new AssertionError(s"no GL window: ${error.describe}"))
      }
      .unsafeRunSync()
    driver shouldBe Some("wayland")
    pixel shouldBe Vector(255, 0, 0, 255)
    swapped shouldBe Right(())
  }

  "Opening a GL window where SDL has no GL" should "refuse with SDL's reason, so the caller falls back" taggedAs SdlNativeTest in {
    val opened = SdlGlWindow.open(sdl, settings("dummy")).use(opened => IO.pure(opened.map(_ => ()))).unsafeRunSync()
    opened match
      case Left(SdlError.Call(_, message)) => message should include("OpenGL")
      case other                           => fail(s"expected a refusal, got $other")
  }

  it should "leave SDL usable for the next window" taggedAs SdlNativeTest in {
    SdlGlWindow.open(sdl, settings("dummy")).use(opened => IO.pure(opened.isLeft)).unsafeRunSync() shouldBe true
    onOffscreen(_.pixelSize) shouldBe (96, 64)
  }
