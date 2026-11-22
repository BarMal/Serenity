package com.serenity.perf

import scala.concurrent.duration.*

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.testkit.SdlNativeTest
import com.serenity.ui.sdl.{Sdl, SdlGlWindow}
import org.scalatest.DoNotDiscover
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The key-to-swap benchmark against the real library, on the `offscreen` driver: keys pushed into SDL's queue go
  * through the translation, the app's own input and render loops and a real GL present, and come out as rows (#2380).
  * Run with `sbt sdlNativeTest -Dserenity.sdl3.library.path=<directory holding libSDL3.so.0>`.
  */
@DoNotDiscover
class SdlKeyToSwapNativeSpec extends AnyFlatSpec with Matchers:

  private lazy val sdl: Sdl = Sdl.load(sys.props.toMap, java.nio.file.Files.exists(_)) match
    case Right(loaded) => loaded
    case Left(error)   => fail(s"libSDL3 did not load: ${error.describe}")

  private val options = SdlKeyToSwapBenchmark.Options(
    keys = 20,
    warmupKeys = 5,
    spacing = 15.millis,
    injector = SdlKeyToSwapBenchmark.Injector.InProcess,
    swapIntervals = List(1)
  )

  private def outcome(): SdlKeyToSwapBenchmark.Outcome =
    SdlGlWindow
      .open(sdl, SdlGlWindow.Settings(SdlPresentWindow.Title, 640, 480, Some("offscreen")))
      .use {
        case Left(error) => IO.raiseError(new AssertionError(s"no GL window: ${error.describe}"))
        case Right(window) =>
          SkiaGlPresentBenchmarks.settledLogicalSize(window).flatMap { (width, height, scale) =>
            SdlKeyToSwapBenchmark.trial(window, SdlKeyToSwapBenchmark.Geometry(width, height, scale), 1, options)
          }
      }
      .unsafeRunSync()

  "The SDL key-to-swap benchmark" should "turn 20 pushed keys into 20 rows, every stamp in order" taggedAs SdlNativeTest in {
    val run = outcome()

    run.sent shouldBe 20
    run.arrived shouldBe options.warmupKeys + options.keys + 1
    run.keys should have size 20
    run.keys.filterNot(_.isOrdered) shouldBe empty
    run.keys.foreach { key =>
      key.eventPickupMs should be >= 0.0
      key.keyToSwapMs should be >= key.eventPickupMs
    }
  }

  it should "stamp each key with the frame that showed it, and print every row" taggedAs SdlNativeTest in {
    val lines = outcome().lines

    lines.exists(_.contains(" key_to_swap p50=")) shouldBe true
    lines.exists(_.contains(" event_pickup p50=")) shouldBe true
    lines.exists(_.contains(" draw p50=")) shouldBe true
    lines.exists(_.contains(" frame_wait p50=")) shouldBe true
    all(lines) should startWith("[sdl3-key]")
  }
