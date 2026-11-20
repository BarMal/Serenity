package com.serenity.perf

import scala.concurrent.duration.*

import com.serenity.testkit.SdlNativeTest
import com.serenity.ui.skia.SkiaGlWindowFixture
import org.scalatest.DoNotDiscover
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The fullscreen calls the present benchmark makes from the test, on the `offscreen` driver, through
  * `sbt sdlNativeTest`.
  */
@DoNotDiscover
class SdlPresentWindowNativeSpec extends AnyFlatSpec with Matchers with SkiaGlWindowFixture:

  "FullscreenCalls" should "make the window as large as the display it is on" taggedAs SdlNativeTest in {
    val calls = SdlPresentWindow.FullscreenCalls.load(sys.props.toMap).fold(fail(_), identity)
    val (entered, pixels, display) = onWindow { window =>
      for
        before  <- window.pixelSize
        entered <- window.onSdlThread(_ => calls.enter(window.handle))
        pixels  <- SdlPresentWindow.settledPixels(window, 5, 100, 20.millis, awayFrom = Some(before))
        display <- window.onSdlThread(_ => calls.displaySize(window.handle))
        scale   <- window.deviceScale
      yield (entered, pixels, display.map(shown => (shown, scale)))
    }

    entered shouldBe true
    val (shown, scale) = display.getOrElse(fail("SDL gave no display size"))
    shown.logicalWidth should be > 0
    shown.density should be > 0.0
    pixels shouldBe (math.round(shown.logicalWidth * scale).toInt, math.round(shown.logicalHeight * scale).toInt)
  }
