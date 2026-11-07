package com.serenity.ui.skia

import com.serenity.testkit.SdlNativeTest
import org.jetbrains.skia.{ImageInfo, Surface}
import org.scalatest.DoNotDiscover
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Skia's GL context on a real SDL context, on the `offscreen` driver (Mesa's software GL). Run through
  * `sbt sdlNativeTest -Dserenity.sdl3.library.path=<directory holding libSDL3.so.0>`.
  */
@DoNotDiscover
class SkiaGlContextNativeSpec extends AnyFlatSpec with Matchers with SkiaGlWindowFixture:

  "A Skia GL context on an SDL GL window" should "open and draw on a GPU surface" taggedAs SdlNativeTest in {
    val drawn = onWindow(_.onSdlThread { owner =>
      SkiaGlContext.open(owner).map { context =>
        try
          val surface =
            Surface.Companion.makeRenderTarget(context.direct, true, ImageInfo.Companion.makeN32Premul(8, 8))
          surface.getCanvas.clear(0xffff0000.toInt)
          context.direct.flushAndSubmit(surface, true)
          try pixelAt(surface, 0, 0)
          finally surface.close()
        finally context.close()
      }
    })
    drawn shouldBe Right(0xffff0000.toInt)
  }

  it should "open again once an earlier one is closed" taggedAs SdlNativeTest in {
    val opened = onWindow(_.onSdlThread(owner => List.fill(2)(SkiaGlContext.open(owner).map(_.close()))))
    opened shouldBe List(Right(()), Right(()))
  }
