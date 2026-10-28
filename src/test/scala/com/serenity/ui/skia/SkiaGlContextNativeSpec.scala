package com.serenity.ui.skia

import java.nio.{ByteBuffer, ByteOrder}

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.testkit.SdlNativeTest
import com.serenity.ui.sdl.{Sdl, SdlGlWindow}
import org.jetbrains.skia.{Bitmap, ImageInfo, Surface}
import org.scalatest.DoNotDiscover
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Skia's GL context on a real SDL context, on the `offscreen` driver (Mesa's software GL). Run through
  * `sbt sdlNativeTest -Dserenity.sdl3.library.path=<directory holding libSDL3.so.0>`.
  */
@DoNotDiscover
class SkiaGlContextNativeSpec extends AnyFlatSpec with Matchers:

  private lazy val sdl: Sdl = Sdl.load(sys.props.toMap, java.nio.file.Files.exists(_)) match
    case Right(loaded) => loaded
    case Left(error)   => fail(s"libSDL3 did not load: ${error.describe}")

  private def onWindow[A](use: SdlGlWindow => IO[A]): A =
    SdlGlWindow
      .open(sdl, SdlGlWindow.Settings("skia gl context spec", 96, 64, Some("offscreen")))
      .use {
        case Right(window) => use(window)
        case Left(error)   => IO.raiseError(new AssertionError(s"no GL window: ${error.describe}"))
      }
      .unsafeRunSync()

  private def firstPixel(surface: Surface): Int =
    val info   = ImageInfo.Companion.makeN32Premul(surface.getWidth, surface.getHeight)
    val bitmap = Bitmap()
    require(bitmap.allocPixels(info) && surface.readPixels(bitmap, 0, 0), "cannot read the surface")
    ByteBuffer.wrap(bitmap.readPixels(info, surface.getWidth * 4, 0, 0)).order(ByteOrder.LITTLE_ENDIAN).getInt(0)

  "A Skia GL context on an SDL GL window" should "open and draw on a GPU surface" taggedAs SdlNativeTest in {
    val drawn = onWindow(_.onSdlThread { owner =>
      SkiaGlContext.open(owner).map { context =>
        try
          val surface =
            Surface.Companion.makeRenderTarget(context.direct, true, ImageInfo.Companion.makeN32Premul(8, 8))
          surface.getCanvas.clear(0xffff0000.toInt)
          context.direct.flushAndSubmit(surface, true)
          try firstPixel(surface)
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
