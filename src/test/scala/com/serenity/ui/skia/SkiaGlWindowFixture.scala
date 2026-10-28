package com.serenity.ui.skia

import java.nio.{ByteBuffer, ByteOrder}

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.ui.sdl.{Sdl, SdlGlWindow}
import org.jetbrains.skia.{Bitmap, ImageInfo, Surface}
import org.scalatest.Assertions

/** What the native Skia GL specs share: the real libSDL3 and a GL window on SDL's `offscreen` driver (Mesa's software
  * GL, no compositor).
  */
trait SkiaGlWindowFixture extends Assertions:

  protected lazy val sdl: Sdl = Sdl.load(sys.props.toMap, java.nio.file.Files.exists(_)) match
    case Right(loaded) => loaded
    case Left(error)   => fail(s"libSDL3 did not load: ${error.describe}")

  protected def onWindow[A](use: SdlGlWindow => IO[A]): A =
    SdlGlWindow
      .open(sdl, SdlGlWindow.Settings("skia gl spec", 96, 64, Some("offscreen")))
      .use {
        case Right(window) => use(window)
        case Left(error)   => IO.raiseError(new AssertionError(s"no GL window: ${error.describe}"))
      }
      .unsafeRunSync()

  /** The pixel at (`x`, `y`) of `surface`, as `0xAARRGGBB`. */
  protected def pixelAt(surface: Surface, x: Int, y: Int): Int =
    val info   = ImageInfo.Companion.makeN32Premul(surface.getWidth, surface.getHeight)
    val bitmap = Bitmap()
    require(bitmap.allocPixels(info) && surface.readPixels(bitmap, 0, 0), "cannot read the surface")
    val bytes = bitmap.readPixels(info, surface.getWidth * 4, 0, 0)
    ByteBuffer.wrap(bytes).order(ByteOrder.LITTLE_ENDIAN).getInt((y * surface.getWidth + x) * 4)
