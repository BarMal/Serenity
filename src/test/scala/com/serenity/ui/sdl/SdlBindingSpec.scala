package com.serenity.ui.sdl

import java.lang.foreign.ValueLayout

import com.serenity.testkit.SdlNativeTest
import org.scalatest.{BeforeAndAfterAll, DoNotDiscover}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The binding against the real library, on SDL's `dummy` video driver (no compositor needed). Run with
  * `sbt sdlNativeTest -Dserenity.sdl3.library.path=<directory holding libSDL3.so.0>`.
  */
@DoNotDiscover
class SdlBindingSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll:

  private lazy val sdl: Sdl = Sdl.load(sys.props.toMap, java.nio.file.Files.exists(_)) match
    case Right(loaded) => loaded
    case Left(error)   => fail(s"libSDL3 did not load: ${error.describe}")

  override protected def beforeAll(): Unit =
    sdl.init(Some("dummy")) shouldBe Right(())

  override protected def afterAll(): Unit = sdl.quit()

  private def withWindow[A](width: Int, height: Int)(use: SdlWindow => A): A =
    val window = sdl.createWindow("binding spec", width, height, SdlLayouts.WindowResizable) match
      case Right(created) => created
      case Left(error)    => fail(error.describe)
    try use(window)
    finally sdl.destroyWindow(window)

  "Sdl" should "report the version of the library it loaded" taggedAs SdlNativeTest in {
    sdl.version should be >= 3004000
  }

  it should "initialise the video subsystem on the requested driver" taggedAs SdlNativeTest in {
    sdl.currentVideoDriver shouldBe Some("dummy")
  }

  it should "give a window a surface of its size with at least four bytes a pixel" taggedAs SdlNativeTest in
    withWindow(320, 200) { window =>
      sdl.windowSize(window) shouldBe Right((320, 200))
      val surface = sdl.surface(window).fold(error => fail(error.describe), identity)
      (surface.width, surface.height) shouldBe (320, 200)
      surface.pitch should be >= 320 * 4
      Set(SdlLayouts.PixelFormatXrgb8888, SdlLayouts.PixelFormatArgb8888) should contain(surface.format)
      surface.pixels.byteSize should be >= surface.pitch.toLong * surface.height
    }

  it should "present pixels written into the surface, whole or by rectangle" taggedAs SdlNativeTest in
    withWindow(64, 48) { window =>
      val surface = sdl.surface(window).fold(error => fail(error.describe), identity)
      surface.pixels.set(ValueLayout.JAVA_INT, 0L, 0xff2040c0)

      sdl.updateSurfaceRects(window, List(SdlRect(0, 0, 16, 16))) shouldBe Right(())
      sdl.updateSurface(window) shouldBe Right(())
    }

  it should "carry SDL's own error text when a call fails" taggedAs SdlNativeTest in
    withWindow(64, 48) { window =>
      sdl.setWindowSize(window, 0, 100) match
        case Left(SdlError.Call(function, message)) =>
          function shouldBe "SDL_SetWindowSize"
          message should not be empty
        case other => fail(s"expected an SDL error, got $other")
    }

  it should "hand a pushed event back through poll, decoded" taggedAs SdlNativeTest in {
    Iterator.continually(sdl.pollEvent()).takeWhile(_ != Right(None)).foreach(_ => ())
    sdl.pushQuit() shouldBe Right(())
    sdl.pollEvent() shouldBe Right(Some(SdlEvent.Quit))
    sdl.pollEvent() shouldBe Right(None)
  }

  it should "round-trip clipboard text, with the UTF-8 it was given" taggedAs SdlNativeTest in {
    sdl.setClipboardText("clip é中") shouldBe Right(())
    sdl.clipboardText shouldBe Right(Some("clip é中"))
  }
