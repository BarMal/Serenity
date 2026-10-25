package com.serenity.ui.sdl

import com.serenity.testkit.SdlNativeTest
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.{BeforeAndAfterAll, DoNotDiscover}

/** The GL half of the binding against the real library. SDL's `offscreen` driver makes an EGL context with no
  * compositor; its `dummy` driver has no GL at all, which is how a machine without one looks. Run with
  * `sbt sdlNativeTest -Dserenity.sdl3.library.path=<directory holding libSDL3.so.0>` where Mesa's EGL is installed.
  */
@DoNotDiscover
class SdlGlBindingSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll:

  private lazy val sdl: Sdl = Sdl.load(sys.props.toMap, java.nio.file.Files.exists(_)) match
    case Right(loaded) => loaded
    case Left(error)   => fail(s"libSDL3 did not load: ${error.describe}")

  override protected def afterAll(): Unit = sdl.quit()

  private def ok[A](result: Either[SdlError, A]): A = result.fold(error => fail(error.describe), identity)

  private def requestEs3(): Unit =
    ok(sdl.glSetAttribute(SdlLayouts.GlAttribute.ContextProfileMask, SdlLayouts.GlProfileEs))
    ok(sdl.glSetAttribute(SdlLayouts.GlAttribute.ContextMajorVersion, 3))
    ok(sdl.glSetAttribute(SdlLayouts.GlAttribute.ContextMinorVersion, 0))
    ok(sdl.glSetAttribute(SdlLayouts.GlAttribute.StencilSize, 8))

  "A GL window on the offscreen driver" should "give a current context that draws and swaps" taggedAs SdlNativeTest in {
    ok(sdl.init(Some("offscreen")))
    requestEs3()
    val window  = ok(sdl.createWindow("gl binding spec", 64, 48, SdlLayouts.WindowOpenGl))
    val context = ok(sdl.glCreateContext(window))
    try
      ok(sdl.glMakeCurrent(window, context))
      ok(sdl.glSetSwapInterval(0))
      new GlProbe(sdl).renderer should not be empty
      ok(sdl.glSwapWindow(window))
    finally
      sdl.glDestroyContext(context)
      sdl.destroyWindow(window)
      sdl.quit()
  }

  it should "find the GL functions a context has by name" taggedAs SdlNativeTest in {
    ok(sdl.init(Some("offscreen")))
    requestEs3()
    val window  = ok(sdl.createWindow("gl binding spec", 64, 48, SdlLayouts.WindowOpenGl))
    val context = ok(sdl.glCreateContext(window))
    try
      ok(sdl.glMakeCurrent(window, context))
      List("glClear", "glClearColor", "glGetString", "glReadPixels").foreach(name =>
        withClue(name)(sdl.glProcAddress(name) should not be empty)
      )
    finally
      sdl.glDestroyContext(context)
      sdl.destroyWindow(window)
      sdl.quit()
  }

  "A GL window on the dummy driver" should "fail with SDL's own text, as a machine without GL does" taggedAs SdlNativeTest in {
    ok(sdl.init(Some("dummy")))
    try
      sdl.createWindow("no gl", 64, 48, SdlLayouts.WindowOpenGl) match
        case Left(SdlError.Call("SDL_CreateWindow", message)) => message should include("OpenGL")
        case other                                            => fail(s"expected SDL_CreateWindow to fail, got $other")
    finally sdl.quit()
  }
