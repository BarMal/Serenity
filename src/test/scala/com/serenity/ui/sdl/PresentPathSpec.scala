package com.serenity.ui.sdl

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Which way a window presents is decided by whether SDL could make a GL context for it. */
class PresentPathSpec extends AnyFlatSpec with Matchers:

  "PresentPath.fromContextAttempt" should "present through GL when SDL made a context" in {
    PresentPath.fromContextAttempt(Right("context")) shouldBe PresentPath.Gpu("context")
  }

  it should "fall back to Skia raster, with SDL's reason, when the context could not be made" in {
    val refusal = SdlError.Call("SDL_GL_CreateContext", "EGL not initialized")
    PresentPath.fromContextAttempt(Left(refusal)) shouldBe PresentPath.RasterFallback(refusal)
  }

  "A raster fallback" should "say why in one line a log can carry" in {
    val reason = PresentPath.RasterFallback(SdlError.Call("SDL_GL_CreateContext", "EGL not initialized")).describe
    reason shouldBe "Skia raster (no GL context: SDL_GL_CreateContext failed: EGL not initialized)"
  }

  "A GPU path" should "say so" in {
    PresentPath.Gpu("context").describe shouldBe "GL"
  }
