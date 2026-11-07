package com.serenity.ui.skia

import com.serenity.ui.sdl.{PresentPath, SdlError}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** What Skia's GL context does when the window cannot give it the functions it needs. Opening one on a real context is
  * [[SkiaGlContextNativeSpec]].
  */
class SkiaGlContextSpec extends AnyFlatSpec with Matchers:

  "Opening a Skia GL context from a lookup that knows no GL function" should "refuse with a reason a log can carry" in {
    SkiaGlContext.open(_ => 0L) match
      case Left(SdlError.Call(function, reason)) =>
        function shouldBe "SkiaGlContext.open"
        reason should not be empty
      case other => fail(s"expected a refusal, got $other")
  }

  it should "be the raster fallback when the caller chooses how to present" in {
    PresentPath.fromContextAttempt(SkiaGlContext.open(_ => 0L)) match
      case PresentPath.RasterFallback(SdlError.Call(function, _)) => function shouldBe "SkiaGlContext.open"
      case other                                                  => fail(s"expected the raster fallback, got $other")
  }
