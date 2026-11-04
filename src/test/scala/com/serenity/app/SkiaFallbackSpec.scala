package com.serenity.app

import com.serenity.app.RenderBackendSelection.Decision
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SkiaFallbackSpec extends AnyFlatSpec with Matchers:

  private val missingLibGl =
    "UnsatisfiedLinkError: /home/me/.skiko/abc/libskiko-linux-x64.so: libGL.so.1: " +
      "cannot open shared object file: No such file or directory"

  private def failed(cause: String): Decision =
    Decision(RenderBackend.Java2D, s"native probe failed: $cause", skiaFailure = Some(cause))

  "SkiaFallback.missingLibrary" should "name the library the dynamic loader could not find" in {
    SkiaFallback.missingLibrary(missingLibGl) shouldBe Some("libGL.so.1")
  }

  it should "give a missing Skiko library by its file name, without the directory" in {
    SkiaFallback.missingLibrary(
      "UnsatisfiedLinkError: /x/libskiko-linux-x64.so: cannot open shared object file: No such file or directory"
    ) shouldBe Some("libskiko-linux-x64.so")
  }

  it should "find nothing in a failure that is not a missing library" in {
    SkiaFallback.missingLibrary("no raster surface") shouldBe None
    SkiaFallback.missingLibrary("NoClassDefFoundError: org/jetbrains/skia/Surface") shouldBe None
  }

  "SkiaFallback.notice" should "stay silent when Skia was never expected to run" in {
    SkiaFallback.notice(Decision(RenderBackend.Java2D, "ui.render.backend=java2d")) shouldBe None
    SkiaFallback.notice(Decision(RenderBackend.Java2D, "skia surface unavailable")) shouldBe None
    SkiaFallback.notice(Decision(RenderBackend.Java2D, "safe mode")) shouldBe None
    SkiaFallback.notice(Decision(RenderBackend.Skia, "native probe passed")) shouldBe None
  }

  it should "say why Skia fell back and how to fix a missing library" in {
    val text = SkiaFallback.notice(failed(missingLibGl)).getOrElse(fail("a failed probe must produce a notice"))
    text should include("Java2D")
    text should include("libGL.so.1")
    text should include("LD_LIBRARY_PATH")
    text should include("Skia on NixOS")
    text should include("ui.render.backend = java2d")
  }

  it should "quote the cause when no missing library can be named" in {
    val text = SkiaFallback.notice(failed("no raster surface")).getOrElse(fail("a failed probe must produce a notice"))
    text should include("no raster surface")
    text should include("[RENDER]")
    text should not include "LD_LIBRARY_PATH"
  }
