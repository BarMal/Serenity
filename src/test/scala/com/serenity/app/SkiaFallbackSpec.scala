package com.serenity.app

import com.serenity.app.RenderBackendSelection.{Decision, Inputs, Probe}
import com.serenity.config.{RenderBackendSetting, WaylandRender}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SkiaFallbackSpec extends AnyFlatSpec with Matchers:

  private val missingLibGl =
    "UnsatisfiedLinkError: /home/me/.skiko/abc/libskiko-linux-x64.so: libGL.so.1: " +
      "cannot open shared object file: No such file or directory"

  private def failedInputs(probe: Probe): Inputs =
    Inputs(
      osName = "Linux",
      toolkit = ToolkitSelection.Choice.PlatformDefault,
      x11Display = true,
      probe = probe,
      setting = RenderBackendSetting.Auto,
      waylandRender = WaylandRender.SkiaRaster
    )

  private def failed(cause: String): Decision =
    Decision(RenderBackend.Terminal, s"native probe failed: $cause", skiaFailure = Some(cause))

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

  // What Skiko throws on the laptop (NixOS, no LD_LIBRARY_PATH): the wrapper's message names only the Skiko library;
  // the missing system library is in the UnsatisfiedLinkError it wraps.
  private def skikoLoadFailure: Throwable =
    new RuntimeException(
      "Failed to loade library /home/me/.skiko/abc/libskiko-linux-x64.so",
      new UnsatisfiedLinkError(
        "/home/me/.skiko/abc/libskiko-linux-x64.so: libGL.so.1: cannot open shared object file: " +
          "No such file or directory"
      )
    )

  "SkiaFallback.failure" should "keep the top-level description and add the causes it wraps" in {
    SkiaFallback.failure(skikoLoadFailure) shouldBe Probe.Failed(
      "RuntimeException: Failed to loade library /home/me/.skiko/abc/libskiko-linux-x64.so",
      Some(
        "UnsatisfiedLinkError: /home/me/.skiko/abc/libskiko-linux-x64.so: libGL.so.1: " +
          "cannot open shared object file: No such file or directory"
      )
    )
  }

  it should "add no detail to a failure that wraps nothing, and say when there is no message" in {
    SkiaFallback.failure(new IllegalStateException()) shouldBe
      Probe.Failed("IllegalStateException: no message", None)
  }

  it should "stop at a cause chain that loops" in {
    val looping = new RuntimeException("a"):
      override def getCause: Throwable = this
    SkiaFallback.failure(looping).detail shouldBe None
  }

  it should "give a notice that names the library the loader could not find" in {
    val text = SkiaFallback
      .notice(RenderBackendSelection.choose(failedInputs(SkiaFallback.failure(skikoLoadFailure))))
      .getOrElse(fail("a failed probe must produce a notice"))
    text should include("libGL.so.1")
    text should include("LD_LIBRARY_PATH")
  }

  "SkiaFallback.notice" should "stay silent when Skia was never expected to run" in {
    SkiaFallback.notice(Decision(RenderBackend.Terminal, "terminal or headless launch")) shouldBe None
    SkiaFallback.notice(Decision(RenderBackend.Skia, "native probe passed")) shouldBe None
  }

  it should "say why Skia fell back and how to fix a missing library" in {
    val text = SkiaFallback.notice(failed(missingLibGl)).getOrElse(fail("a failed probe must produce a notice"))
    text should include("terminal")
    text should not include "Java2D"
    text should include("libGL.so.1")
    text should include("LD_LIBRARY_PATH")
    text should include("Skia on NixOS")
    text should not include "java2d"
  }

  it should "quote the cause when no missing library can be named" in {
    val text = SkiaFallback.notice(failed("no raster surface")).getOrElse(fail("a failed probe must produce a notice"))
    text should include("no raster surface")
    text should include("[RENDER]")
    text should not include "LD_LIBRARY_PATH"
  }

  "StartupRecovery.Plan.withSkiaFallback" should "put the Skia notice on the start page after the earlier notices" in {
    val plan  = StartupRecovery.Plan(safeMode = false, notices = List("Earlier notice."))
    val skia  = SkiaFallback.notice(failed(missingLibGl)).getOrElse(fail("a failed probe must produce a notice"))
    val shown = plan.withSkiaFallback(failed(missingLibGl)).noticeWith(Some("Config notice."))
    shown shouldBe Some(s"Earlier notice. $skia Config notice.")
  }

  it should "add nothing when Skia was not expected to run or loaded" in {
    val plan = StartupRecovery.Plan(safeMode = false, notices = Nil)
    plan.withSkiaFallback(Decision(RenderBackend.Terminal, "terminal or headless launch")) shouldBe plan
    plan.withSkiaFallback(Decision(RenderBackend.Skia, "native probe passed")) shouldBe plan
  }
