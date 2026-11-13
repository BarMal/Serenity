package com.serenity.app

import com.serenity.app.RenderBackendSelection.{Decision, Inputs, Probe}
import com.serenity.config.{RenderBackendSetting, WaylandRender}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SkiaFallbackSpec extends AnyFlatSpec with Matchers:

  given com.serenity.rope.Balance = com.serenity.rope.Balance.default

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

  it should "give a header that names the library the loader could not find" in {
    val text = SkiaFallback
      .header(RenderBackendSelection.choose(failedInputs(SkiaFallback.failure(skikoLoadFailure))))
      .getOrElse(fail("a failed probe must produce a header"))
    text should include("libGL.so.1")
  }

  "SkiaFallback.header" should "stay silent when Skia was never expected to run" in {
    SkiaFallback.header(Decision(RenderBackend.Terminal, "terminal or headless launch")) shouldBe None
    SkiaFallback.header(Decision(RenderBackend.Skia, "native probe passed")) shouldBe None
  }

  it should "say that Skia could not load and that Serenity is running in the terminal, naming a missing library" in {
    SkiaFallback.header(failed(missingLibGl)) shouldBe
      Some("Skia could not load: the system library libGL.so.1 is missing. Running in the terminal.")
  }

  it should "quote the cause when no missing library can be named" in {
    SkiaFallback.header(failed("no raster surface")) shouldBe
      Some("Skia could not load: no raster surface. Running in the terminal.")
  }

  it should "be one line, whatever the cause says" in {
    SkiaFallback
      .header(failed("first line\nsecond line"))
      .getOrElse(fail("a failed probe must produce a header")) should
      not include "\n"
  }

  "StartupRecovery.Plan.withSkiaFallback" should "carry the header, and leave the start-page notices alone" in {
    val plan   = StartupRecovery.Plan(safeMode = false, notices = List("Earlier notice."))
    val withIt = plan.withSkiaFallback(failed(missingLibGl))
    withIt.fallbackHeader shouldBe SkiaFallback.header(failed(missingLibGl))
    withIt.noticeWith(Some("Config notice.")) shouldBe Some("Earlier notice. Config notice.")
  }

  it should "add nothing when Skia was not expected to run or loaded" in {
    val plan = StartupRecovery.Plan(safeMode = false, notices = Nil)
    plan.withSkiaFallback(Decision(RenderBackend.Terminal, "terminal or headless launch")) shouldBe plan
    plan.withSkiaFallback(Decision(RenderBackend.Skia, "native probe passed")) shouldBe plan
  }

  "StartupRecovery.Plan.appliedTo" should "show the header as one floating top-edge surface, and nothing without one" in {
    val base = com.serenity.state.models.AppState.initial
    val shown =
      StartupRecovery.Plan(safeMode = false, notices = Nil).withSkiaFallback(failed(missingLibGl)).appliedTo(base)
    shown.runtime.uiSurfaces.map(_.content) shouldBe
      List(
        com.serenity.state.models.SurfaceContent.FallbackHeader(
          SkiaFallback.header(failed(missingLibGl)).getOrElse(fail("a failed probe must produce a header"))
        )
      )
    StartupRecovery.Plan.normal.appliedTo(base).runtime.uiSurfaces shouldBe Nil
  }
