package com.serenity.app

import com.serenity.app.RenderBackendSelection.{Inputs, Probe}
import com.serenity.config.RenderBackendSetting
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class RenderBackendSelectionSpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  private val healthyLinux = Inputs(
    osName = "Linux",
    toolkit = ToolkitSelection.Choice.X11,
    x11Display = true,
    probe = Probe.Loaded,
    setting = RenderBackendSetting.Auto,
    skiaSurfaceAvailable = true
  )

  private val variable = RenderBackendSelection.OverrideVariable

  "RenderBackendSelection.choose" should "pick Skia on a healthy X11 Linux under auto" in {
    RenderBackendSelection.choose(healthyLinux).backend shouldBe RenderBackend.Skia
  }

  it should "pick Skia on Windows and macOS without any display or toolkit signal" in {
    val bare = healthyLinux.copy(toolkit = ToolkitSelection.Choice.PlatformDefault, x11Display = false)
    RenderBackendSelection.choose(bare.copy(osName = "Windows 11")).backend shouldBe RenderBackend.Skia
    RenderBackendSelection.choose(bare.copy(osName = "Mac OS X")).backend shouldBe RenderBackend.Skia
  }

  it should "use Java2D under WLToolkit on Linux" in {
    val decision = RenderBackendSelection.choose(healthyLinux.copy(toolkit = ToolkitSelection.Choice.Wayland))
    decision.backend shouldBe RenderBackend.Java2D
    decision.reason should include("WLToolkit")
  }

  it should "say why an explicit skia setting cannot run on Wayland" in {
    val decision = RenderBackendSelection.choose(
      healthyLinux.copy(toolkit = ToolkitSelection.Choice.Wayland, setting = RenderBackendSetting.Skia)
    )
    decision shouldBe RenderBackendSelection.Decision(RenderBackend.Java2D, "skia needs X11; running on Wayland")
  }

  it should "use Java2D on Linux without an X11 display" in {
    val decision = RenderBackendSelection.choose(healthyLinux.copy(x11Display = false))
    decision.backend shouldBe RenderBackend.Java2D
    decision.reason should include("X11 display")
  }

  it should "use Java2D when the native probe failed, naming the cause" in {
    val decision = RenderBackendSelection.choose(healthyLinux.copy(probe = Probe.Failed("libGL.so.1 missing")))
    decision.backend shouldBe RenderBackend.Java2D
    decision.reason should include("libGL.so.1 missing")
  }

  it should "log an explicit skia setting that cannot load as a Java2D fallback with the cause" in {
    val decision = RenderBackendSelection.choose(
      healthyLinux.copy(setting = RenderBackendSetting.Skia, probe = Probe.Failed("no libGL"))
    )
    decision.backend shouldBe RenderBackend.Java2D
    decision.reason should (include("ui.render.backend=skia") and include("no libGL"))
  }

  it should "honour an explicit java2d setting" in {
    val decision = RenderBackendSelection.choose(healthyLinux.copy(setting = RenderBackendSetting.Java2D))
    decision shouldBe RenderBackendSelection.Decision(RenderBackend.Java2D, "ui.render.backend=java2d")
  }

  it should "fall back with 'skia surface unavailable' while no Skia surface is built" in {
    RenderBackendSelection.choose(healthyLinux.copy(skiaSurfaceAvailable = false)) shouldBe
      RenderBackendSelection.Decision(RenderBackend.Java2D, "skia surface unavailable")
  }

  it should "format the decision for the log" in {
    RenderBackendSelection.choose(healthyLinux).logLine should startWith("backend=skia reason=")
    RenderBackendSelection.choose(healthyLinux.copy(setting = RenderBackendSetting.Java2D)).logLine shouldBe
      "backend=java2d reason=ui.render.backend=java2d"
  }

  private val inputsGen: Gen[Inputs] =
    for
      os      <- Gen.oneOf("Linux", "Windows 11", "Mac OS X")
      toolkit <- Gen.oneOf(ToolkitSelection.Choice.values.toSeq)
      display <- Gen.oneOf(true, false)
      probe   <- Gen.oneOf(Probe.Loaded, Probe.Failed("boom"), Probe.NotRun)
      setting <- Gen.oneOf(RenderBackendSetting.values.toSeq)
      surface <- Gen.oneOf(true, false)
    yield Inputs(os, toolkit, display, probe, setting, surface)

  it should "never choose Skia for an explicit java2d setting" in
    forAll(inputsGen)(inputs =>
      RenderBackendSelection
        .choose(inputs.copy(setting = RenderBackendSetting.Java2D))
        .backend shouldBe RenderBackend.Java2D
    )

  it should "never choose Skia unless the probe loaded" in
    forAll(inputsGen) { inputs =>
      if RenderBackendSelection.choose(inputs).backend == RenderBackend.Skia then inputs.probe shouldBe Probe.Loaded
    }

  it should "never choose Skia on Linux under WLToolkit or without an X11 display" in
    forAll(inputsGen) { inputs =>
      val linux = inputs.osName == "Linux"
      if linux && (inputs.toolkit == ToolkitSelection.Choice.Wayland || !inputs.x11Display) then
        RenderBackendSelection.choose(inputs).backend shouldBe RenderBackend.Java2D
    }

  it should "always give a reason" in
    forAll(inputsGen)(inputs => RenderBackendSelection.choose(inputs).reason should not be empty)

  "RenderBackendSelection.probeNeeded" should "ask for a probe exactly when a passing probe would choose Skia" in
    forAll(inputsGen.map(_.copy(probe = Probe.NotRun))) { inputs =>
      RenderBackendSelection.probeNeeded(inputs) shouldBe
        (RenderBackendSelection.choose(inputs.copy(probe = Probe.Loaded)).backend == RenderBackend.Skia)
    }

  "RenderBackendSelection.requested" should "let the environment variable override the setting" in {
    RenderBackendSelection.requested(RenderBackendSetting.Skia, Map(variable -> " Java2D ")) shouldBe
      RenderBackendSetting.Java2D
    RenderBackendSelection.requested(RenderBackendSetting.Java2D, Map(variable -> "skia")) shouldBe
      RenderBackendSetting.Skia
  }

  it should "ignore an unrecognised or empty override" in {
    RenderBackendSelection.requested(RenderBackendSetting.Skia, Map(variable -> "vulkan")) shouldBe
      RenderBackendSetting.Skia
    RenderBackendSelection.requested(RenderBackendSetting.Auto, Map(variable -> "")) shouldBe
      RenderBackendSetting.Auto
  }
