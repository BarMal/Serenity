package com.serenity.app

import com.serenity.app.RenderBackendSelection.{Decision, Inputs, Probe}
import com.serenity.config.{RenderBackendSetting, WaylandRender}
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
    setting = RenderBackendSetting.Skia,
    waylandRender = WaylandRender.SkiaRaster
  )

  private val onWayland = healthyLinux.copy(toolkit = ToolkitSelection.Choice.Wayland)

  private val variable        = RenderBackendSelection.OverrideVariable
  private val waylandVariable = RenderBackendSelection.WaylandOverrideVariable

  "RenderBackendSelection.choose" should "pick Skia on a healthy X11 Linux when skia is requested" in {
    RenderBackendSelection.choose(healthyLinux).backend shouldBe RenderBackend.Skia
  }

  it should "pick Skia on Windows and macOS without any display or toolkit signal when skia is requested" in {
    val bare = healthyLinux.copy(toolkit = ToolkitSelection.Choice.PlatformDefault, x11Display = false)
    RenderBackendSelection.choose(bare.copy(osName = "Windows 11")).backend shouldBe RenderBackend.Skia
    RenderBackendSelection.choose(bare.copy(osName = "Mac OS X")).backend shouldBe RenderBackend.Skia
  }

  it should "pick Skia under auto wherever an explicit skia setting would be picked" in {
    val bare = healthyLinux.copy(toolkit = ToolkitSelection.Choice.PlatformDefault, x11Display = false)
    for inputs <- List(healthyLinux, bare.copy(osName = "Windows 11"), bare.copy(osName = "Mac OS X")) do
      RenderBackendSelection.choose(inputs.copy(setting = RenderBackendSetting.Auto)) shouldBe
        Decision(RenderBackend.Skia, "native probe passed")
  }

  it should "paint with the Skia raster under WLToolkit on Linux by default" in {
    val decision = RenderBackendSelection.choose(onWayland.copy(waylandRender = WaylandRender.SkiaRaster))
    decision shouldBe Decision(RenderBackend.SkiaRaster, "ui.render.wayland=skia-raster")
  }

  it should "use the Skia raster on Linux without an X11 display" in {
    val decision = RenderBackendSelection.choose(healthyLinux.copy(x11Display = false))
    decision.backend shouldBe RenderBackend.SkiaRaster
    decision.reason should include("X11 display")
  }

  it should "fall back to the terminal when the native probe failed, naming the cause" in {
    val decision = RenderBackendSelection.choose(healthyLinux.copy(probe = Probe.Failed("libGL.so.1 missing")))
    decision.backend shouldBe RenderBackend.Terminal
    decision.reason should include("libGL.so.1 missing")
  }

  it should "log an explicit skia setting that cannot load as a terminal fallback with the cause" in {
    val decision = RenderBackendSelection.choose(
      healthyLinux.copy(setting = RenderBackendSetting.Skia, probe = Probe.Failed("no libGL"))
    )
    decision.backend shouldBe RenderBackend.Terminal
    decision.reason should (include("ui.render.backend=skia") and include("no libGL"))
  }

  it should "carry the probe's cause on a terminal decision only when the probe failed" in {
    val failed = RenderBackendSelection.choose(healthyLinux.copy(probe = Probe.Failed("libGL.so.1 missing")))
    failed.skiaFailure shouldBe Some("libGL.so.1 missing")
    RenderBackendSelection
      .choose(healthyLinux.copy(setting = RenderBackendSetting.Skia, probe = Probe.Failed("no libGL")))
      .skiaFailure shouldBe Some("no libGL")
    RenderBackendSelection.choose(healthyLinux).skiaFailure shouldBe None
  }

  it should "carry the wrapped causes with the failure, and keep them out of the reason and the log line" in {
    val decision = RenderBackendSelection.choose(
      healthyLinux.copy(
        setting = RenderBackendSetting.Auto,
        probe = Probe.Failed("Wrapper: failed", Some("Inner: libGL.so.1"))
      )
    )
    decision.skiaFailure shouldBe Some("Wrapper: failed (Inner: libGL.so.1)")
    decision.reason shouldBe "native probe failed: Wrapper: failed"
    decision.logLine shouldBe "backend=terminal reason=native probe failed: Wrapper: failed"
  }

  it should "keep the log line identical whether or not a cause is carried" in {
    val failedProbe = Probe.Failed("libGL.so.1 missing")
    RenderBackendSelection
      .choose(healthyLinux.copy(setting = RenderBackendSetting.Auto, probe = failedProbe))
      .logLine shouldBe "backend=terminal reason=native probe failed: libGL.so.1 missing"
    RenderBackendSelection
      .choose(healthyLinux.copy(probe = failedProbe))
      .logLine shouldBe "backend=terminal reason=ui.render.backend=skia could not load: libGL.so.1 missing"
  }

  it should "format the decision for the log" in {
    RenderBackendSelection.choose(healthyLinux).logLine should startWith("backend=skia reason=")
    RenderBackendSelection.choose(healthyLinux.copy(setting = RenderBackendSetting.SkiaRaster)).logLine shouldBe
      "backend=skia-raster reason=ui.render.backend=skia-raster"
  }

  "RenderBackendSelection.choose on Wayland" should "choose the raster backend for wayland render skia-raster once the probe loaded" in {
    val raster = onWayland.copy(waylandRender = WaylandRender.SkiaRaster)
    for setting <- List(RenderBackendSetting.Auto, RenderBackendSetting.Skia) do
      RenderBackendSelection.choose(raster.copy(setting = setting)).backend shouldBe RenderBackend.SkiaRaster
    RenderBackendSelection.choose(raster).logLine should startWith("backend=skia-raster reason=")
  }

  it should "need no X11 display for the raster backend" in {
    RenderBackendSelection
      .choose(onWayland.copy(waylandRender = WaylandRender.SkiaRaster, x11Display = false))
      .backend shouldBe RenderBackend.SkiaRaster
  }

  it should "fall back to the terminal when the raster probe fails" in {
    val decision = RenderBackendSelection.choose(
      onWayland.copy(waylandRender = WaylandRender.SkiaRaster, probe = Probe.Failed("no natives"))
    )
    decision.backend shouldBe RenderBackend.Terminal
    decision.reason should include("no natives")
  }

  it should "use the Skia raster for xwayland when WLToolkit is still in force, because the toolkit was named explicitly" in {
    val decision = RenderBackendSelection.choose(onWayland.copy(waylandRender = WaylandRender.XWayland))
    decision.backend shouldBe RenderBackend.SkiaRaster
    decision.reason should include("xwayland")
  }

  it should "choose Skia for xwayland once the toolkit is X11" in {
    RenderBackendSelection
      .choose(healthyLinux.copy(waylandRender = WaylandRender.XWayland))
      .backend shouldBe RenderBackend.Skia
  }

  it should "ignore the wayland render under an X11 toolkit and off Linux" in {
    for mode <- WaylandRender.values do
      RenderBackendSelection.choose(healthyLinux.copy(waylandRender = mode)).backend shouldBe RenderBackend.Skia
    val windows = healthyLinux.copy(osName = "Windows 11", toolkit = ToolkitSelection.Choice.Wayland)
    for mode <- WaylandRender.values do
      RenderBackendSelection.choose(windows.copy(waylandRender = mode)).backend shouldBe RenderBackend.Skia
  }

  private val explicitRaster = healthyLinux.copy(setting = RenderBackendSetting.SkiaRaster)

  it should "paint with the Skia raster wherever it can, when skia-raster is requested" in {
    val reason = "ui.render.backend=skia-raster"
    RenderBackendSelection.choose(explicitRaster) shouldBe Decision(RenderBackend.SkiaRaster, reason)
    RenderBackendSelection.choose(explicitRaster.copy(toolkit = ToolkitSelection.Choice.Wayland)) shouldBe
      Decision(RenderBackend.SkiaRaster, reason)
    RenderBackendSelection.choose(explicitRaster.copy(x11Display = false)) shouldBe
      Decision(RenderBackend.SkiaRaster, reason)
    RenderBackendSelection.choose(explicitRaster.copy(osName = "Windows 11")) shouldBe
      Decision(RenderBackend.SkiaRaster, reason)
    for mode <- WaylandRender.values do
      RenderBackendSelection.choose(explicitRaster.copy(waylandRender = mode)).backend shouldBe
        RenderBackend.SkiaRaster
  }

  it should "name skia-raster and the cause when its native library will not load" in {
    val decision = RenderBackendSelection.choose(explicitRaster.copy(probe = Probe.Failed("no libGL")))
    decision.backend shouldBe RenderBackend.Terminal
    decision.reason should (include("ui.render.backend=skia-raster") and include("no libGL"))
  }

  it should "never choose the raster for skia-raster unless the probe loaded" in {
    RenderBackendSelection.choose(explicitRaster.copy(probe = Probe.Failed("boom"))).backend shouldBe
      RenderBackend.Terminal
  }

  private val inputsGen: Gen[Inputs] =
    for
      os      <- Gen.oneOf("Linux", "Windows 11", "Mac OS X")
      toolkit <- Gen.oneOf(ToolkitSelection.Choice.values.toSeq)
      display <- Gen.oneOf(true, false)
      probe   <- Gen.oneOf(Probe.Loaded, Probe.Failed("boom"))
      setting <- Gen.oneOf(RenderBackendSetting.values.toSeq)
      wayland <- Gen.oneOf(WaylandRender.values.toSeq)
    yield Inputs(os, toolkit, display, probe, setting, wayland)

  it should "decide every backend setting x wayland render x toolkit combination without a gap" in {
    val combinations = for
      setting <- RenderBackendSetting.values.toList
      mode    <- WaylandRender.values.toList
      toolkit <- ToolkitSelection.Choice.values.toList
    yield RenderBackendSelection.choose(healthyLinux.copy(setting = setting, waylandRender = mode, toolkit = toolkit))
    combinations should have size
      (RenderBackendSetting.values.length * WaylandRender.values.length * ToolkitSelection.Choice.values.length)
    combinations.foreach(_.reason should not be empty)
  }

  it should "never choose a Skia backend unless the probe loaded" in
    forAll(inputsGen) { inputs =>
      if RenderBackendSelection.choose(inputs).backend != RenderBackend.Terminal then inputs.probe shouldBe Probe.Loaded
    }

  it should "never choose direct Skia on Linux under WLToolkit or without an X11 display" in
    forAll(inputsGen) { inputs =>
      val linux = inputs.osName == "Linux"
      if linux && (inputs.toolkit == ToolkitSelection.Choice.Wayland || !inputs.x11Display) then
        RenderBackendSelection.choose(inputs).backend should not be RenderBackend.Skia
    }

  it should "choose the raster backend only on Linux, for WLToolkit or no X11 display" in
    forAll(inputsGen) { inputs =>
      if inputs.setting != RenderBackendSetting.SkiaRaster &&
          RenderBackendSelection.choose(inputs).backend == RenderBackend.SkiaRaster
      then
        val wayland = inputs.toolkit == ToolkitSelection.Choice.Wayland
        (inputs.osName, wayland || !inputs.x11Display) shouldBe ("Linux", true)
    }

  it should "always give a reason" in
    forAll(inputsGen)(inputs => RenderBackendSelection.choose(inputs).reason should not be empty)

  "RenderBackendSelection.requested" should "let the environment variable override the setting" in {
    RenderBackendSelection.requested(RenderBackendSetting.Skia, Map(variable -> " Skia-Raster "), None) shouldBe
      RenderBackendSetting.SkiaRaster
    RenderBackendSelection.requested(RenderBackendSetting.SkiaRaster, Map(variable -> "skia"), None) shouldBe
      RenderBackendSetting.Skia
  }

  it should "let the command line flag beat the environment variable and the file" in {
    RenderBackendSelection.requested(
      RenderBackendSetting.Skia,
      Map(variable -> "skia"),
      Some(RenderBackendSetting.SkiaRaster)
    ) shouldBe RenderBackendSetting.SkiaRaster
  }

  it should "ignore an unrecognised or empty override" in {
    RenderBackendSelection.requested(RenderBackendSetting.Skia, Map(variable -> "vulkan"), None) shouldBe
      RenderBackendSetting.Skia
    RenderBackendSelection.requested(RenderBackendSetting.Auto, Map(variable -> ""), None) shouldBe
      RenderBackendSetting.Auto
  }

  "RenderBackendSelection.requestedWayland" should "apply flag, then environment variable, then file" in {
    val fromFile = WaylandRender.XWayland
    RenderBackendSelection.requestedWayland(fromFile, Map.empty, None) shouldBe WaylandRender.XWayland
    RenderBackendSelection.requestedWayland(fromFile, Map(waylandVariable -> " Skia-Raster "), None) shouldBe
      WaylandRender.SkiaRaster
    RenderBackendSelection.requestedWayland(
      fromFile,
      Map(waylandVariable -> "xwayland"),
      Some(WaylandRender.SkiaRaster)
    ) shouldBe WaylandRender.SkiaRaster
    RenderBackendSelection.requestedWayland(fromFile, Map(waylandVariable -> "nonsense"), None) shouldBe fromFile
  }
