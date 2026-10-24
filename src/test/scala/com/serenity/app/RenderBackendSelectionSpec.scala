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
    skiaSurfaceAvailable = true,
    waylandRender = WaylandRender.Java2D,
    rasterSurfaceAvailable = true
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

  it should "use Java2D under auto everywhere until Skia is measured faster" in {
    val bare = healthyLinux.copy(toolkit = ToolkitSelection.Choice.PlatformDefault, x11Display = false)
    val auto = healthyLinux.copy(setting = RenderBackendSetting.Auto)
    for inputs <- List(auto, bare.copy(osName = "Windows 11"), bare.copy(osName = "Mac OS X")) do
      RenderBackendSelection.choose(inputs.copy(setting = RenderBackendSetting.Auto)) shouldBe
        Decision(RenderBackend.Java2D, "auto uses Java2D until Skia is measured faster")
  }

  it should "need the native probe only when skia is requested or a Wayland mode asks for Skia" in {
    val unprobed = healthyLinux.copy(probe = Probe.NotRun)
    RenderBackendSelection.probeNeeded(unprobed.copy(setting = RenderBackendSetting.Auto)) shouldBe false
    RenderBackendSelection.probeNeeded(unprobed) shouldBe true
    RenderBackendSelection.probeNeeded(
      onWayland.copy(probe = Probe.NotRun, waylandRender = WaylandRender.SkiaRaster)
    ) shouldBe true
  }

  it should "use Java2D under WLToolkit on Linux by default" in {
    val decision = RenderBackendSelection.choose(onWayland.copy(setting = RenderBackendSetting.Auto))
    decision.backend shouldBe RenderBackend.Java2D
    decision.reason should include("WLToolkit")
  }

  it should "say why an explicit skia setting cannot run on Wayland" in {
    RenderBackendSelection.choose(onWayland.copy(setting = RenderBackendSetting.Skia)) shouldBe
      Decision(RenderBackend.Java2D, "skia needs X11; running on Wayland")
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
    RenderBackendSelection.choose(healthyLinux.copy(setting = RenderBackendSetting.Java2D)) shouldBe
      Decision(RenderBackend.Java2D, "ui.render.backend=java2d")
  }

  it should "fall back with 'skia surface unavailable' while no Skia surface is built" in {
    RenderBackendSelection.choose(healthyLinux.copy(skiaSurfaceAvailable = false)) shouldBe
      Decision(RenderBackend.Java2D, "skia surface unavailable")
  }

  it should "format the decision for the log" in {
    RenderBackendSelection.choose(healthyLinux).logLine should startWith("backend=skia reason=")
    RenderBackendSelection.choose(healthyLinux.copy(setting = RenderBackendSetting.Java2D)).logLine shouldBe
      "backend=java2d reason=ui.render.backend=java2d"
  }

  "RenderBackendSelection.choose on Wayland" should "keep WLToolkit and Java2D for wayland render java2d" in {
    for setting <- List(RenderBackendSetting.Auto, RenderBackendSetting.Skia) do
      RenderBackendSelection
        .choose(onWayland.copy(setting = setting, waylandRender = WaylandRender.Java2D))
        .backend shouldBe RenderBackend.Java2D
  }

  it should "choose the raster backend for wayland render skia-raster once the probe loaded" in {
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

  it should "fall back with 'skia raster surface unavailable' when no raster factory is registered" in {
    RenderBackendSelection.choose(
      onWayland.copy(waylandRender = WaylandRender.SkiaRaster, rasterSurfaceAvailable = false)
    ) shouldBe Decision(RenderBackend.Java2D, "skia raster surface unavailable")
  }

  it should "fall back to Java2D when the raster probe fails" in {
    val decision = RenderBackendSelection.choose(
      onWayland.copy(waylandRender = WaylandRender.SkiaRaster, probe = Probe.Failed("no natives"))
    )
    decision.backend shouldBe RenderBackend.Java2D
    decision.reason should include("no natives")
  }

  it should "stay on Java2D for an explicit java2d setting whatever the wayland render" in {
    for mode <- WaylandRender.values do
      RenderBackendSelection
        .choose(onWayland.copy(setting = RenderBackendSetting.Java2D, waylandRender = mode))
        .backend shouldBe RenderBackend.Java2D
  }

  it should "use Java2D for xwayland when WLToolkit is still in force, because the toolkit was named explicitly" in {
    val decision = RenderBackendSelection.choose(onWayland.copy(waylandRender = WaylandRender.XWayland))
    decision.backend shouldBe RenderBackend.Java2D
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

  private val inputsGen: Gen[Inputs] =
    for
      os      <- Gen.oneOf("Linux", "Windows 11", "Mac OS X")
      toolkit <- Gen.oneOf(ToolkitSelection.Choice.values.toSeq)
      display <- Gen.oneOf(true, false)
      probe   <- Gen.oneOf(Probe.Loaded, Probe.Failed("boom"), Probe.NotRun)
      setting <- Gen.oneOf(RenderBackendSetting.values.toSeq)
      surface <- Gen.oneOf(true, false)
      wayland <- Gen.oneOf(WaylandRender.values.toSeq)
      raster  <- Gen.oneOf(true, false)
    yield Inputs(os, toolkit, display, probe, setting, surface, wayland, raster)

  it should "decide every backend setting x wayland render x toolkit combination without a gap" in {
    val combinations = for
      setting <- RenderBackendSetting.values.toList
      mode    <- WaylandRender.values.toList
      toolkit <- ToolkitSelection.Choice.values.toList
    yield RenderBackendSelection.choose(healthyLinux.copy(setting = setting, waylandRender = mode, toolkit = toolkit))
    combinations should have size 27
    combinations.foreach(_.reason should not be empty)
  }

  it should "never choose a Skia backend for an explicit java2d setting" in
    forAll(inputsGen)(inputs =>
      RenderBackendSelection
        .choose(inputs.copy(setting = RenderBackendSetting.Java2D))
        .backend shouldBe RenderBackend.Java2D
    )

  it should "never choose a Skia backend unless the probe loaded" in
    forAll(inputsGen) { inputs =>
      if RenderBackendSelection.choose(inputs).backend != RenderBackend.Java2D then inputs.probe shouldBe Probe.Loaded
    }

  it should "never choose direct Skia on Linux under WLToolkit or without an X11 display" in
    forAll(inputsGen) { inputs =>
      val linux = inputs.osName == "Linux"
      if linux && (inputs.toolkit == ToolkitSelection.Choice.Wayland || !inputs.x11Display) then
        RenderBackendSelection.choose(inputs).backend should not be RenderBackend.Skia
    }

  it should "choose the raster backend only under WLToolkit on Linux with skia-raster and a raster factory" in
    forAll(inputsGen) { inputs =>
      if RenderBackendSelection.choose(inputs).backend == RenderBackend.SkiaRaster then
        (
          inputs.osName,
          inputs.toolkit,
          inputs.waylandRender,
          inputs.rasterSurfaceAvailable
        ) shouldBe ("Linux", ToolkitSelection.Choice.Wayland, WaylandRender.SkiaRaster, true)
    }

  it should "always give a reason" in
    forAll(inputsGen)(inputs => RenderBackendSelection.choose(inputs).reason should not be empty)

  "RenderBackendSelection.probeNeeded" should "ask for a probe exactly when a passing probe would choose Skia" in
    forAll(inputsGen.map(_.copy(probe = Probe.NotRun))) { inputs =>
      RenderBackendSelection.probeNeeded(inputs) shouldBe
        (RenderBackendSelection.choose(inputs.copy(probe = Probe.Loaded)).backend != RenderBackend.Java2D)
    }

  "RenderBackendSelection.requested" should "let the environment variable override the setting" in {
    RenderBackendSelection.requested(RenderBackendSetting.Skia, Map(variable -> " Java2D "), None) shouldBe
      RenderBackendSetting.Java2D
    RenderBackendSelection.requested(RenderBackendSetting.Java2D, Map(variable -> "skia"), None) shouldBe
      RenderBackendSetting.Skia
  }

  it should "let the command line flag beat the environment variable and the file" in {
    RenderBackendSelection.requested(
      RenderBackendSetting.Skia,
      Map(variable -> "skia"),
      Some(RenderBackendSetting.Java2D)
    ) shouldBe RenderBackendSetting.Java2D
  }

  it should "ignore an unrecognised or empty override" in {
    RenderBackendSelection.requested(RenderBackendSetting.Skia, Map(variable -> "vulkan"), None) shouldBe
      RenderBackendSetting.Skia
    RenderBackendSelection.requested(RenderBackendSetting.Auto, Map(variable -> ""), None) shouldBe
      RenderBackendSetting.Auto
  }

  "RenderBackendSelection.requestedWayland" should "apply flag, then environment variable, then file" in {
    val fromFile = WaylandRender.Java2D
    RenderBackendSelection.requestedWayland(fromFile, Map.empty, None) shouldBe WaylandRender.Java2D
    RenderBackendSelection.requestedWayland(fromFile, Map(waylandVariable -> " XWayland "), None) shouldBe
      WaylandRender.XWayland
    RenderBackendSelection.requestedWayland(
      fromFile,
      Map(waylandVariable -> "xwayland"),
      Some(WaylandRender.SkiaRaster)
    ) shouldBe WaylandRender.SkiaRaster
    RenderBackendSelection.requestedWayland(fromFile, Map(waylandVariable -> "nonsense"), None) shouldBe fromFile
  }
