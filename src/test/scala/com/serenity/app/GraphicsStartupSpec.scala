package com.serenity.app

import java.util.concurrent.atomic.AtomicInteger

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.app.RenderBackendSelection.Probe
import com.serenity.config.{RenderBackendSetting, RenderStartupSettings, WaylandRender}
import com.serenity.ui.renderer.FrameSurfaceFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GraphicsStartupSpec extends AnyFlatSpec with Matchers:

  private val jbr = Map("java.vendor" -> "JetBrains s.r.o.", "java.vm.vendor" -> "JetBrains s.r.o.")

  private val x11Env = ToolkitSelection.Environment("Linux", Map.empty, Map("DISPLAY" -> ":0"), false, None)

  private val waylandEnv = ToolkitSelection.Environment(
    "Linux",
    jbr,
    Map("DISPLAY" -> ":0", "WAYLAND_DISPLAY" -> "wayland-1"),
    waylandToolkitAvailable = true,
    waylandSocketProblem = None
  )

  final private class Counters:
    val probes: AtomicInteger = new AtomicInteger(0)
    val peeks: AtomicInteger  = new AtomicInteger(0)
    val built: AtomicInteger  = new AtomicInteger(0)

  private def resolve(
    counters: Counters,
    launch: GraphicsStartup.Launch = GraphicsStartup.Launch(windowed = true, safeMode = false),
    environment: ToolkitSelection.Environment = x11Env,
    configured: RenderStartupSettings = RenderStartupSettings(RenderBackendSetting.Auto, WaylandRender.Java2D),
    probe: Probe = Probe.Loaded,
    hasSkiaSurface: Boolean = true,
    hasRasterSurface: Boolean = true
  ): GraphicsStartup.Resolved =
    def factory(): FrameSurfaceFactory =
      counters.built.incrementAndGet()
      FrameSurfaceFactory.Java2D
    GraphicsStartup
      .resolve(
        launch,
        environment,
        SurfaceCatalogue(Option.when(hasSkiaSurface)(() => factory()), Option.when(hasRasterSurface)(() => factory())),
        IO(counters.peeks.incrementAndGet()).as(configured),
        IO(counters.probes.incrementAndGet()).as(probe)
      )
      .unsafeRunSync()

  private def settings(backend: RenderBackendSetting, wayland: WaylandRender): RenderStartupSettings =
    RenderStartupSettings(backend, wayland)

  "GraphicsStartup.resolve" should "choose Skia and build its factory when everything allows it" in {
    val counters = new Counters
    val resolved = resolve(counters)
    resolved.backend.backend shouldBe RenderBackend.Skia
    counters.probes.get shouldBe 1
    counters.built.get shouldBe 1
  }

  it should "not probe, read the config or build Skia for a terminal launch" in {
    val counters = new Counters
    val resolved = resolve(counters, launch = GraphicsStartup.Launch(windowed = false, safeMode = false))
    resolved.backend.backend shouldBe RenderBackend.Java2D
    (counters.probes.get, counters.peeks.get, counters.built.get) shouldBe (0, 0, 0)
  }

  it should "still settle the toolkit for a terminal launch" in {
    val resolved = resolve(
      new Counters,
      launch = GraphicsStartup.Launch(windowed = false, safeMode = false),
      environment = waylandEnv
    )
    resolved.toolkit.choice shouldBe ToolkitSelection.Choice.Wayland
  }

  it should "stay on Java2D in safe mode without probing" in {
    val counters = new Counters
    val resolved = resolve(counters, launch = GraphicsStartup.Launch(windowed = true, safeMode = true))
    resolved.backend.backend shouldBe RenderBackend.Java2D
    counters.probes.get shouldBe 0
  }

  it should "not probe when the setting is java2d" in {
    val counters = new Counters
    val java2d   = settings(RenderBackendSetting.Java2D, WaylandRender.Java2D)
    resolve(counters, configured = java2d).backend.backend shouldBe RenderBackend.Java2D
    counters.probes.get shouldBe 0
    counters.built.get shouldBe 0
  }

  it should "not probe when there is no X11 display" in {
    val counters = new Counters
    resolve(counters, environment = x11Env.copy(env = Map.empty)).backend.backend shouldBe RenderBackend.Java2D
    counters.probes.get shouldBe 0
  }

  it should "fall back to Java2D when the probe fails" in {
    val counters = new Counters
    val resolved = resolve(counters, probe = Probe.Failed("no libGL"))
    resolved.backend.backend shouldBe RenderBackend.Java2D
    resolved.backend.reason should include("no libGL")
    counters.built.get shouldBe 0
  }

  it should "fall back with 'skia surface unavailable' when no Skia surface exists" in {
    val counters = new Counters
    resolve(counters, hasSkiaSurface = false).backend.reason shouldBe "skia surface unavailable"
    counters.probes.get shouldBe 0
  }

  it should "let the environment variable beat the configured setting" in {
    val env = x11Env.copy(env = x11Env.env + (RenderBackendSelection.OverrideVariable -> "java2d"))
    resolve(new Counters, environment = env).backend.backend shouldBe RenderBackend.Java2D
  }

  it should "let the --renderer flag beat the environment variable" in {
    val env    = x11Env.copy(env = x11Env.env + (RenderBackendSelection.OverrideVariable -> "java2d"))
    val launch = GraphicsStartup.Launch(windowed = true, safeMode = false, renderer = Some(RenderBackendSetting.Skia))
    resolve(new Counters, launch = launch, environment = env).backend.backend shouldBe RenderBackend.Skia
  }

  "GraphicsStartup.resolve on a Wayland session" should "keep WLToolkit and Java2D by default, without probing" in {
    val counters = new Counters
    val resolved = resolve(counters, environment = waylandEnv)
    resolved.toolkit.choice shouldBe ToolkitSelection.Choice.Wayland
    resolved.backend.backend shouldBe RenderBackend.Java2D
    counters.probes.get shouldBe 0
  }

  it should "name the Wayland reason for an explicit skia setting under WLToolkit" in {
    val explicit = settings(RenderBackendSetting.Skia, WaylandRender.Java2D)
    resolve(new Counters, environment = waylandEnv, configured = explicit).backend.reason shouldBe
      "skia needs X11; running on Wayland"
  }

  it should "force XToolkit before AWT and choose Skia for ui.render.wayland=xwayland" in {
    val counters = new Counters
    val xwayland = settings(RenderBackendSetting.Auto, WaylandRender.XWayland)
    val resolved = resolve(counters, environment = waylandEnv, configured = xwayland)
    resolved.toolkit.choice shouldBe ToolkitSelection.Choice.X11
    resolved.backend.backend shouldBe RenderBackend.Skia
    counters.probes.get shouldBe 1
  }

  it should "force XToolkit from the --wayland-render flag and the environment variable too" in {
    val flag = GraphicsStartup.Launch(windowed = true, safeMode = false, waylandRender = Some(WaylandRender.XWayland))
    resolve(new Counters, launch = flag, environment = waylandEnv).toolkit.choice shouldBe ToolkitSelection.Choice.X11
    val env = waylandEnv.copy(env = waylandEnv.env + (RenderBackendSelection.WaylandOverrideVariable -> "xwayland"))
    resolve(new Counters, environment = env).toolkit.choice shouldBe ToolkitSelection.Choice.X11
  }

  it should "let --wayland-render=java2d beat an xwayland setting and keep WLToolkit" in {
    val flag     = GraphicsStartup.Launch(windowed = true, safeMode = false, waylandRender = Some(WaylandRender.Java2D))
    val xwayland = settings(RenderBackendSetting.Auto, WaylandRender.XWayland)
    val resolved = resolve(new Counters, launch = flag, environment = waylandEnv, configured = xwayland)
    resolved.toolkit.choice shouldBe ToolkitSelection.Choice.Wayland
    resolved.backend.backend shouldBe RenderBackend.Java2D
  }

  it should "leave the toolkit on Wayland when the xwayland probe fails" in {
    val xwayland = settings(RenderBackendSetting.Auto, WaylandRender.XWayland)
    val resolved = resolve(new Counters, environment = waylandEnv, configured = xwayland, probe = Probe.Failed("x"))
    resolved.toolkit.choice shouldBe ToolkitSelection.Choice.Wayland
    resolved.backend.backend shouldBe RenderBackend.Java2D
  }

  it should "not force XToolkit when the backend is java2d or no Skia surface exists" in {
    val xwaylandJava2d = settings(RenderBackendSetting.Java2D, WaylandRender.XWayland)
    resolve(new Counters, environment = waylandEnv, configured = xwaylandJava2d).toolkit.choice shouldBe
      ToolkitSelection.Choice.Wayland
    val xwayland = settings(RenderBackendSetting.Auto, WaylandRender.XWayland)
    resolve(
      new Counters,
      environment = waylandEnv,
      configured = xwayland,
      hasSkiaSurface = false
    ).toolkit.choice shouldBe
      ToolkitSelection.Choice.Wayland
  }

  it should "not force XToolkit without an X11 display" in {
    val xwayland = settings(RenderBackendSetting.Auto, WaylandRender.XWayland)
    val noX11    = waylandEnv.copy(env = Map("WAYLAND_DISPLAY" -> "wayland-1"))
    resolve(new Counters, environment = noX11, configured = xwayland).toolkit.choice shouldBe
      ToolkitSelection.Choice.Wayland
  }

  it should "let SERENITY_TOOLKIT=wayland win over xwayland, falling back to Java2D" in {
    val xwayland = settings(RenderBackendSetting.Auto, WaylandRender.XWayland)
    val env      = waylandEnv.copy(env = waylandEnv.env + ("SERENITY_TOOLKIT" -> "wayland"))
    val resolved = resolve(new Counters, environment = env, configured = xwayland)
    resolved.toolkit.choice shouldBe ToolkitSelection.Choice.Wayland
    resolved.backend.backend shouldBe RenderBackend.Java2D
  }

  it should "let a toolkit named on the command line win over xwayland" in {
    val xwayland = settings(RenderBackendSetting.Auto, WaylandRender.XWayland)
    val env      = waylandEnv.copy(properties = jbr + (ToolkitSelection.ToolkitProperty -> "WLToolkit"))
    val resolved = resolve(new Counters, environment = env, configured = xwayland)
    resolved.toolkit.choice shouldBe ToolkitSelection.Choice.PlatformDefault
    resolved.backend.backend shouldBe RenderBackend.Java2D
  }

  it should "keep WLToolkit and choose the raster backend for skia-raster when a raster factory exists" in {
    val counters = new Counters
    val raster   = settings(RenderBackendSetting.Auto, WaylandRender.SkiaRaster)
    val resolved = resolve(counters, environment = waylandEnv, configured = raster)
    resolved.toolkit.choice shouldBe ToolkitSelection.Choice.Wayland
    resolved.backend.backend shouldBe RenderBackend.SkiaRaster
    counters.built.get shouldBe 1
  }

  it should "fall back with 'skia raster surface unavailable' when no raster factory is registered" in {
    val raster   = settings(RenderBackendSetting.Auto, WaylandRender.SkiaRaster)
    val counters = new Counters
    val resolved = resolve(counters, environment = waylandEnv, configured = raster, hasRasterSurface = false)
    resolved.toolkit.choice shouldBe ToolkitSelection.Choice.Wayland
    resolved.backend.reason shouldBe "skia raster surface unavailable"
    counters.probes.get shouldBe 0
  }

  it should "ignore the wayland render on a session that is not on WLToolkit" in {
    val stockJdk = waylandEnv.copy(properties = Map.empty, waylandToolkitAvailable = false)
    val xwayland = settings(RenderBackendSetting.Auto, WaylandRender.SkiaRaster)
    val resolved = resolve(new Counters, environment = stockJdk, configured = xwayland)
    resolved.backend.backend shouldBe RenderBackend.Skia
    resolved.toolkit.choice shouldBe ToolkitSelection.Choice.PlatformDefault
  }
