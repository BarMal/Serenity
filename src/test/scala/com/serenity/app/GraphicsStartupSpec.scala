package com.serenity.app

import java.util.concurrent.atomic.AtomicInteger

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.app.RenderBackendSelection.Probe
import com.serenity.config.RenderBackendSetting
import com.serenity.ui.renderer.FrameSurfaceFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GraphicsStartupSpec extends AnyFlatSpec with Matchers:

  private val x11Env = ToolkitSelection.Environment("Linux", Map.empty, Map("DISPLAY" -> ":0"), false)

  final private class Counters:
    val probes: AtomicInteger = new AtomicInteger(0)
    val peeks: AtomicInteger  = new AtomicInteger(0)
    val built: AtomicInteger  = new AtomicInteger(0)

  private def resolve(
    counters: Counters,
    launch: GraphicsStartup.Launch = GraphicsStartup.Launch(windowed = true, safeMode = false),
    environment: ToolkitSelection.Environment = x11Env,
    configured: RenderBackendSetting = RenderBackendSetting.Auto,
    probe: Probe = Probe.Loaded,
    hasSkiaSurface: Boolean = true
  ): GraphicsStartup.Resolved =
    val skia: Option[() => FrameSurfaceFactory] =
      Option.when(hasSkiaSurface)(() =>
        counters.built.incrementAndGet()
        FrameSurfaceFactory.Java2D
      )
    GraphicsStartup
      .resolve(
        launch,
        environment,
        ToolkitSelection.Decision(ToolkitSelection.Choice.PlatformDefault, "test"),
        SurfaceCatalogue(skia),
        IO(counters.peeks.incrementAndGet()).as(configured),
        IO(counters.probes.incrementAndGet()).as(probe)
      )
      .unsafeRunSync()

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

  it should "stay on Java2D in safe mode without probing" in {
    val counters = new Counters
    val resolved = resolve(counters, launch = GraphicsStartup.Launch(windowed = true, safeMode = true))
    resolved.backend.backend shouldBe RenderBackend.Java2D
    counters.probes.get shouldBe 0
  }

  it should "not probe when the setting is java2d" in {
    val counters = new Counters
    resolve(counters, configured = RenderBackendSetting.Java2D).backend.backend shouldBe RenderBackend.Java2D
    counters.probes.get shouldBe 0
    counters.built.get shouldBe 0
  }

  it should "not probe when there is no X11 display" in {
    val counters  = new Counters
    val noDisplay = x11Env.copy(env = Map.empty)
    resolve(counters, environment = noDisplay).backend.backend shouldBe RenderBackend.Java2D
    counters.probes.get shouldBe 0
  }

  it should "use Java2D under WLToolkit and name the Wayland reason for an explicit skia setting" in {
    val counters = new Counters
    val wayland  = x11Env.copy(properties = Map(ToolkitSelection.ToolkitProperty -> "WLToolkit"))
    resolve(counters, environment = wayland, configured = RenderBackendSetting.Skia).backend.reason shouldBe
      "skia needs X11; running on Wayland"
    counters.probes.get shouldBe 0
  }

  it should "use Java2D when the toolkit decision chose the Wayland toolkit" in {
    val counters = new Counters
    val resolved = GraphicsStartup
      .resolve(
        GraphicsStartup.Launch(windowed = true, safeMode = false),
        x11Env,
        ToolkitSelection.Decision(ToolkitSelection.Choice.Wayland, "Wayland session"),
        SurfaceCatalogue(Some(() => FrameSurfaceFactory.Java2D)),
        IO.pure(RenderBackendSetting.Auto),
        IO(counters.probes.incrementAndGet()).as(Probe.Loaded)
      )
      .unsafeRunSync()
    resolved.backend.backend shouldBe RenderBackend.Java2D
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
    val resolved = resolve(counters, hasSkiaSurface = false)
    resolved.backend.reason shouldBe "skia surface unavailable"
    counters.probes.get shouldBe 0
  }

  it should "let the environment variable beat the configured setting" in {
    val env = x11Env.copy(env = x11Env.env + (RenderBackendSelection.OverrideVariable -> "java2d"))
    resolve(new Counters, environment = env).backend.backend shouldBe RenderBackend.Java2D
  }

  it should "pass the toolkit decision through unchanged" in {
    resolve(new Counters).toolkit.reason shouldBe "test"
  }
