package com.serenity.app

import java.util.concurrent.atomic.AtomicInteger

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.app.RenderBackendSelection.Probe
import com.serenity.config.{RenderBackendSetting, RenderStartupSettings, WaylandRender}
import com.serenity.ui.renderer.FrameSurfaceFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The production catalogue, driven through [[GraphicsStartup.resolve]] with the probe and the config stubbed, so the
  * wiring is checked without a window or a native library.
  */
class SkiaWiringSpec extends AnyFlatSpec with Matchers:

  private val x11Environment = ToolkitSelection.Environment("Linux", Map.empty, Map("DISPLAY" -> ":0"), false)

  private val waylandEnvironment = ToolkitSelection.Environment(
    "Linux",
    Map("java.vendor" -> "JetBrains s.r.o.", "java.vm.vendor" -> "JetBrains s.r.o."),
    Map("DISPLAY"     -> ":0", "WAYLAND_DISPLAY"              -> "wayland-1"),
    waylandToolkitAvailable = true
  )

  final private class Built:
    val gpu: AtomicInteger    = new AtomicInteger(0)
    val raster: AtomicInteger = new AtomicInteger(0)

  private def counted(built: Built): SurfaceCatalogue =
    def counting(counter: AtomicInteger)(build: () => FrameSurfaceFactory): () => FrameSurfaceFactory = () =>
      counter.incrementAndGet()
      build()
    GraphicsStartup.catalogue.copy(
      skia = GraphicsStartup.catalogue.skia.map(counting(built.gpu)),
      skiaRaster = GraphicsStartup.catalogue.skiaRaster.map(counting(built.raster))
    )

  private def resolve(
    built: Built,
    launch: GraphicsStartup.Launch,
    environment: ToolkitSelection.Environment,
    setting: RenderBackendSetting = RenderBackendSetting.Auto
  ): GraphicsStartup.Resolved =
    GraphicsStartup
      .resolve(
        launch,
        environment,
        counted(built),
        IO.pure(RenderStartupSettings(setting, WaylandRender.Java2D)),
        IO.pure(Probe.Loaded)
      )
      .unsafeRunSync()

  private val windowed = GraphicsStartup.Launch(windowed = true, safeMode = false)

  "GraphicsStartup.catalogue" should "register both Skia factories" in {
    GraphicsStartup.catalogue.skia should not be empty
    GraphicsStartup.catalogue.skiaRaster should not be empty
  }

  it should "give an X11 desktop the GPU factory, building only that one" in {
    val built    = new Built
    val resolved = resolve(built, windowed, x11Environment)

    resolved.backend.backend shouldBe RenderBackend.Skia
    resolved.surfaces should not be theSameInstanceAs(FrameSurfaceFactory.Java2D)
    (built.gpu.get, built.raster.get) shouldBe (1, 0)
  }

  it should "give native Wayland the raster factory under --wayland-render=skia-raster, with no hosted canvas" in {
    val built = new Built
    val resolved =
      resolve(built, windowed.copy(waylandRender = Some(WaylandRender.SkiaRaster)), waylandEnvironment)

    resolved.backend.backend shouldBe RenderBackend.SkiaRaster
    resolved.surfaces should not be theSameInstanceAs(FrameSurfaceFactory.Java2D)
    resolved.surfaces.hostedCanvas shouldBe None
    (built.gpu.get, built.raster.get) shouldBe (0, 1)
  }

  it should "build neither Skia factory when the renderer is java2d" in {
    val built    = new Built
    val resolved = resolve(built, windowed, x11Environment, RenderBackendSetting.Java2D)

    resolved.surfaces shouldBe theSameInstanceAs(FrameSurfaceFactory.Java2D)
    (built.gpu.get, built.raster.get) shouldBe (0, 0)
  }

  it should "build neither Skia factory for a terminal launch or safe mode" in
    List(
      GraphicsStartup.Launch(windowed = false, safeMode = false),
      GraphicsStartup.Launch(windowed = true, safeMode = true)
    ).foreach { launch =>
      val built    = new Built
      val resolved = resolve(built, launch, x11Environment)
      resolved.surfaces shouldBe theSameInstanceAs(FrameSurfaceFactory.Java2D)
      (built.gpu.get, built.raster.get) shouldBe (0, 0)
    }

  "The Java2D factory" should "host no canvas, so the window keeps painting its own" in {
    FrameSurfaceFactory.Java2D.hostedCanvas shouldBe None
  }
