package com.serenity.app

import cats.effect.IO
import com.serenity.app.RenderBackendSelection.{Decision, Inputs, Probe}
import com.serenity.config.{RenderBackendPeek, RenderBackendSetting, RenderStartupSettings, WaylandRender}
import com.serenity.ui.renderer.FrameSurfaceFactory
import com.serenity.ui.skia.SkiaProbe

/** Settles the AWT toolkit and the renderer together, before any java.awt class loads and before any window exists.
  *
  * They are one decision because each depends on the other: Skia on a Wayland session needs the X11 toolkit, and the
  * X11 toolkit is only worth forcing if Skia then actually runs. A terminal launch never reads the config, probes the
  * native library or builds a Skia factory, which keeps Skiko and Kotlin classes unloaded.
  */
object GraphicsStartup:

  final case class Launch(
      windowed: Boolean,
      safeMode: Boolean,
      renderer: Option[RenderBackendSetting] = None,
      waylandRender: Option[WaylandRender] = None
  )

  final case class Resolved(
      toolkit: ToolkitSelection.Decision,
      backend: Decision,
      surfaces: FrameSurfaceFactory
  )

  def launchOf(options: LaunchOptions): Launch =
    Launch(
      windowed = !LaunchOptions.resolveTuiMode(options),
      safeMode = options.safeMode,
      renderer = options.renderer,
      waylandRender = options.waylandRender
    )

  /** The production wiring: the config file and the Skiko probe, both reached only when a windowed launch gets that
    * far, then the toolkit property is set. No Skia surface exists yet, so until one is registered the catalogue offers
    * Java2D alone.
    */
  def current(launch: Launch): IO[Resolved] =
    ToolkitSelection.currentEnvironment
      .flatMap(environment =>
        resolve(
          launch,
          environment,
          SurfaceCatalogue.Java2DOnly,
          RenderBackendPeek.read(),
          IO.defer(SkiaProbe.run)
        )
      )
      .flatTap(resolved => ToolkitSelection(resolved.toolkit))

  def resolve(
    launch: Launch,
    environment: ToolkitSelection.Environment,
    catalogue: SurfaceCatalogue,
    configured: IO[RenderStartupSettings],
    probe: IO[Probe]
  ): IO[Resolved] =
    val base = ToolkitSelection.choose(environment)
    val settled: IO[(ToolkitSelection.Decision, Decision)] =
      if !launch.windowed then IO.pure((base, javaOnly("terminal or headless launch")))
      else if launch.safeMode then IO.pure((base, javaOnly("safe mode")))
      else configured.flatMap(settings => settleWindowed(launch, environment, catalogue, base, settings, probe))
    settled.map((toolkit, decision) => Resolved(toolkit, decision, catalogue.factoryFor(decision.backend)))

  private def settleWindowed(
    launch: Launch,
    environment: ToolkitSelection.Environment,
    catalogue: SurfaceCatalogue,
    base: ToolkitSelection.Decision,
    settings: RenderStartupSettings,
    probe: IO[Probe]
  ): IO[(ToolkitSelection.Decision, Decision)] =
    val wayland    = RenderBackendSelection.requestedWayland(settings.wayland, environment.env, launch.waylandRender)
    val x11Display = environment.env.get("DISPLAY").exists(_.trim.nonEmpty)
    def inputsFor(toolkit: ToolkitSelection.Decision): Inputs =
      Inputs(
        osName = environment.osName,
        toolkit = effectiveToolkit(environment, toolkit),
        x11Display = x11Display,
        probe = Probe.NotRun,
        setting = RenderBackendSelection.requested(settings.backend, environment.env, launch.renderer),
        skiaSurfaceAvailable = catalogue.skia.isDefined,
        waylandRender = wayland,
        rasterSurfaceAvailable = catalogue.skiaRaster.isDefined
      )
    // Forced only if it would really leave WLToolkit: an explicit SERENITY_TOOLKIT or -Dawt.toolkit.name wins.
    val forcedX11 = Option
      .when(
        wayland == WaylandRender.XWayland && x11Display &&
          effectiveToolkit(environment, base) == ToolkitSelection.Choice.Wayland
      )(ToolkitSelection.choose(environment.copy(xwaylandForSkia = true)))
      .filter(forced => effectiveToolkit(environment, forced) != ToolkitSelection.Choice.Wayland)
    val candidate = forcedX11.getOrElse(base)
    val inputs    = inputsFor(candidate)
    val probed =
      if RenderBackendSelection.probeNeeded(inputs) then probe.map(result => inputs.copy(probe = result))
      else IO.pure(inputs)
    probed.map { ready =>
      val decision = RenderBackendSelection.choose(ready)
      // The X11 toolkit is only worth having if Skia runs on it.
      if decision.backend == RenderBackend.Skia then (candidate, decision) else (base, decision)
    }

  /** A toolkit named on the command line is in force whatever [[ToolkitSelection]] decided. */
  private def effectiveToolkit(
    environment: ToolkitSelection.Environment,
    toolkit: ToolkitSelection.Decision
  ): ToolkitSelection.Choice =
    environment.properties.get(ToolkitSelection.ToolkitProperty).map(_.trim) match
      case Some("WLToolkit") => ToolkitSelection.Choice.Wayland
      case Some("XToolkit")  => ToolkitSelection.Choice.X11
      case _                 => toolkit.choice

  private def javaOnly(reason: String): Decision = Decision(RenderBackend.Java2D, reason)
