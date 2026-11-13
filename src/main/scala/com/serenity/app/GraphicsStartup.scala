package com.serenity.app

import cats.effect.IO
import com.serenity.app.RenderBackendSelection.{Decision, Inputs, Probe}
import com.serenity.config.{RenderBackendPeek, RenderBackendSetting, RenderStartupSettings, SkiaFrames, WaylandRender}
import com.serenity.ui.renderer.FrameSurfaceFactory
import com.serenity.ui.skia.{LayerFrames, SkiaProbe}

/** Settles the AWT toolkit and the renderer together, before any java.awt class loads and before any window exists.
  *
  * They are one decision because each depends on the other: Skia on a Wayland session needs the X11 toolkit, and the
  * X11 toolkit is only worth forcing if Skia then actually runs. A terminal launch never reads the config, probes the
  * native library or builds a Skia factory, which keeps Skiko and Kotlin classes unloaded. A window launch that cannot
  * load Skia shows the terminal instead, unless it insisted on a window or there is no terminal.
  */
object GraphicsStartup:

  /** `forcedWindow` is a launch that asked for the window by name (`--gui`, `--smoke-test`, `--startup-probe`), so a
    * Skia that cannot load is a failure rather than a reason to show the terminal; `terminalAvailable` is whether there
    * is a terminal to show it on.
    */
  final case class Launch(
      windowed: Boolean,
      safeMode: Boolean,
      renderer: Option[RenderBackendSetting] = None,
      waylandRender: Option[WaylandRender] = None,
      forcedWindow: Boolean = false,
      terminalAvailable: Boolean = true
  )

  /** What [[resolve]] fails with for a window launch that Skia cannot serve and that has nowhere else to go; the
    * message is what the person is told.
    */
  final class SkiaUnavailable(decision: Decision)
      extends RuntimeException(
        s"Serenity could not open its window because Skia could not load: ${decision.skiaFailure.getOrElse(decision.reason)}"
      )

  /** What the launch puts on screen. */
  enum Display:
    case Window(surfaces: FrameSurfaceFactory)
    case Terminal

    /** Every display is routed to one of the two, so a new one cannot be left out. */
    def route[A](window: FrameSurfaceFactory => A, terminal: => A): A =
      this match
        case Window(surfaces) => window(surfaces)
        case Terminal         => terminal

  final case class Resolved(
      toolkit: ToolkitSelection.Decision,
      backend: Decision,
      display: Display,
      skiaVsync: Boolean = false,
      skiaFrames: SkiaFrames = SkiaFrames.Image
  ):

    def windowed: Boolean = display.route(_ => true, false)

  def launchOf(options: LaunchOptions, stdoutIsTty: Boolean = System.console() != null): Launch =
    Launch(
      windowed = !LaunchOptions.resolveTuiMode(options, stdoutIsTty = stdoutIsTty),
      safeMode = options.safeMode,
      renderer = options.renderer,
      waylandRender = options.waylandRender,
      forcedWindow = options.gui || options.smokeTest || options.startupProbe || options.rendererTrial.isDefined,
      terminalAvailable = stdoutIsTty
    )

  /** The production wiring: the config file and the Skiko probe, both reached only when a windowed launch gets that
    * far, then the toolkit property is set. The catalogue offers the GPU layer, which selection picks wherever the
    * probe passes, and the raster surface, which it picks only on Wayland when `ui.render.wayland` asks for
    * `skia-raster`.
    */
  def current(launch: Launch): IO[Resolved] =
    ToolkitSelection.currentEnvironment
      .flatMap(environment =>
        resolve(
          launch,
          environment,
          SurfaceCatalogue.Available,
          RenderBackendPeek.read(),
          IO.defer(SkiaProbe.run)
        )
      )
      .flatTap(resolved => ToolkitSelection(resolved.toolkit))
      .flatTap(resolved => applySkiaVsync(resolved))
      .flatTap(resolved => applySkiaFrames(resolved))

  private def applySkiaVsync(resolved: Resolved): IO[Unit] =
    IO.whenA(resolved.backend.backend == RenderBackend.Skia)(
      IO {
        val explicit = Option(System.getProperty(SkiaVsync.Property)).map(SkiaVsync.Property -> _).toMap
        SkiaVsync
          .property(resolved.skiaVsync, sys.env, explicit)
          .foreach((key, value) => System.setProperty(key, value))
      }
    )

  private def applySkiaFrames(resolved: Resolved): IO[Unit] =
    IO.whenA(resolved.backend.backend == RenderBackend.Skia)(
      IO {
        val explicit = Option(System.getProperty(LayerFrames.Property)).map(LayerFrames.Property -> _).toMap
        SkiaFramesProperty
          .property(resolved.skiaFrames, explicit)
          .foreach((key, value) => System.setProperty(key, value))
      }
    )

  /** What a launch that reads no config leaves the render settings at: vsync off, images. */
  private val Unchosen: (Boolean, SkiaFrames) = (false, SkiaFrames.Image)

  def resolve(
    launch: Launch,
    environment: ToolkitSelection.Environment,
    catalogue: SurfaceCatalogue,
    configured: IO[RenderStartupSettings],
    probe: IO[Probe]
  ): IO[Resolved] =
    val base = ToolkitSelection.choose(environment)
    val settled: IO[(ToolkitSelection.Decision, Decision, (Boolean, SkiaFrames))] =
      if !launch.windowed then IO.pure((base, terminal("terminal or headless launch"), Unchosen))
      else
        // Safe mode never reads the user's config, so it paints with the defaults.
        val settings = if launch.safeMode then IO.pure(RenderBackendPeek.Defaults) else configured
        settings.flatMap(chosen =>
          settleWindowed(launch, environment, base, chosen, probe)
            .map((toolkit, decision) => (toolkit, decision, (chosen.skiaVsync, chosen.skiaFrames)))
        )
    settled.flatMap {
      case (toolkit, decision, (vsync, frames)) =>
        displayFor(launch, decision, catalogue)
          .fold[IO[Resolved]](IO.raiseError(SkiaUnavailable(decision)))(display =>
            IO.pure(Resolved(toolkit, decision, display, vsync, frames))
          )
    }

  /** `None` for a window launch that cannot have its window and has no terminal to fall back to, or insisted on a
    * window.
    */
  private def displayFor(launch: Launch, decision: Decision, catalogue: SurfaceCatalogue): Option[Display] =
    catalogue.factoryFor(decision.backend) match
      case Some(surfaces) => Some(Display.Window(surfaces))
      case None if !launch.windowed || (launch.terminalAvailable && !launch.forcedWindow) => Some(Display.Terminal)
      case None                                                                           => None

  private def settleWindowed(
    launch: Launch,
    environment: ToolkitSelection.Environment,
    base: ToolkitSelection.Decision,
    settings: RenderStartupSettings,
    probe: IO[Probe]
  ): IO[(ToolkitSelection.Decision, Decision)] =
    val wayland    = RenderBackendSelection.requestedWayland(settings.wayland, environment.env, launch.waylandRender)
    val x11Display = environment.env.get("DISPLAY").exists(_.trim.nonEmpty)
    def inputsFor(toolkit: ToolkitSelection.Decision, probed: Probe): Inputs =
      Inputs(
        osName = environment.osName,
        toolkit = effectiveToolkit(environment, toolkit),
        x11Display = x11Display,
        probe = probed,
        setting = RenderBackendSelection.requested(settings.backend, environment.env, launch.renderer),
        waylandRender = wayland
      )
    // Forced only if it would really leave WLToolkit: an explicit SERENITY_TOOLKIT or -Dawt.toolkit.name wins.
    val forcedX11 = Option
      .when(
        wayland == WaylandRender.XWayland && x11Display &&
          effectiveToolkit(environment, base) == ToolkitSelection.Choice.Wayland
      )(ToolkitSelection.choose(environment.copy(xwaylandForSkia = true)))
      .filter(forced => effectiveToolkit(environment, forced) != ToolkitSelection.Choice.Wayland)
    val candidate = forcedX11.getOrElse(base)
    probe.map { probed =>
      val decision = RenderBackendSelection.choose(inputsFor(candidate, probed))
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

  private def terminal(reason: String): Decision = Decision(RenderBackend.Terminal, reason)
