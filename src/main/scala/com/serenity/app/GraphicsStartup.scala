package com.serenity.app

import cats.effect.IO
import com.serenity.app.RenderBackendSelection.{Decision, Inputs, Probe}
import com.serenity.config.{RenderBackendPeek, RenderBackendSetting}
import com.serenity.ui.renderer.FrameSurfaceFactory
import com.serenity.ui.skia.SkiaProbe

/** Settles the renderer after the toolkit is fixed and before any window exists. A terminal launch never reads the
  * config, probes the native library or builds a Skia factory, which keeps Skiko and Kotlin classes unloaded.
  */
object GraphicsStartup:

  final case class Launch(windowed: Boolean, safeMode: Boolean)

  final case class Resolved(
      toolkit: ToolkitSelection.Decision,
      backend: Decision,
      surfaces: FrameSurfaceFactory
  )

  def launchOf(options: LaunchOptions): Launch =
    Launch(windowed = !LaunchOptions.resolveTuiMode(options), safeMode = options.safeMode)

  /** The production wiring: the config file and the Skiko probe, both reached only when a windowed launch gets that
    * far. No Skia surface exists yet, so until one is registered the catalogue offers Java2D alone.
    */
  def current(launch: Launch, toolkit: ToolkitSelection.Decision): IO[Resolved] =
    ToolkitSelection.currentEnvironment.flatMap(environment =>
      resolve(
        launch,
        environment,
        toolkit,
        SurfaceCatalogue.Java2DOnly,
        RenderBackendPeek.read(),
        IO.defer(SkiaProbe.run)
      )
    )

  def resolve(
    launch: Launch,
    environment: ToolkitSelection.Environment,
    toolkit: ToolkitSelection.Decision,
    catalogue: SurfaceCatalogue,
    configured: IO[RenderBackendSetting],
    probe: IO[Probe]
  ): IO[Resolved] =
    val decided: IO[Decision] =
      if !launch.windowed then IO.pure(javaOnly("terminal or headless launch"))
      else if launch.safeMode then IO.pure(javaOnly("safe mode"))
      else
        configured.flatMap { setting =>
          val inputs = Inputs(
            osName = environment.osName,
            toolkit = effectiveToolkit(environment, toolkit),
            x11Display = environment.env.get("DISPLAY").exists(_.trim.nonEmpty),
            probe = Probe.NotRun,
            setting = RenderBackendSelection.requested(setting, environment.env),
            skiaSurfaceAvailable = catalogue.skia.isDefined
          )
          val probed =
            if RenderBackendSelection.probeNeeded(inputs) then probe.map(result => inputs.copy(probe = result))
            else IO.pure(inputs)
          probed.map(RenderBackendSelection.choose)
        }
    decided.map(decision => Resolved(toolkit, decision, catalogue.factoryFor(decision.backend)))

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
