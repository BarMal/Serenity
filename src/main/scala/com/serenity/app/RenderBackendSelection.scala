package com.serenity.app

import java.util.Locale

import com.serenity.config.{RenderBackendSetting, WaylandRender}

enum RenderBackend(val configKey: String):
  case Skia       extends RenderBackend("skia")
  case SkiaRaster extends RenderBackend("skia-raster")
  case Java2D     extends RenderBackend("java2d")

/** Which renderer paints the window, decided once at startup from facts that are all known before a window exists. */
object RenderBackendSelection:

  /** `auto` stays on Java2D until the built-in benchmark (`--render-benchmark`) shows Skia on a GPU ahead of it. */
  private val AutoReason = "auto uses Java2D until Skia is measured faster"

  val OverrideVariable: String        = "SERENITY_RENDER_BACKEND"
  val WaylandOverrideVariable: String = "SERENITY_WAYLAND_RENDER"

  enum Probe:
    case NotRun
    case Loaded
    case Failed(cause: String)

  final case class Inputs(
      osName: String,
      toolkit: ToolkitSelection.Choice,
      x11Display: Boolean,
      probe: Probe,
      setting: RenderBackendSetting,
      skiaSurfaceAvailable: Boolean,
      waylandRender: WaylandRender = WaylandRender.Java2D,
      rasterSurfaceAvailable: Boolean = false
  )

  final case class Decision(backend: RenderBackend, reason: String):
    def logLine: String = s"backend=${backend.configKey} reason=$reason"

  /** The flag beats the environment variable, which beats the config file; a value it does not recognise is ignored.
    */
  def requested(
    configured: RenderBackendSetting,
    env: Map[String, String],
    flag: Option[RenderBackendSetting]
  ): RenderBackendSetting =
    flag.orElse(fromEnvironment(env, OverrideVariable)(RenderBackendSetting.fromConfigKey)).getOrElse(configured)

  def requestedWayland(
    configured: WaylandRender,
    env: Map[String, String],
    flag: Option[WaylandRender]
  ): WaylandRender =
    flag.orElse(fromEnvironment(env, WaylandOverrideVariable)(WaylandRender.fromConfigKey)).getOrElse(configured)

  private def fromEnvironment[A](env: Map[String, String], name: String)(parse: String => Option[A]): Option[A] =
    env.get(name).map(_.trim.toLowerCase(Locale.ROOT)).flatMap(parse)

  def choose(inputs: Inputs): Decision =
    val linux = inputs.osName.toLowerCase(Locale.ROOT).contains("linux")
    if inputs.setting == RenderBackendSetting.Java2D then java2d("ui.render.backend=java2d")
    else if linux && inputs.toolkit == ToolkitSelection.Choice.Wayland then onWayland(inputs)
    else if inputs.setting == RenderBackendSetting.Auto then java2d(AutoReason)
    else if !inputs.skiaSurfaceAvailable then java2d("skia surface unavailable")
    else if linux && !inputs.x11Display then java2d("no X11 display for Skia")
    else afterProbe(inputs, RenderBackend.Skia, "native probe passed")

  /** WLToolkit cannot host Skia's GLX layer, so what happens next is the Wayland render mode's call. */
  private def onWayland(inputs: Inputs): Decision =
    inputs.waylandRender match
      case WaylandRender.Java2D =>
        java2d(
          if inputs.setting == RenderBackendSetting.Skia then "skia needs X11; running on Wayland"
          else "WLToolkit cannot host Skia"
        )
      case WaylandRender.XWayland =>
        java2d("ui.render.wayland=xwayland needs XToolkit, but WLToolkit is in force")
      case WaylandRender.SkiaRaster if !inputs.rasterSurfaceAvailable =>
        java2d("skia raster surface unavailable")
      case WaylandRender.SkiaRaster =>
        afterProbe(inputs, RenderBackend.SkiaRaster, "ui.render.wayland=skia-raster")

  private def afterProbe(inputs: Inputs, backend: RenderBackend, loadedReason: String): Decision =
    val explicit = inputs.setting == RenderBackendSetting.Skia
    inputs.probe match
      case Probe.Failed(cause) =>
        java2d(if explicit then s"ui.render.backend=skia could not load: $cause" else s"native probe failed: $cause")
      case Probe.NotRun => java2d("native probe not run")
      case Probe.Loaded =>
        val reason = if explicit && backend == RenderBackend.Skia then "ui.render.backend=skia" else loadedReason
        Decision(backend, reason)

  /** Whether loading the native library can change the outcome: only when a passing probe would choose Skia. */
  def probeNeeded(inputs: Inputs): Boolean =
    inputs.probe == Probe.NotRun &&
      choose(inputs.copy(probe = Probe.Loaded)).backend != RenderBackend.Java2D

  private def java2d(reason: String): Decision = Decision(RenderBackend.Java2D, reason)
