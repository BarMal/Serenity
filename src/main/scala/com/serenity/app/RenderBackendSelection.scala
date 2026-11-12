package com.serenity.app

import java.util.Locale

import com.serenity.config.{RenderBackendSetting, WaylandRender}

enum RenderBackend(val configKey: String):
  case Skia       extends RenderBackend("skia")
  case SkiaRaster extends RenderBackend("skia-raster")
  case Terminal   extends RenderBackend("terminal")

/** Which renderer paints the window, decided once at startup from facts that are all known before a window exists. */
object RenderBackendSelection:

  val OverrideVariable: String        = "SERENITY_RENDER_BACKEND"
  val WaylandOverrideVariable: String = "SERENITY_WAYLAND_RENDER"

  enum Probe:
    case Loaded
    case Failed(cause: String, detail: Option[String] = None)

  final case class Inputs(
      osName: String,
      toolkit: ToolkitSelection.Choice,
      x11Display: Boolean,
      probe: Probe,
      setting: RenderBackendSetting,
      waylandRender: WaylandRender = WaylandRender.SkiaRaster
  )

  /** `skiaFailure` is set only when Skia was wanted and the native probe failed: the one fallback a user did not ask
    * for and cannot see. It is deliberately not part of `logLine`.
    */
  final case class Decision(backend: RenderBackend, reason: String, skiaFailure: Option[String] = None):
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
    if inputs.setting == RenderBackendSetting.SkiaRaster then
      afterProbe(inputs, RenderBackend.SkiaRaster, "ui.render.backend=skia-raster")
    else if linux && inputs.toolkit == ToolkitSelection.Choice.Wayland then onWayland(inputs)
    else if linux && !inputs.x11Display then
      afterProbe(inputs, RenderBackend.SkiaRaster, "no X11 display for Skia; using the Skia raster surface")
    else afterProbe(inputs, RenderBackend.Skia, "native probe passed")

  /** WLToolkit cannot host Skia's GLX layer, so what happens next is the Wayland render mode's call. */
  private def onWayland(inputs: Inputs): Decision =
    inputs.waylandRender match
      case WaylandRender.XWayland =>
        afterProbe(
          inputs,
          RenderBackend.SkiaRaster,
          "ui.render.wayland=xwayland needs XToolkit, but WLToolkit is in force; using the Skia raster surface"
        )
      case WaylandRender.SkiaRaster =>
        afterProbe(inputs, RenderBackend.SkiaRaster, "ui.render.wayland=skia-raster")

  private def afterProbe(inputs: Inputs, backend: RenderBackend, loadedReason: String): Decision =
    val explicit = inputs.setting == RenderBackendSetting.Skia || inputs.setting == RenderBackendSetting.SkiaRaster
    inputs.probe match
      case Probe.Failed(cause, detail) =>
        Decision(
          RenderBackend.Terminal,
          if explicit then s"ui.render.backend=${inputs.setting.configKey} could not load: $cause"
          else s"native probe failed: $cause",
          Some(detail.fold(cause)(causes => s"$cause ($causes)"))
        )
      case Probe.Loaded =>
        val reason =
          if inputs.setting == RenderBackendSetting.Skia && backend == RenderBackend.Skia then "ui.render.backend=skia"
          else loadedReason
        Decision(backend, reason)
