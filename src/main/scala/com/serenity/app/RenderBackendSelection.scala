package com.serenity.app

import java.util.Locale

import com.serenity.config.RenderBackendSetting

enum RenderBackend(val configKey: String):
  case Skia   extends RenderBackend("skia")
  case Java2D extends RenderBackend("java2d")

/** Which renderer paints the window, decided once at startup from facts that are all known before a window exists. */
object RenderBackendSelection:

  val OverrideVariable: String = "SERENITY_RENDER_BACKEND"

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
      skiaSurfaceAvailable: Boolean
  )

  final case class Decision(backend: RenderBackend, reason: String):
    def logLine: String = s"backend=${backend.configKey} reason=$reason"

  /** The environment variable wins over the config file; a value it does not recognise is ignored. */
  def requested(configured: RenderBackendSetting, env: Map[String, String]): RenderBackendSetting =
    env
      .get(OverrideVariable)
      .map(_.trim.toLowerCase(Locale.ROOT))
      .flatMap(RenderBackendSetting.fromConfigKey)
      .getOrElse(configured)

  def choose(inputs: Inputs): Decision =
    val linux    = inputs.osName.toLowerCase(Locale.ROOT).contains("linux")
    val explicit = inputs.setting == RenderBackendSetting.Skia
    if inputs.setting == RenderBackendSetting.Java2D then java2d("ui.render.backend=java2d")
    else if !inputs.skiaSurfaceAvailable then java2d("skia surface unavailable")
    else if linux && inputs.toolkit == ToolkitSelection.Choice.Wayland then
      java2d(if explicit then "skia needs X11; running on Wayland" else "WLToolkit cannot host Skia")
    else if linux && !inputs.x11Display then java2d("no X11 display for Skia")
    else
      inputs.probe match
        case Probe.Failed(cause) =>
          java2d(if explicit then s"ui.render.backend=skia could not load: $cause" else s"native probe failed: $cause")
        case Probe.NotRun => java2d("native probe not run")
        case Probe.Loaded =>
          Decision(RenderBackend.Skia, if explicit then "ui.render.backend=skia" else "native probe passed")

  /** Whether loading the native library can change the outcome: only when a passing probe would choose Skia. */
  def probeNeeded(inputs: Inputs): Boolean =
    inputs.probe == Probe.NotRun && choose(inputs.copy(probe = Probe.Loaded)).backend == RenderBackend.Skia

  private def java2d(reason: String): Decision = Decision(RenderBackend.Java2D, reason)
