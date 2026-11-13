package com.serenity.ui.benchmark

import com.serenity.ui.skia.FallbackAgreement
import com.serenity.ui.terminal.LayerDrawStats

/** The machine and window a run was made on, and the render API Skia actually used: `Left` with the cause when the
  * layer could not be made at all. `toolkit` is the AWT toolkit class that drew and `startup` the launch's toolkit and
  * renderer choice; `skiaLayers` is how many layers the Skia window ended with (more than one makes every figure
  * unusable) and `skiaDraws` what the layer's draws cost. `refresh` is the display's rate, the unit of the frame
  * counts. `machine` is the CPU, governor and power source the figures were taken under.
  */
final case class BenchmarkEnvironment(
    os: String,
    java: String,
    widthPx: Int,
    heightPx: Int,
    deviceScale: Double,
    skiaRenderApi: Either[String, String],
    toolkit: String = "unknown",
    startup: String = "",
    skiaLayers: Option[Int] = None,
    skiaDraws: Option[LayerDrawStats] = None,
    refresh: RefreshRate = RefreshRate.Assumed60,
    rasterNote: Option[String] = Some("no raster arm was asked for"),
    machine: MachineState = MachineState.Unknown,
    coldPhases: Option[ColdFramePhases] = None,
    skiaColdSplit: Option[ColdFrameSplit] = None,
    fallbackAgreement: Option[FallbackAgreement] = None,
    drawnWindows: List[(String, WindowPx)] = Nil,
    drawnCanvases: List[(String, WindowPx)] = Nil,
    notTimed: Map[Scenario, String] = Map.empty,
    scenesLikeTheApp: Boolean = false,
    skiaShapeTraffic: Map[Scenario, ShapingTraffic] = Map.empty
):
  /** Skiko falls back to its software redrawers where a GPU API cannot start; those are not a GPU result. */
  def gpuUsed: Boolean = skiaRenderApi.exists(api => !api.startsWith("SOFTWARE") && api != "UNKNOWN")

/** One scenario on each Skia backend: the layer, which decides the verdict, and the CPU raster. */
final case class ScenarioResult(scenario: Scenario, skia: Option[FrameStats], raster: Option[FrameStats] = None)

enum Verdict:
  case Measured
  case NotMeasured(reason: String)

/** Whether a run's figures can be used: the layer was made on a GPU, drew in one layer, and every judged scenario was
  * timed without dropped frames. Scenarios added later (`Scenario.judged` is false) are reported but neither judged nor
  * able to withhold the verdict.
  */
object BenchmarkVerdict:

  def of(results: Vector[ScenarioResult], environment: BenchmarkEnvironment): Verdict =
    environment.skiaRenderApi match
      case Left(cause) => Verdict.NotMeasured(s"Skia layer unavailable: $cause")
      case Right(api) if !environment.gpuUsed =>
        Verdict.NotMeasured(s"GPU NOT USED (software fallback: $api)")
      case Right(_) if environment.skiaLayers.exists(_ > 1) =>
        Verdict.NotMeasured(
          s"the Skia window ended with ${environment.skiaLayers.getOrElse(0)} layers, and each frame waited for the others to draw"
        )
      case Right(_) => judge(results)

  private def judge(results: Vector[ScenarioResult]): Verdict =
    val ruled      = results.filter(_.scenario.judged)
    val unmeasured = ruled.filter(_.skia.isEmpty).map(_.scenario.label)
    val missing =
      Scenario.values.toList.filter(_.judged).filterNot(s => results.exists(_.scenario == s)).map(_.label)
    val dropped = ruled.filter(_.skia.exists(_.dropped > 0)).map(_.scenario.label)
    if missing.nonEmpty || unmeasured.nonEmpty then
      Verdict.NotMeasured(s"no figures for ${(missing ++ unmeasured).mkString(", ")}")
    else if dropped.nonEmpty then Verdict.NotMeasured(s"frames were dropped in ${dropped.mkString(", ")}; run it again")
    else Verdict.Measured
