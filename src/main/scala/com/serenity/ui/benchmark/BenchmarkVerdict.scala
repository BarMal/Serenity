package com.serenity.ui.benchmark

import com.serenity.ui.terminal.LayerDrawStats

/** The machine and window a run was made on, and the render API Skia actually used: `Left` with the cause when the
  * layer could not be made at all. `toolkit` is the AWT toolkit class that drew and `startup` the launch's toolkit and
  * renderer choice; `skiaLayers` is how many layers the Skia window ended with (more than one makes every figure
  * unusable) and `skiaDraws` what the layer's draws cost.
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
    skiaDraws: Option[LayerDrawStats] = None
):
  /** Skiko falls back to its software redrawers where a GPU API cannot start; those are not a GPU result. */
  def gpuUsed: Boolean = skiaRenderApi.exists(api => !api.startsWith("SOFTWARE") && api != "UNKNOWN")

final case class ScenarioResult(scenario: Scenario, java2d: FrameStats, skia: Option[FrameStats])

enum Verdict:
  case SkiaWins
  case Java2DStays(reasons: List[String])
  case NotMeasured(reason: String)

/** The owner's rule for whether `auto` may pick Skia: on a GPU, Skia's p50 and p95 are no worse than Java2D's in every
  * scenario but the cold full frame, and its p95 is better at scrolling and page down.
  */
object BenchmarkVerdict:

  private val Tie = 1e-9

  private val strictlyBetterAt = Set(Scenario.ScrollOneLine, Scenario.ScrollFiveLines, Scenario.PageDown)

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
    val unmeasured = results.filter(_.skia.isEmpty).map(_.scenario.label)
    val missing    = Scenario.values.toList.filterNot(s => results.exists(_.scenario == s)).map(_.label)
    val dropped    = results.filter(r => r.java2d.dropped > 0 || r.skia.exists(_.dropped > 0)).map(_.scenario.label)
    if missing.nonEmpty || unmeasured.nonEmpty then
      Verdict.NotMeasured(s"no figures for ${(missing ++ unmeasured).mkString(", ")}")
    else if dropped.nonEmpty then Verdict.NotMeasured(s"frames were dropped in ${dropped.mkString(", ")}; run it again")
    else
      val reasons = results.filter(_.scenario != Scenario.ColdFullFrame).flatMap(result => complaints(result))
      if reasons.isEmpty then Verdict.SkiaWins else Verdict.Java2DStays(reasons.toList)

  private def complaints(result: ScenarioResult): List[String] =
    result.skia.toList.flatMap { skia =>
      val java2d = result.java2d
      val label  = result.scenario.label
      List(
        Option.when(skia.p50Ms > java2d.p50Ms)(
          s"$label: Skia p50 ${f(skia.p50Ms)} ms is worse than Java2D's ${f(java2d.p50Ms)} ms"
        ),
        Option.when(skia.p95Ms > java2d.p95Ms)(
          s"$label: Skia p95 ${f(skia.p95Ms)} ms is worse than Java2D's ${f(java2d.p95Ms)} ms"
        ),
        Option.when(math.abs(skia.p95Ms - java2d.p95Ms) < Tie && strictlyBetterAt.contains(result.scenario))(
          s"$label: Skia p95 ${f(skia.p95Ms)} ms is not better than Java2D's ${f(java2d.p95Ms)} ms"
        )
      ).flatten
    }

  private def f(millis: Double): String = f"$millis%.2f"
