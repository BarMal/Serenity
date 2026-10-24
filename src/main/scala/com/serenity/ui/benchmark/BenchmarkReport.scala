package com.serenity.ui.benchmark

import com.serenity.ui.terminal.LayerDrawStats
import io.circe.Json
import io.circe.syntax.*

/** A run's figures as the text the owner reads and the JSON that can be kept. */
final case class BenchmarkReport(environment: BenchmarkEnvironment, results: Vector[ScenarioResult], verdict: Verdict):

  def markdown: String =
    val header =
      "| Scenario | Java2D p50 | p95 | max | Skia p50 | p95 | max | Skia/Java2D p50 | p95 |\n" +
        "|---|---:|---:|---:|---:|---:|---:|---:|---:|"
    val rows = results.map(row)
    (List("# Render benchmark", "", environmentLine) ++ launchLines ++ List("", header) ++ rows ++ List(
      "",
      verdictText
    ))
      .mkString("\n") + "\n"

  def json: String =
    Json
      .obj(
        "environment" -> environmentJson,
        "scenarios"   -> results.map(scenarioJson).asJson,
        "verdict"     -> verdictJson
      )
      .spaces2

  private def environmentLine: String =
    val api = environment.skiaRenderApi.fold(cause => s"Skia layer unavailable ($cause)", identity)
    s"Window ${environment.widthPx}x${environment.heightPx}, scale ${environment.deviceScale}, Skia render API $api, " +
      s"toolkit ${environment.toolkit}, ${environment.os}, ${environment.java}."

  private def launchLines: List[String] =
    val startup = Option.when(environment.startup.nonEmpty)(s"Launch: ${environment.startup}.")
    val layers =
      environment.skiaLayers.map(count => s"Skia window ended with $count ${if count == 1 then "layer" else "layers"}.")
    val draws = environment.skiaDraws.map { stats =>
      f"Skia layer: ${stats.renders} draws for ${stats.publishes} published frames; publish to draw mean ${stats.publishToRenderMeanMs}%.2f ms (max ${stats.publishToRenderMaxMs}%.2f); " +
        f"per draw: replay ${stats.replayMeanMs}%.2f ms, carets ${stats.caretsMeanMs}%.2f ms, GPU sync ${stats.syncMeanMs}%.2f ms."
    }
    val cache = environment.skiaDraws.map { stats =>
      s"Skia layer: replay cache: ${stats.replayCacheNote}; ${stats.replays} replays in ${stats.renders} draws; " +
        s"GPU sync: ${stats.gpuSyncNote}."
    }
    List(startup, layers, draws, cache).flatten.map(line => s"\n$line")

  private def row(result: ScenarioResult): String =
    val java2d = result.java2d
    val skia = result.skia.fold("| - | - | - | - | - |")(s =>
      s"| ${f(s.p50Ms)} | ${f(s.p95Ms)} | ${f(s.maxMs)} | ${ratio(s.p50Ms, java2d.p50Ms)} | ${ratio(s.p95Ms, java2d.p95Ms)} |"
    )
    s"| ${result.scenario.label} | ${f(java2d.p50Ms)} | ${f(java2d.p95Ms)} | ${f(java2d.maxMs)} $skia"

  private def verdictText: String =
    verdict match
      case Verdict.SkiaWins =>
        "**Skia wins** on this machine under the owner's rule: auto may pick Skia."
      case Verdict.Java2DStays(reasons) =>
        ("**auto stays on Java2D.**" :: reasons.map(reason => s"- $reason")).mkString("\n")
      case Verdict.NotMeasured(reason) => s"**No verdict:** $reason."

  private def scenarioJson(result: ScenarioResult): Json =
    val base = List("scenario" -> result.scenario.label.asJson, "java2d" -> statsJson(result.java2d))
    Json.obj(base ++ result.skia.map(stats => "skia" -> statsJson(stats)).toList*)

  private def statsJson(stats: FrameStats): Json =
    Json.obj(
      "count"   -> stats.count.asJson,
      "p50Ms"   -> stats.p50Ms.asJson,
      "p95Ms"   -> stats.p95Ms.asJson,
      "maxMs"   -> stats.maxMs.asJson,
      "dropped" -> stats.dropped.asJson
    )

  private def environmentJson: Json =
    val api =
      environment.skiaRenderApi.fold(cause => "skiaUnavailable" -> cause.asJson, api => "skiaRenderApi" -> api.asJson)
    Json.obj(
      List(
        "os"          -> environment.os.asJson,
        "java"        -> environment.java.asJson,
        "width"       -> environment.widthPx.asJson,
        "height"      -> environment.heightPx.asJson,
        "deviceScale" -> environment.deviceScale.asJson,
        "toolkit"     -> environment.toolkit.asJson,
        "startup"     -> environment.startup.asJson,
        api
      ) ++ environment.skiaLayers.map(count => "skiaLayers" -> count.asJson).toList ++
        environment.skiaDraws.map(stats => "skiaDraws" -> drawsJson(stats)).toList*
    )

  private def drawsJson(stats: LayerDrawStats): Json =
    Json.obj(
      "renders"               -> stats.renders.asJson,
      "publishes"             -> stats.publishes.asJson,
      "publishToRenderMeanMs" -> stats.publishToRenderMeanMs.asJson,
      "publishToRenderMaxMs"  -> stats.publishToRenderMaxMs.asJson,
      "replayMeanMs"          -> stats.replayMeanMs.asJson,
      "caretsMeanMs"          -> stats.caretsMeanMs.asJson,
      "syncMeanMs"            -> stats.syncMeanMs.asJson,
      "replays"               -> stats.replays.asJson,
      "cachedDraws"           -> stats.cachedDraws.asJson,
      "syncedDraws"           -> stats.syncedDraws.asJson,
      "gpuContext"            -> stats.gpuContext.asJson,
      "replayCacheEngaged"    -> stats.replayCacheEngaged.asJson
    )

  private def verdictJson: Json =
    verdict match
      case Verdict.SkiaWins             => Json.obj("kind" -> "skia-wins".asJson)
      case Verdict.Java2DStays(reasons) => Json.obj("kind" -> "java2d-stays".asJson, "reasons" -> reasons.asJson)
      case Verdict.NotMeasured(reason)  => Json.obj("kind" -> "not-measured".asJson, "reason" -> reason.asJson)

  private def f(millis: Double): String = f"$millis%.2f"

  private def ratio(skia: Double, java2d: Double): String = if java2d > 0 then f"${skia / java2d}%.2fx" else "-"
