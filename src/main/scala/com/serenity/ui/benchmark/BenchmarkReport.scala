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
    val tailHeader =
      "| Scenario | Java2D p75 | p90 | p99 | >33 ms | early | Skia p75 | p90 | p99 | >33 ms | early |" +
        (if hasRaster then " Raster p75 | p90 | p99 | >33 ms | early |" else "") +
        "\n" + "|---|" + "---:|" * (10 + (if hasRaster then 5 else 0))
    val framesHeader =
      "| Scenario | Java2D p50 | p95 | missed | Skia p50 | p95 | missed |" +
        (if hasRaster then " Raster p50 | p95 | missed |" else "") +
        "\n" + "|---|" + "---:|" * (6 + (if hasRaster then 3 else 0))
    (List("# Render benchmark", "", environmentLine) ++ launchLines ++ List("", header) ++ rows ++ rasterLines ++ List(
      "",
      "Tail of each scenario; early is the samples whose last presentation came before the render call returned.",
      "",
      tailHeader
    ) ++ results.map(tailRow) ++ List(
      "",
      s"Frames to present: each time rounded up to whole refreshes; missed is the samples that took more than one. $refreshLine",
      "",
      framesHeader
    ) ++ results.map(framesRow) ++ List("", verdictText))
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
      s"toolkit ${environment.toolkit}, ${environment.os}, ${environment.java}.\n\n" +
      s"Machine: ${environment.machine.cpuModel}, governor ${environment.machine.cpuGovernor}, power ${environment.machine.power}."

  private def launchLines: List[String] =
    val startup = Option.when(environment.startup.nonEmpty)(s"Launch: ${environment.startup}.")
    val layers =
      environment.skiaLayers.map(count => s"Skia window ended with $count ${if count == 1 then "layer" else "layers"}.")
    val draws = environment.skiaDraws.map { stats =>
      f"Skia layer: ${stats.renders} draws for ${stats.publishes} published frames; publish to draw mean ${stats.publishToRenderMeanMs}%.2f ms (max ${stats.publishToRenderMaxMs}%.2f); " +
        f"per draw: picture ${stats.replayMeanMs}%.2f ms, carets ${stats.caretsMeanMs}%.2f ms, GPU sync ${stats.syncMeanMs}%.2f ms."
    }
    val carets = environment.skiaDraws.filter(_.caretWaits > 0).map { stats =>
      f"Skia layer: ${stats.caretWaits} changes of carets; change to draw mean ${stats.caretToRenderMeanMs}%.2f ms (max ${stats.caretToRenderMaxMs}%.2f)."
    }
    val sync = environment.skiaDraws.map(stats => s"Skia layer: GPU sync: ${stats.gpuSyncNote}.")
    val pictures = environment.skiaDraws.map { stats =>
      f"Skia layer pictures: ${stats.pictureOpsMean}%.0f ops, ${stats.pictureBytesMean / 1024}%.1f KB on average."
    }
    List(startup, layers, draws, carets, sync, pictures).flatten.map(line => s"\n$line")

  private def row(result: ScenarioResult): String =
    val java2d = result.java2d
    val skia = result.skia.fold("| - | - | - | - | - |")(s =>
      s"| ${f(s.p50Ms)} | ${f(s.p95Ms)} | ${f(s.maxMs)} | ${ratio(s.p50Ms, java2d.p50Ms)} | ${ratio(s.p95Ms, java2d.p95Ms)} |"
    )
    s"| ${result.scenario.label} | ${f(java2d.p50Ms)} | ${f(java2d.p95Ms)} | ${f(java2d.maxMs)} $skia"

  private def hasRaster: Boolean = results.exists(_.raster.isDefined)

  private def rasterLines: List[String] =
    if hasRaster then
      val rasterHeader =
        "| Scenario | Raster p50 | p95 | max | Raster/Java2D p50 | p95 |\n|---|---:|---:|---:|---:|---:|"
      List(
        "",
        "Skia raster (CPU), the same scenarios painted by Skia on the CPU and presented through Java2D:",
        "",
        rasterHeader
      ) ++
        results.flatMap(result =>
          result.raster.map(r =>
            s"| ${result.scenario.label} | ${f(r.p50Ms)} | ${f(r.p95Ms)} | ${f(r.maxMs)} | ${ratio(r.p50Ms, result.java2d.p50Ms)} | ${ratio(r.p95Ms, result.java2d.p95Ms)} |"
          )
        )
    else List("", s"Skia raster (CPU): not measured (${environment.rasterNote.getOrElse("no reason given")}).")

  private def refreshLine: String =
    val refresh = environment.refresh
    if refresh.assumed then f"Refresh: ${refresh.hz}%.0f Hz (assumed: the display reported no rate)."
    else f"Refresh: ${refresh.hz}%.0f Hz."

  private def framesRow(result: ScenarioResult): String =
    def cells(s: FrameStats) =
      val share = if s.count == 0 then 0 else math.round(100.0 * s.missedRefresh / s.count)
      s"${s.p50Frames} | ${s.p95Frames} | ${s.missedRefresh} ($share%) |"
    val skia   = result.skia.fold("- | - | - |")(cells)
    val raster = if hasRaster then result.raster.fold(" - | - | - |")(r => s" ${cells(r)}") else ""
    s"| ${result.scenario.label} | ${cells(result.java2d)} $skia$raster"

  private def tailRow(result: ScenarioResult): String =
    def cells(s: FrameStats) =
      s"${f(s.p75Ms)} | ${f(s.p90Ms)} | ${f(s.p99Ms)} | ${s.over33Ms} | ${s.endedBeforeReturn} |"
    val skia   = result.skia.fold("- | - | - | - | - |")(cells)
    val raster = if hasRaster then result.raster.fold(" - | - | - | - | - |")(r => s" ${cells(r)}") else ""
    s"| ${result.scenario.label} | ${cells(result.java2d)} $skia$raster"

  private def verdictText: String =
    verdict match
      case Verdict.SkiaWins =>
        "**Skia wins** on this machine under the owner's rule. (`auto` picks the Skia layer whatever this says.)"
      case Verdict.Java2DStays(reasons) =>
        ("**Skia does not win** on this machine under the owner's rule. (`auto` picks the Skia layer whatever this says.)" :: reasons
          .map(reason => s"- $reason")).mkString("\n")
      case Verdict.NotMeasured(reason) => s"**No verdict:** $reason."

  private def scenarioJson(result: ScenarioResult): Json =
    val base = List("scenario" -> result.scenario.label.asJson, "java2d" -> statsJson(result.java2d))
    Json.obj(
      base ++ result.skia.map(stats => "skia" -> statsJson(stats)).toList ++
        result.raster.map(stats => "raster" -> statsJson(stats)).toList*
    )

  private def statsJson(stats: FrameStats): Json =
    Json.obj(
      "count"             -> stats.count.asJson,
      "p50Ms"             -> stats.p50Ms.asJson,
      "p95Ms"             -> stats.p95Ms.asJson,
      "maxMs"             -> stats.maxMs.asJson,
      "dropped"           -> stats.dropped.asJson,
      "p75Ms"             -> stats.p75Ms.asJson,
      "p90Ms"             -> stats.p90Ms.asJson,
      "p99Ms"             -> stats.p99Ms.asJson,
      "over33Ms"          -> stats.over33Ms.asJson,
      "endedBeforeReturn" -> stats.endedBeforeReturn.asJson,
      "p50Frames"         -> stats.p50Frames.asJson,
      "p95Frames"         -> stats.p95Frames.asJson,
      "missedRefresh"     -> stats.missedRefresh.asJson
    )

  private def environmentJson: Json =
    val api =
      environment.skiaRenderApi.fold(cause => "skiaUnavailable" -> cause.asJson, api => "skiaRenderApi" -> api.asJson)
    Json.obj(
      List(
        "os"             -> environment.os.asJson,
        "java"           -> environment.java.asJson,
        "width"          -> environment.widthPx.asJson,
        "height"         -> environment.heightPx.asJson,
        "deviceScale"    -> environment.deviceScale.asJson,
        "toolkit"        -> environment.toolkit.asJson,
        "startup"        -> environment.startup.asJson,
        "refreshHz"      -> environment.refresh.hz.asJson,
        "refreshAssumed" -> environment.refresh.assumed.asJson,
        "cpuModel"       -> environment.machine.cpuModel.asJson,
        "cpuGovernor"    -> environment.machine.cpuGovernor.asJson,
        "power"          -> environment.machine.power.asJson,
        api
      ) ++ environment.rasterNote.map(note => "rasterNote" -> note.asJson).toList ++
        environment.skiaLayers.map(count => "skiaLayers" -> count.asJson).toList ++
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
      "syncedDraws"           -> stats.syncedDraws.asJson,
      "gpuContext"            -> stats.gpuContext.asJson,
      "pictureOpsMean"        -> stats.pictureOpsMean.asJson,
      "pictureBytesMean"      -> stats.pictureBytesMean.asJson,
      "caretWaits"            -> stats.caretWaits.asJson,
      "caretToRenderMeanMs"   -> stats.caretToRenderMeanMs.asJson,
      "caretToRenderMaxMs"    -> stats.caretToRenderMaxMs.asJson
    )

  private def verdictJson: Json =
    verdict match
      case Verdict.SkiaWins             => Json.obj("kind" -> "skia-wins".asJson)
      case Verdict.Java2DStays(reasons) => Json.obj("kind" -> "java2d-stays".asJson, "reasons" -> reasons.asJson)
      case Verdict.NotMeasured(reason)  => Json.obj("kind" -> "not-measured".asJson, "reason" -> reason.asJson)

  private def f(millis: Double): String = f"$millis%.2f"

  private def ratio(skia: Double, java2d: Double): String = if java2d > 0 then f"${skia / java2d}%.2fx" else "-"
