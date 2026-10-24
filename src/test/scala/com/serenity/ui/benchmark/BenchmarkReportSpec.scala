package com.serenity.ui.benchmark

import com.serenity.ui.terminal.LayerDrawStats
import io.circe.parser.parse
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BenchmarkReportSpec extends AnyFlatSpec with Matchers:

  private def stats(p50: Double, p95: Double) = FrameStats(200, p50, p95, p95 + 5, 0)

  private val environment = BenchmarkEnvironment("Linux", "Java 21", 1600, 1000, 2.0, Right("OPENGL"))

  private val results =
    Scenario.values.toVector.map(scenario => ScenarioResult(scenario, stats(10, 20), Some(stats(5, 10))))

  private val report = BenchmarkReport(environment, results, Verdict.SkiaWins)

  "BenchmarkReport.markdown" should "state the environment, one row per scenario and the verdict" in {
    val text = report.markdown
    text should include("1600x1000")
    text should include("OPENGL")
    text should include("scale 2.0")
    Scenario.values.foreach(scenario => text should include(scenario.label))
    text should include("0.50x")
    text should include("Skia wins")
  }

  it should "name the toolkit, the startup choice, the layer count and what the draws cost" in {
    val draws = LayerDrawStats(
      renders = 330,
      publishes = 300,
      publishToRenderMeanMs = 4.5,
      publishToRenderMaxMs = 40.0,
      replayMeanMs = 1.25,
      caretsMeanMs = 0.05,
      syncMeanMs = 0.5
    )
    val detailed = BenchmarkReport(
      environment.copy(
        toolkit = "sun.awt.X11.XToolkit",
        startup = "X11 (SERENITY_TOOLKIT=x11)",
        skiaLayers = Some(1),
        skiaDraws = Some(draws)
      ),
      results,
      Verdict.SkiaWins
    )
    val text = detailed.markdown
    List("sun.awt.X11.XToolkit", "SERENITY_TOOLKIT=x11", "1 layer", "330 draws for 300 published frames", "4.50")
      .foreach(part => text should include(part))
    val cursor = parse(detailed.json).getOrElse(fail("not JSON")).hcursor.downField("environment")
    cursor.downField("toolkit").as[String] shouldBe Right("sun.awt.X11.XToolkit")
    cursor.downField("skiaLayers").as[Int] shouldBe Right(1)
    cursor.downField("skiaDraws").downField("renders").as[Int] shouldBe Right(330)
  }

  it should "say whether the GPU was waited on after each draw, and why not when it could not be" in {
    val base = LayerDrawStats(1460, 1156, 9.0, 54.0, 3.4, 0.05, 0.0)
    def textFor(stats: LayerDrawStats): String =
      BenchmarkReport(environment.copy(skiaDraws = Some(stats)), results, Verdict.SkiaWins).markdown
    textFor(base.copy(syncedDraws = 0, gpuContext = false)) should include(
      "GPU sync: not done (no GPU context on the canvas Skiko draws the layer with, which records into a picture)"
    )
    textFor(base.copy(syncedDraws = 1460, gpuContext = true)) should include("GPU sync: 1460 of 1460 draws")
    val json = parse(
      BenchmarkReport(environment.copy(skiaDraws = Some(base.copy(syncedDraws = 7))), results, Verdict.SkiaWins).json
    ).getOrElse(fail("not JSON")).hcursor.downField("environment").downField("skiaDraws")
    json.downField("syncedDraws").as[Int] shouldBe Right(7)
    json.downField("gpuContext").as[Boolean] shouldBe Right(false)
  }

  it should "show the tail of each scenario and the pictures the layer was given" in {
    val tail =
      FrameStats(200, 10, 20, 30, 0, p75Ms = 12.5, p90Ms = 17.5, p99Ms = 27.5, over33Ms = 4, endedBeforeReturn = 3)
    val withTail = BenchmarkReport(
      environment.copy(skiaDraws =
        Some(LayerDrawStats(1, 1, 1.0, 1.0, 1.0, 1.0, 1.0, pictureOpsMean = 812.0, pictureBytesMean = 65536.0))
      ),
      Scenario.values.toVector.map(scenario => ScenarioResult(scenario, tail, Some(tail))),
      Verdict.SkiaWins
    )
    val text = withTail.markdown
    text should include("| Scenario | Java2D p75 |")
    val row = text.linesIterator.filter(_.contains("caret blink")).toList.lastOption.getOrElse(fail("no tail row"))
    List("12.50", "17.50", "27.50", "| 4 |", "| 3 |").foreach(figure => row should include(figure))
    text should include("pictures: 812 ops, 64.0 KB on average")
    val skia =
      parse(withTail.json).getOrElse(fail("not JSON")).hcursor.downField("scenarios").downArray.downField("skia")
    skia.downField("p99Ms").as[Double] shouldBe Right(27.5)
    skia.downField("over33Ms").as[Int] shouldBe Right(4)
    skia.downField("endedBeforeReturn").as[Int] shouldBe Right(3)
  }

  it should "show every figure of both backends in a scenario's row" in {
    val row = report.markdown.linesIterator.find(_.contains("type one character")).getOrElse(fail("no row"))
    List("10.00", "20.00", "25.00", "5.00", "10.00", "15.00").foreach(figure => row should include(figure))
  }

  it should "say GPU NOT USED instead of a verdict when Skia ran in software" in {
    val software =
      BenchmarkReport(
        environment.copy(skiaRenderApi = Right("SOFTWARE_FAST")),
        results,
        Verdict.NotMeasured("GPU NOT USED (software fallback: SOFTWARE_FAST)")
      )
    software.markdown should include("GPU NOT USED (software fallback: SOFTWARE_FAST)")
    software.markdown should not include "Skia wins"
  }

  it should "give a reason for each scenario that keeps Java2D" in {
    val stays = BenchmarkReport(environment, results, Verdict.Java2DStays(List("scroll one line: p95 not better")))
    stays.markdown should include("auto stays on Java2D")
    stays.markdown should include("scroll one line: p95 not better")
  }

  "BenchmarkReport.json" should "carry the environment, every scenario's figures and the verdict" in {
    val json   = parse(report.json).getOrElse(fail("not JSON"))
    val cursor = json.hcursor
    cursor.downField("environment").downField("skiaRenderApi").as[String] shouldBe Right("OPENGL")
    cursor.downField("environment").downField("width").as[Int] shouldBe Right(1600)
    cursor.downField("scenarios").as[Vector[io.circe.Json]].map(_.size) shouldBe Right(Scenario.values.length)
    cursor.downField("scenarios").downArray.downField("java2d").downField("p50Ms").as[Double] shouldBe Right(10.0)
    cursor.downField("scenarios").downArray.downField("skia").downField("p95Ms").as[Double] shouldBe Right(10.0)
    cursor.downField("verdict").downField("kind").as[String] shouldBe Right("skia-wins")
  }

  it should "leave out the Skia figures when Skia was not measured" in {
    val unmeasured = BenchmarkReport(
      environment.copy(skiaRenderApi = Left("no GL context")),
      results.map(_.copy(skia = None)),
      Verdict.NotMeasured("Skia layer unavailable: no GL context")
    )
    val json = parse(unmeasured.json).getOrElse(fail("not JSON"))
    json.hcursor.downField("scenarios").downArray.downField("skia").succeeded shouldBe false
    json.hcursor.downField("verdict").downField("kind").as[String] shouldBe Right("not-measured")
    json.hcursor.downField("verdict").downField("reason").as[String] shouldBe Right(
      "Skia layer unavailable: no GL context"
    )
  }
