package com.serenity.ui.benchmark

import com.serenity.ui.skia.FallbackAgreement
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

  it should "state the CPU, its governor and the power source, in the text and the JSON" in {
    val machine     = environment.copy(machine = MachineState("AMD Ryzen 7 7840U", "powersave", "battery"))
    val withMachine = BenchmarkReport(machine, results, Verdict.SkiaWins)

    withMachine.markdown should include("AMD Ryzen 7 7840U")
    withMachine.markdown should include("governor powersave")
    withMachine.markdown should include("battery")
    val json = parse(withMachine.json).toOption.get.hcursor.downField("environment")
    json.downField("cpuModel").as[String] shouldBe Right("AMD Ryzen 7 7840U")
    json.downField("cpuGovernor").as[String] shouldBe Right("powersave")
    json.downField("power").as[String] shouldBe Right("battery")
  }

  it should "say unknown for a machine it could not read" in {
    report.markdown should include("governor unknown")
    val json = parse(report.json).toOption.get.hcursor.downField("environment")
    json.downField("cpuGovernor").as[String] shouldBe Right("unknown")
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

  it should "say where a cold frame's time goes in the Skia caches" in {
    val phases = ColdFramePhases(
      frames = 20,
      facesPerFrame = 2.0,
      faceMsPerFrame = 19.9,
      shapersPerFrame = 1.0,
      shaperMsPerFrame = 2.5,
      shapingsPerFrame = 165.0,
      shapingMsPerFrame = 22.8
    )
    val detailed = BenchmarkReport(environment.copy(coldPhases = Some(phases)), results, Verdict.SkiaWins)
    detailed.markdown should include(
      "Cold frame, Skia caches (mean of 20): 2.0 faces 19.90 ms, 1.0 shapers 2.50 ms, 165 shapings 22.80 ms."
    )
    val json = parse(detailed.json).getOrElse(fail("not JSON")).hcursor.downField("environment").downField("coldPhases")
    json.downField("frames").as[Int] shouldBe Right(20)
    json.downField("faceMsPerFrame").as[Double] shouldBe Right(19.9)
    json.downField("shaperMsPerFrame").as[Double] shouldBe Right(2.5)
    json.downField("shapingsPerFrame").as[Double] shouldBe Right(165.0)
    json.downField("shapingMsPerFrame").as[Double] shouldBe Right(22.8)
  }

  it should "split a cold frame into layout, drawing, the rest and the wait to be presented, for each backend" in {
    val java2d = ColdFrameSplit(20, renderMs = 70.0, layoutMs = 15.5, drawMs = 45.25, otherMs = 9.25, presentMs = 5.0)
    val skia   = ColdFrameSplit(20, renderMs = 80.0, layoutMs = 15.0, drawMs = 55.0, otherMs = 10.0, presentMs = 6.75)
    val detailed =
      BenchmarkReport(
        environment.copy(java2dColdSplit = Some(java2d), skiaColdSplit = Some(skia)),
        results,
        Verdict.SkiaWins
      )
    detailed.markdown should include(
      "Cold frame split, Java2D (mean of 20): render call 70.00 ms (layout 15.50, drawing 45.25, rest 9.25), then 5.00 ms until presented."
    )
    detailed.markdown should include(
      "Cold frame split, Skia layer (mean of 20): render call 80.00 ms (layout 15.00, drawing 55.00, rest 10.00), then 6.75 ms until presented."
    )
    val json = parse(detailed.json).getOrElse(fail("not JSON")).hcursor.downField("environment").downField("coldSplit")
    json.downField("java2d").downField("frames").as[Int] shouldBe Right(20)
    json.downField("java2d").downField("layoutMs").as[Double] shouldBe Right(15.5)
    json.downField("skia").downField("drawMs").as[Double] shouldBe Right(55.0)
    json.downField("skia").downField("presentMs").as[Double] shouldBe Right(6.75)
  }

  it should "say how many strings the fallback runs shape exactly as TextLine does on this machine" in {
    val agreement = FallbackAgreement(
      checked = 1008,
      identical = 1006,
      differing = Vector("text (Inter) U+2502", "code (Mono) U+2192")
    )
    val detailed = BenchmarkReport(environment.copy(fallbackAgreement = Some(agreement)), results, Verdict.SkiaWins)

    detailed.markdown should include(
      "Fallback runs against TextLine on this machine: 1006 of 1008 strings identical. Differing: text (Inter) U+2502, code (Mono) U+2192."
    )
    val json =
      parse(detailed.json).getOrElse(fail("not JSON")).hcursor.downField("environment").downField("fallbackAgreement")
    json.downField("checked").as[Int] shouldBe Right(1008)
    json.downField("identical").as[Int] shouldBe Right(1006)
    json.downField("differing").as[List[String]] shouldBe Right(List("text (Inter) U+2502", "code (Mono) U+2192"))
  }

  it should "leave out the differing list when every string is identical, and the line when nothing was checked" in {
    val clean = BenchmarkReport(
      environment.copy(fallbackAgreement = Some(FallbackAgreement(1008, 1008, Vector.empty))),
      results,
      Verdict.SkiaWins
    )
    clean.markdown should include("Fallback runs against TextLine on this machine: 1008 of 1008 strings identical.")
    clean.markdown should not include "Differing"
    BenchmarkReport(environment, results, Verdict.SkiaWins).markdown should not include "Fallback runs against TextLine"
  }

  it should "leave the cold-frame split out when no cold frame was measured" in {
    val text = BenchmarkReport(environment, results, Verdict.SkiaWins)
    text.markdown should not include "Cold frame split"
    parse(text.json).getOrElse(fail("not JSON")).hcursor.downField("environment").downField("coldSplit").failed shouldBe
      true
  }

  it should "leave the cold-frame line out when no Skia cache was measured" in {
    BenchmarkReport(environment, results, Verdict.SkiaWins).markdown should not include "Cold frame, Skia caches"
  }

  it should "say how long a change of carets waited for its draw" in {
    val draws = LayerDrawStats(
      renders = 330,
      publishes = 300,
      publishToRenderMeanMs = 4.5,
      publishToRenderMaxMs = 40.0,
      replayMeanMs = 1.25,
      caretsMeanMs = 0.05,
      syncMeanMs = 0.5,
      caretWaits = 120,
      caretToRenderMeanMs = 0.45,
      caretToRenderMaxMs = 3.2
    )
    val detailed = BenchmarkReport(environment.copy(skiaDraws = Some(draws)), results, Verdict.SkiaWins)
    detailed.markdown should include("120 changes of carets; change to draw mean 0.45 ms (max 3.20)")
    val json = parse(detailed.json).getOrElse(fail("not JSON")).hcursor.downField("environment").downField("skiaDraws")
    json.downField("caretWaits").as[Int] shouldBe Right(120)
    json.downField("caretToRenderMeanMs").as[Double] shouldBe Right(0.45)
    json.downField("caretToRenderMaxMs").as[Double] shouldBe Right(3.2)
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
    val tailTable = text.linesIterator.dropWhile(!_.contains("| Scenario | Java2D p75 |")).takeWhile(_.nonEmpty).toList
    val row       = tailTable.find(_.contains("caret blink")).getOrElse(fail("no tail row"))
    List("12.50", "17.50", "27.50", "| 4 |", "| 3 |").foreach(figure => row should include(figure))
    text should include("pictures: 812 ops, 64.0 KB on average")
    val skia =
      parse(withTail.json).getOrElse(fail("not JSON")).hcursor.downField("scenarios").downArray.downField("skia")
    skia.downField("p99Ms").as[Double] shouldBe Right(27.5)
    skia.downField("over33Ms").as[Int] shouldBe Right(4)
    skia.downField("endedBeforeReturn").as[Int] shouldBe Right(3)
  }

  it should "say what the refresh rate is, and whether it was assumed" in {
    val assumed = report.copy(environment = environment.copy(refresh = RefreshRate.of(0))).markdown
    assumed should include("Refresh: 60 Hz (assumed: the display reported no rate)")
    val reported = report.copy(environment = environment.copy(refresh = RefreshRate.of(144))).markdown
    reported should include("Refresh: 144 Hz")
    reported should not include "assumed"
  }

  it should "show the frames to present of each scenario, and how many samples missed a refresh" in {
    val slow = FrameStats(
      200,
      10,
      20,
      30,
      0,
      p75Ms = 12,
      p90Ms = 18,
      p99Ms = 28,
      over33Ms = 0,
      endedBeforeReturn = 0,
      p50Frames = 2,
      p95Frames = 4,
      missedRefresh = 50
    )
    val fast = FrameStats(200, 5, 10, 15, 0, p50Frames = 1, p95Frames = 1, missedRefresh = 0)
    val withFrames = BenchmarkReport(
      environment,
      Scenario.values.toVector.map(scenario => ScenarioResult(scenario, slow, Some(fast))),
      Verdict.SkiaWins
    )
    val text = withFrames.markdown
    text should include("Frames to present")
    text should include("| Scenario | Java2D p50 | p95 | missed | Skia p50 | p95 | missed |")
    val framesTable = text.linesIterator.dropWhile(!_.contains("| Scenario | Java2D p50 | p95 | missed |")).toList
    val row         = framesTable.find(_.contains("page down")).getOrElse(fail("no frames row"))
    List("| 2 |", "| 4 |", "50 (25%)", "0 (0%)").foreach(cell => row should include(cell))
    val json = parse(withFrames.json).getOrElse(fail("not JSON")).hcursor
    json.downField("environment").downField("refreshHz").as[Double] shouldBe Right(60.0)
    json.downField("environment").downField("refreshAssumed").as[Boolean] shouldBe Right(true)
    val java2d = json.downField("scenarios").downArray.downField("java2d")
    java2d.downField("p50Frames").as[Int] shouldBe Right(2)
    java2d.downField("missedRefresh").as[Int] shouldBe Right(50)
  }

  it should "show the raster arm beside the other two in every table, and give the reason when it is missing" in {
    val raster = FrameStats(
      200,
      8,
      16,
      24,
      0,
      p75Ms = 9,
      p90Ms = 14,
      p99Ms = 22,
      over33Ms = 2,
      endedBeforeReturn = 1,
      p50Frames = 1,
      p95Frames = 1,
      missedRefresh = 10
    )
    val withRaster = BenchmarkReport(
      environment,
      Scenario.values.toVector.map(scenario =>
        ScenarioResult(scenario, stats(10, 20), Some(stats(5, 10)), Some(raster))
      ),
      Verdict.SkiaWins
    )
    val text = withRaster.markdown
    text should include("Skia raster (CPU)")
    val rasterRow = text.linesIterator
      .dropWhile(!_.contains("Skia raster (CPU)"))
      .find(_.contains("page down"))
      .getOrElse(fail("no raster row"))
    List("8.00", "16.00", "24.00", "0.80x", "0.80x").foreach(figure => rasterRow should include(figure))
    text should include(
      "| Scenario | Java2D p75 | p90 | p99 | >33 ms | early | Skia p75 | p90 | p99 | >33 ms | early | Raster p75 | p90 | p99 | >33 ms | early |"
    )
    text should include(
      "| Scenario | Java2D p50 | p95 | missed | Skia p50 | p95 | missed | Raster p50 | p95 | missed |"
    )
    parse(withRaster.json)
      .getOrElse(fail("not JSON"))
      .hcursor
      .downField("scenarios")
      .downArray
      .downField("raster")
      .downField("p99Ms")
      .as[Double] shouldBe Right(22.0)
    val without = report
      .copy(environment = environment.copy(rasterNote = Some("the raster arm failed: no raster surface")))
      .markdown
    without should include("Skia raster (CPU): not measured (the raster arm failed: no raster surface)")
    without should not include "Raster p75"
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
    stays.markdown should include("Skia does not win")
    stays.markdown should include("scroll one line: p95 not better")
  }

  it should "say that auto no longer depends on the verdict, whichever it is" in {
    val wins  = BenchmarkReport(environment, results, Verdict.SkiaWins).markdown
    val stays = BenchmarkReport(environment, results, Verdict.Java2DStays(List("scroll one line"))).markdown
    List(wins, stays).foreach(text => text should include("`auto` picks the Skia layer whatever this says"))
    List(wins, stays).foreach(text => text should not include "auto stays on Java2D")
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
