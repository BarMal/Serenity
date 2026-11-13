package com.serenity.ui.benchmark

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BenchmarkVerdictSpec extends AnyFlatSpec with Matchers:

  private def stats(p50: Double, p95: Double, dropped: Int = 0) = FrameStats(200, p50, p95, p95 * 2, dropped)

  private val gpu =
    BenchmarkEnvironment("Linux", "Java 21", 1600, 1000, 1.0, Right("OPENGL"))

  private val timed = Scenario.values.map(scenario => scenario -> stats(10, 15)).toMap

  private def results(skia: Map[Scenario, FrameStats]) =
    Scenario.values.toVector.map(scenario => ScenarioResult(scenario, skia.get(scenario)))

  "BenchmarkVerdict.of" should "call the figures measured when every judged scenario was timed on a GPU" in {
    BenchmarkVerdict.of(results(timed), gpu) shouldBe Verdict.Measured
  }

  it should "not let a raster arm change the verdict" in {
    val withRaster = results(timed).map(_.copy(raster = Some(stats(1000, 2000))))
    BenchmarkVerdict.of(withRaster, gpu) shouldBe BenchmarkVerdict.of(results(timed), gpu)
  }

  it should "refuse a verdict when the window ended with more than one Skia layer" in {
    BenchmarkVerdict.of(results(timed), gpu.copy(skiaLayers = Some(3))) match
      case Verdict.NotMeasured(reason) =>
        reason should include("3")
        reason should include("layer")
      case other => fail(s"stacked layers got a verdict: $other")
  }

  it should "judge as usual with exactly one Skia layer" in {
    BenchmarkVerdict.of(results(timed), gpu.copy(skiaLayers = Some(1))) shouldBe Verdict.Measured
  }

  it should "not call a software fallback a result, whatever the numbers say" in {
    val software = gpu.copy(skiaRenderApi = Right("SOFTWARE_FAST"))
    BenchmarkVerdict.of(results(timed), software) shouldBe
      Verdict.NotMeasured("GPU NOT USED (software fallback: SOFTWARE_FAST)")
  }

  it should "say so when Skia could not be made" in {
    val unavailable = gpu.copy(skiaRenderApi = Left("no GL context"))
    BenchmarkVerdict.of(results(Map.empty), unavailable) shouldBe
      Verdict.NotMeasured("Skia layer unavailable: no GL context")
  }

  it should "not judge a run that dropped frames" in {
    val dropped = timed.updated(Scenario.PageDown, stats(1, 1, dropped = 3))
    BenchmarkVerdict.of(results(dropped), gpu) match
      case Verdict.NotMeasured(reason) => reason should include("dropped")
      case other                       => fail(s"expected no verdict, got $other")
  }

  it should "not judge a run with a scenario missing" in {
    BenchmarkVerdict.of(results(timed - Scenario.PageDown), gpu) match
      case Verdict.NotMeasured(reason) => reason should include("page down")
      case other                       => fail(s"expected no verdict, got $other")
  }

  it should "not judge a run whose Skia layer has no figure for a scenario" in {
    val results = Scenario.values.toVector.map(scenario =>
      ScenarioResult(scenario, Option.when(scenario != Scenario.CaretBlink)(stats(10, 15)))
    )

    BenchmarkVerdict.of(results, gpu) match
      case Verdict.NotMeasured(reason) => reason should include("caret blink")
      case other                       => fail(s"expected no verdict, got $other")
  }

  it should "not let dropped frames in a scenario outside the judged ones withhold the verdict" in
    List(Scenario.WarmFullFrame, Scenario.ResizeWindow).foreach { scenario =>
      val dropped = timed.updated(scenario, stats(1, 1, dropped = 3))
      withClue(scenario.label) {
        BenchmarkVerdict.of(results(dropped), gpu) shouldBe Verdict.Measured
      }
    }
