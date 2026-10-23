package com.serenity.ui.benchmark

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BenchmarkVerdictSpec extends AnyFlatSpec with Matchers:

  private def stats(p50: Double, p95: Double, dropped: Int = 0) = FrameStats(200, p50, p95, p95 * 2, dropped)

  private val gpu =
    BenchmarkEnvironment("Linux", "Java 21", 1600, 1000, 1.0, Right("OPENGL"))

  private val java2d = Map(
    Scenario.ColdFullFrame   -> stats(20, 30),
    Scenario.TypeCharacter   -> stats(6, 9),
    Scenario.ScrollOneLine   -> stats(7, 10),
    Scenario.ScrollFiveLines -> stats(8, 12),
    Scenario.PageDown        -> stats(15, 22),
    Scenario.CaretBlink      -> stats(2, 3)
  )

  private def results(skia: Map[Scenario, FrameStats]) =
    Scenario.values.toVector.map(scenario => ScenarioResult(scenario, java2d(scenario), skia.get(scenario)))

  private val allFaster = Scenario.values.map(s => s -> stats(java2d(s).p50Ms * 0.5, java2d(s).p95Ms * 0.5)).toMap

  "BenchmarkVerdict.of" should "let Skia win when it is no worse anywhere and better at scrolling and page down" in {
    BenchmarkVerdict.of(results(allFaster), gpu) shouldBe Verdict.SkiaWins
  }

  it should "refuse a verdict when the window ended with more than one Skia layer" in {
    BenchmarkVerdict.of(results(allFaster), gpu.copy(skiaLayers = Some(3))) match
      case Verdict.NotMeasured(reason) =>
        reason should include("3")
        reason should include("layer")
      case other => fail(s"stacked layers got a verdict: $other")
  }

  it should "judge as usual with exactly one Skia layer" in {
    BenchmarkVerdict.of(results(allFaster), gpu.copy(skiaLayers = Some(1))) shouldBe Verdict.SkiaWins
  }

  it should "forgive a slower cold full frame" in {
    val slowerCold = allFaster.updated(Scenario.ColdFullFrame, stats(40, 60))
    BenchmarkVerdict.of(results(slowerCold), gpu) shouldBe Verdict.SkiaWins
  }

  it should "keep Java2D when Skia's p50 is worse in any other scenario, naming it" in {
    val slowerTyping = allFaster.updated(Scenario.TypeCharacter, stats(7, 5))
    BenchmarkVerdict.of(results(slowerTyping), gpu) match
      case Verdict.Java2DStays(reasons) =>
        reasons.exists(r => r.contains("type one character") && r.contains("p50")) shouldBe true
      case other => fail(s"expected Java2D to stay, got $other")
  }

  it should "keep Java2D when Skia's p95 is worse in any other scenario, naming it" in {
    val slowerBlink = allFaster.updated(Scenario.CaretBlink, stats(1, 4))
    BenchmarkVerdict.of(results(slowerBlink), gpu) match
      case Verdict.Java2DStays(reasons) =>
        reasons.exists(r => r.contains("caret blink") && r.contains("p95")) shouldBe true
      case other => fail(s"expected Java2D to stay, got $other")
  }

  it should "need Skia's p95 strictly better at scrolling and page down" in
    List(Scenario.ScrollOneLine, Scenario.ScrollFiveLines, Scenario.PageDown).foreach { scenario =>
      val tied = allFaster.updated(scenario, stats(java2d(scenario).p50Ms * 0.5, java2d(scenario).p95Ms))
      withClue(s"${scenario.label}: ") {
        BenchmarkVerdict.of(results(tied), gpu) match
          case Verdict.Java2DStays(reasons) => reasons.exists(_.contains(scenario.label)) shouldBe true
          case other                        => fail(s"expected Java2D to stay, got $other")
      }
    }

  it should "not call a software fallback a result, whatever the numbers say" in {
    val software = gpu.copy(skiaRenderApi = Right("SOFTWARE_FAST"))
    BenchmarkVerdict.of(results(allFaster), software) shouldBe
      Verdict.NotMeasured("GPU NOT USED (software fallback: SOFTWARE_FAST)")
  }

  it should "say so when Skia could not be made" in {
    val unavailable = gpu.copy(skiaRenderApi = Left("no GL context"))
    BenchmarkVerdict.of(results(Map.empty), unavailable) shouldBe
      Verdict.NotMeasured("Skia layer unavailable: no GL context")
  }

  it should "not judge a run that dropped frames" in {
    val dropped = allFaster.updated(Scenario.PageDown, stats(1, 1, dropped = 3))
    BenchmarkVerdict.of(results(dropped), gpu) match
      case Verdict.NotMeasured(reason) => reason should include("dropped")
      case other                       => fail(s"expected no verdict, got $other")
  }

  it should "not judge a run with a scenario missing" in {
    BenchmarkVerdict.of(results(allFaster - Scenario.PageDown), gpu) match
      case Verdict.NotMeasured(reason) => reason should include("page down")
      case other                       => fail(s"expected no verdict, got $other")
  }
