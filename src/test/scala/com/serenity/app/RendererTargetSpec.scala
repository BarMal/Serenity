package com.serenity.app

import com.serenity.app.RendererChoice.*
import com.serenity.app.RendererTarget.Verdict
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RendererTargetSpec extends AnyFlatSpec with Matchers:

  private def run(p95: Double, scale: Double = 2.0, resolution: Double = 1.0): Outcome =
    Outcome.Measured(
      Workload(
        typingP50Ms = p95 / 2,
        typingP95Ms = p95,
        scale = scale,
        clockResolutionMs = resolution,
        fullWindowMs = Map.empty
      )
    )

  private def runs(candidate: Candidate, outcomes: Outcome*): CandidateRuns = CandidateRuns(candidate, outcomes.toList)

  "RendererTarget.judge" should "say a mode meets the target when its typing p95 at 2x is at or below 16.7 ms" in {
    RendererTarget.judge(runs(Candidate.LayerImage, run(16.7), run(12.0))) shouldBe Verdict.Meets((12.0 + 16.7) / 2.0)
  }

  it should "say by how much a mode misses it, from the median of its runs' p95" in {
    RendererTarget.judge(runs(Candidate.SkiaRaster, run(46.0), run(50.0), run(60.0))) match
      case Verdict.Misses(p95, over) =>
        p95 shouldBe 50.0
        over shouldBe (33.3 +- 1e-9)
      case other => fail(s"expected a miss, got $other")
  }

  it should "not judge a run at another scale, and say what scale it ran at" in {
    RendererTarget.judge(runs(Candidate.LayerImage, run(10.0, scale = 1.0))) match
      case Verdict.NotJudged(reason) => reason should (include("1.0x") and include("2x"))
      case other                     => fail(s"expected no verdict, got $other")
  }

  it should "not judge a mode whose clock resolves only coarsely, and say how coarsely" in {
    RendererTarget.judge(runs(Candidate.LayerImage, run(10.0, resolution = 5.0), run(10.0, resolution = 6.0))) match
      case Verdict.NotJudged(reason) => reason should (include("resolves only to 5.5 ms") and include("4.2 ms"))
      case other                     => fail(s"expected no verdict, got $other")
  }

  it should "judge a clock that resolves to exactly a quarter of the target" in {
    RendererTarget.judge(runs(Candidate.LayerImage, run(10.0, resolution = 16.7 / 4.0))) shouldBe a[Verdict.Meets]
  }

  it should "not judge runs that were measured at different scales" in {
    RendererTarget.judge(runs(Candidate.LayerImage, run(10.0), run(10.0, scale = 1.0))) shouldBe a[Verdict.NotJudged]
  }

  it should "not judge a mode with no completed run" in {
    RendererTarget.judge(runs(Candidate.LayerPicture, Outcome.Failed("fell back to the terminal"))) match
      case Verdict.NotJudged(reason) => reason should include("no completed run")
      case other                     => fail(s"expected no verdict, got $other")
    RendererTarget.judge(runs(Candidate.LayerPicture)) shouldBe a[Verdict.NotJudged]
  }

  "RendererTarget.lines" should "say no mode reaches the target when every judged mode misses it" in {
    val lines = RendererTarget.lines(
      List(runs(Candidate.LayerImage, run(46.0)), runs(Candidate.SkiaRaster, run(80.0)))
    )
    lines should contain("layer+image: typing p95 46.0 ms at 2x, 16.7 ms target missed by 29.3 ms")
    lines should contain("skia-raster: typing p95 80.0 ms at 2x, 16.7 ms target missed by 63.3 ms")
    lines.last should (include("No renderer mode reaches") and include("16.7 ms") and include("#2489"))
  }

  it should "name the modes that meet the target instead of saying none does" in {
    val lines = RendererTarget.lines(List(runs(Candidate.LayerImage, run(15.0)), runs(Candidate.SkiaRaster, run(80.0))))
    lines should contain("layer+image: typing p95 15.0 ms at 2x, 16.7 ms target met")
    lines.last should (include("Meets the 16.7 ms target") and include("layer+image"))
    lines.last should not include "skia-raster"
  }

  it should "report modes it could not judge without claiming none reaches the target" in {
    val lines = RendererTarget.lines(List(runs(Candidate.LayerImage, run(10.0, scale = 1.0))))
    lines.head should (include("layer+image") and include("not judged"))
    lines.mkString("\n") should not include "No renderer mode reaches"
  }

  "RendererTarget.annotate" should "append the lines to a recommendation without changing what it recommends" in {
    val results = List(runs(Candidate.LayerImage, run(46.0)))
    val switch  = Recommendation.Switch(Candidate.LayerImage, List("evidence one"))
    val keep    = Recommendation.Keep("no clear winner", List("evidence two"))
    RendererTarget.annotate(switch, results) should matchPattern {
      case Recommendation.Switch(Candidate.LayerImage, "evidence one" :: _ :: _) =>
    }
    RendererTarget.annotate(keep, results) should matchPattern {
      case Recommendation.Keep("no clear winner", "evidence two" :: _ :: _) =>
    }
    RendererTarget.annotate(keep, results).reason shouldBe keep.reason
  }
