package com.serenity.app

import com.serenity.app.RendererChoice.*
import com.serenity.app.RendererTarget.Verdict
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RendererTargetSpec extends AnyFlatSpec with Matchers:

  private val SixMegapixels = 6_000_000L

  private def run(
    p95: Double,
    scale: Double = 2.0,
    resolution: Double = 1.0,
    canvasPixels: Long = SixMegapixels
  ): Outcome =
    Outcome.Measured(
      Workload(
        typingP50Ms = p95 / 2,
        typingP95Ms = p95,
        scale = scale,
        clockResolutionMs = resolution,
        fullWindowMs = Map.empty,
        canvasPixels = canvasPixels
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

  it should "judge by the pixels the trial drew, whatever scale the JVM reported: 2x is a canvas of at least 5 megapixels" in {
    RendererTarget.judge(runs(Candidate.LayerImage, run(10.0, scale = 1.0, canvasPixels = 5_000_000L))) shouldBe
      Verdict.Meets(10.0)
    RendererTarget.judge(runs(Candidate.LayerImage, run(40.0, scale = 1.0, canvasPixels = 12_000_000L))) shouldBe
      a[Verdict.Misses]
  }

  it should "not judge a canvas with fewer pixels than that, and say how many it had and how many it needs" in {
    RendererTarget.judge(runs(Candidate.LayerImage, run(10.0, scale = 2.0, canvasPixels = 2_722_440L))) match
      case Verdict.NotJudged(reason) => reason should (include("2.7 megapixels") and include("5.0 megapixels"))
      case other                     => fail(s"expected no verdict, got $other")
  }

  it should "not judge a run whose canvas size was not recorded" in {
    RendererTarget.judge(runs(Candidate.LayerImage, run(10.0, canvasPixels = 0L))) match
      case Verdict.NotJudged(reason) => reason should include("not recorded")
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

  it should "judge a mode by the smallest canvas any of its runs drew on" in {
    RendererTarget.judge(runs(Candidate.LayerImage, run(10.0), run(10.0, canvasPixels = 3_000_000L))) shouldBe
      a[Verdict.NotJudged]
    RendererTarget.judge(runs(Candidate.LayerImage, run(10.0), run(14.0, canvasPixels = 8_000_000L))) shouldBe
      Verdict.Meets(12.0)
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
    val lines = RendererTarget.lines(List(runs(Candidate.LayerImage, run(10.0, canvasPixels = 1_000_000L))))
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

  it should "say a verdict is on the in-app clock, a lower bound, when the runs were measured by it" in {
    val inApp =
      Outcome.Measured(Workload(8.0, 12.0, 2.0, 0.0, Map.empty, clock = Clock.InApp, canvasPixels = SixMegapixels))
    val text = RendererTarget.lines(List(runs(Candidate.LayerImage, inApp))).mkString("\n")
    text should (include("target met") and include("clock=in-app") and include("lower bound"))
  }
