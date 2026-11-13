package com.serenity.app

import com.serenity.app.RendererChoice.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RendererChoiceSpec extends AnyFlatSpec with Matchers:

  private def workload(typing: Double, rows: (String, Double)*): Workload =
    Workload(
      typingP50Ms = typing,
      typingP95Ms = typing,
      scale = 2.0,
      clockResolutionMs = 1.0,
      fullWindowMs = rows.toMap
    )

  private def measured(candidate: Candidate, runs: Workload*): CandidateRuns =
    CandidateRuns(candidate, runs.toList.map(Outcome.Measured(_)))

  private def failed(candidate: Candidate, reason: String): CandidateRuns =
    CandidateRuns(candidate, List(Outcome.Failed(reason)))

  "RendererChoice.percentile" should "take the nearest rank: the smallest value with at least that share of the samples at or below it" in {
    percentile((1 to 150).map(_.toDouble).toList, 95) shouldBe 143.0
    percentile((1 to 100).map(_.toDouble).toList, 95) shouldBe 95.0
    percentile(List(10.0, 20.0), 95) shouldBe 20.0
    percentile(List(7.0), 95) shouldBe 7.0
  }

  it should "not depend on the order the samples came in" in {
    percentile(List(30.0, 10.0, 20.0, 40.0), 50) shouldBe 20.0
    percentile(List(40.0, 20.0, 10.0, 30.0), 50) shouldBe 20.0
  }

  "RendererChoice.recommend" should "switch to a candidate that is clearly better at typing and no worse on full-window rows" in {
    val result = recommend(
      current = Candidate.SkiaRaster,
      List(
        measured(Candidate.LayerImage, workload(30.0, "page_down" -> 40.0, "full_redraw" -> 60.0)),
        measured(Candidate.SkiaRaster, workload(50.0, "page_down" -> 40.0, "full_redraw" -> 60.0))
      )
    )
    result should matchPattern { case Recommendation.Switch(Candidate.LayerImage, _) => }
  }

  it should "keep the current mode when it is the clear winner" in {
    val result = recommend(
      current = Candidate.LayerImage,
      List(
        measured(Candidate.LayerImage, workload(30.0, "page_down" -> 40.0)),
        measured(Candidate.SkiaRaster, workload(50.0, "page_down" -> 40.0))
      )
    )
    result should matchPattern { case Recommendation.Keep(_, _) => }
    result.reason should include("already")
  }

  it should "find no clear winner when the typing gap is under 15%" in {
    val result = recommend(
      current = Candidate.SkiaRaster,
      List(
        measured(Candidate.LayerImage, workload(44.0, "page_down" -> 40.0)),
        measured(Candidate.SkiaRaster, workload(50.0, "page_down" -> 40.0))
      )
    )
    result should matchPattern { case Recommendation.Keep(_, _) => }
    result.reason should include("no clear winner")
  }

  it should "count exactly 15% as a win" in {
    val result = recommend(
      current = Candidate.SkiaRaster,
      List(
        measured(Candidate.LayerImage, workload(42.5, "page_down" -> 40.0)),
        measured(Candidate.SkiaRaster, workload(50.0, "page_down" -> 40.0))
      )
    )
    result should matchPattern { case Recommendation.Switch(Candidate.LayerImage, _) => }
  }

  it should "not switch to a candidate that is more than 15% worse on any full-window row" in {
    val result = recommend(
      current = Candidate.SkiaRaster,
      List(
        measured(Candidate.LayerImage, workload(30.0, "page_down" -> 40.0, "full_redraw" -> 90.0)),
        measured(Candidate.SkiaRaster, workload(50.0, "page_down" -> 40.0, "full_redraw" -> 60.0))
      )
    )
    result should matchPattern { case Recommendation.Keep(_, _) => }
    result.reason should include("full_redraw")
  }

  it should "allow a full-window row exactly 15% worse" in {
    val result = recommend(
      current = Candidate.SkiaRaster,
      List(
        measured(Candidate.LayerImage, workload(30.0, "full_redraw" -> 69.0)),
        measured(Candidate.SkiaRaster, workload(50.0, "full_redraw" -> 60.0))
      )
    )
    result should matchPattern { case Recommendation.Switch(Candidate.LayerImage, _) => }
  }

  it should "compare only the rows both candidates measured" in {
    val result = recommend(
      current = Candidate.SkiaRaster,
      List(
        measured(Candidate.LayerImage, workload(30.0, "page_down" -> 40.0)),
        measured(Candidate.SkiaRaster, workload(50.0, "page_down" -> 40.0, "full_redraw" -> 1.0))
      )
    )
    result should matchPattern { case Recommendation.Switch(Candidate.LayerImage, _) => }
  }

  it should "need a gap of more than twice the spread between a candidate's own runs" in {
    val noisy = recommend(
      current = Candidate.SkiaRaster,
      List(
        measured(Candidate.LayerImage, workload(30.0), workload(45.0)),
        measured(Candidate.SkiaRaster, workload(60.0), workload(61.0))
      )
    )
    noisy should matchPattern { case Recommendation.Keep(_, _) => }
    noisy.reason should include("spread")

    val steady = recommend(
      current = Candidate.SkiaRaster,
      List(
        measured(Candidate.LayerImage, workload(30.0), workload(32.0)),
        measured(Candidate.SkiaRaster, workload(60.0), workload(61.0))
      )
    )
    steady should matchPattern { case Recommendation.Switch(Candidate.LayerImage, _) => }
  }

  it should "use the median of a candidate's runs" in {
    val result = recommend(
      current = Candidate.SkiaRaster,
      List(
        measured(Candidate.LayerImage, workload(30.0), workload(31.0), workload(32.0)),
        measured(Candidate.SkiaRaster, workload(50.0), workload(51.0), workload(52.0))
      )
    )
    result should matchPattern { case Recommendation.Switch(Candidate.LayerImage, _) => }
  }

  it should "never choose a candidate that failed, and say why it failed" in {
    val result = recommend(
      current = Candidate.SkiaRaster,
      List(
        failed(Candidate.LayerImage, "fell back to the terminal"),
        measured(Candidate.SkiaRaster, workload(50.0, "page_down" -> 40.0))
      )
    )
    result should matchPattern { case Recommendation.Keep(_, _) => }
    result.reason should include("fell back to the terminal")
  }

  it should "report when every candidate failed" in {
    val result = recommend(
      current = Candidate.SkiaRaster,
      List(failed(Candidate.LayerImage, "timed out"), failed(Candidate.SkiaRaster, "no window"))
    )
    result should matchPattern { case Recommendation.Keep(_, _) => }
    result.reason should (include("timed out") and include("no window"))
  }

  it should "treat a candidate with any failed run as failed" in {
    val mixed = CandidateRuns(
      Candidate.LayerImage,
      List(Outcome.Measured(workload(30.0)), Outcome.Failed("window closed"))
    )
    val result = recommend(Candidate.SkiaRaster, List(mixed, measured(Candidate.SkiaRaster, workload(50.0))))
    result should matchPattern { case Recommendation.Keep(_, _) => }
    result.reason should include("window closed")
  }

  it should "not compare a lone measured candidate with nothing" in {
    val result = recommend(current = Candidate.SkiaRaster, List(measured(Candidate.SkiaRaster, workload(50.0))))
    result should matchPattern { case Recommendation.Keep(_, _) => }
    result.reason should include("only one")
  }

  it should "pick the best of three when it beats both of the others" in {
    val result = recommend(
      current = Candidate.SkiaRaster,
      List(
        measured(Candidate.LayerImage, workload(30.0, "page_down" -> 40.0)),
        measured(Candidate.LayerPicture, workload(130.0, "page_down" -> 40.0)),
        measured(Candidate.SkiaRaster, workload(50.0, "page_down" -> 40.0))
      )
    )
    result should matchPattern { case Recommendation.Switch(Candidate.LayerImage, _) => }
  }

  it should "find no clear winner when the best of three is not clear of the second" in {
    val result = recommend(
      current = Candidate.SkiaRaster,
      List(
        measured(Candidate.LayerImage, workload(30.0)),
        measured(Candidate.LayerPicture, workload(33.0)),
        measured(Candidate.SkiaRaster, workload(50.0))
      )
    )
    result should matchPattern { case Recommendation.Keep(_, _) => }
  }

  it should "give evidence lines naming each candidate's typing median" in {
    val result = recommend(
      current = Candidate.SkiaRaster,
      List(
        measured(Candidate.LayerImage, workload(30.0, "page_down" -> 40.0)),
        measured(Candidate.SkiaRaster, workload(50.0, "page_down" -> 40.0))
      )
    )
    val text = result.evidence.mkString("\n")
    text should include("layer+image")
    text should include("skia-raster")
    text should include("30.0")
    text should include("50.0")
  }
