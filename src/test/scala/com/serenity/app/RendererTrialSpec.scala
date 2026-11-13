package com.serenity.app

import com.serenity.app.RendererChoice.{Candidate, Outcome, Workload}
import com.serenity.app.RendererTrial.Report
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RendererTrialSpec extends AnyFlatSpec with Matchers:

  private val measured = Outcome.Measured(Workload(31.5, 48.0, 2.0, 1.2, Map("page-down" -> 12.25, "full-redraw" -> 40.0)))

  private def report(
    requested: Candidate = Candidate.LayerImage,
    backend: RenderBackend = RenderBackend.Skia,
    renderApi: Option[String] = Some("OPENGL"),
    outcome: Outcome = measured
  ) = Report(requested, backend, renderApi, outcome)

  "RendererTrial.line" should "be one line that starts with the marker the coordinator looks for" in {
    val line = RendererTrial.line(report())
    line should startWith("RENDERER_TRIAL ")
    line should not include "\n"
  }

  it should "read back exactly as it was written, for every mode and for failed runs" in {
    val reports = List(
      report(),
      report(Candidate.LayerPicture),
      report(Candidate.SkiaRaster, RenderBackend.SkiaRaster, None),
      report(outcome = Outcome.Failed("every typing sample timed out (150)"))
    )
    reports.foreach(r => RendererTrial.read(RendererTrial.line(r)) shouldBe Some(r))
  }

  "RendererTrial.read" should "find nothing in a line that is not a trial line or cannot be decoded" in
    List(
      "",
      "RENDERER_TRIAL",
      "RENDERER_TRIAL not json",
      "RENDERER_TRIAL {}",
      "RENDERER_TRIAL {\"requested\":\"layer+nonsense\"}",
      "something else {\"requested\":\"layer+image\"}"
    ).foreach(text => withClue(text)(RendererTrial.read(text) shouldBe None))

  "RendererTrial.outcomeOf" should "take the measured outcome of the one trial line in a child's output" in {
    val output = s"""starting the window
                    |some log line
                    |${RendererTrial.line(report())}
                    |quitting""".stripMargin
    RendererTrial.outcomeOf(output, Candidate.LayerImage) shouldBe measured
  }

  it should "fail a child that printed no trial line" in {
    RendererTrial.outcomeOf("window opened\nquit\n", Candidate.LayerImage) match
      case Outcome.Failed(reason) => reason should include("RENDERER_TRIAL")
      case other                  => fail(s"expected a failure, got $other")
  }

  it should "fail a child whose trial line is garbage, and say what it printed" in {
    RendererTrial.outcomeOf("RENDERER_TRIAL {oops\n", Candidate.LayerImage) match
      case Outcome.Failed(reason) => reason should (include("unreadable") and include("{oops"))
      case other                  => fail(s"expected a failure, got $other")
  }

  it should "fail a child that printed more than one trial line rather than pick one" in {
    val twice = RendererTrial.line(report()) + "\n" + RendererTrial.line(report())
    RendererTrial.outcomeOf(twice, Candidate.LayerImage) match
      case Outcome.Failed(reason) => reason should include("more than one")
      case other                  => fail(s"expected a failure, got $other")
  }

  it should "fail a child that measured a different mode from the one it was asked for" in {
    RendererTrial.outcomeOf(
      RendererTrial.line(report(Candidate.SkiaRaster, RenderBackend.SkiaRaster)),
      Candidate.LayerImage
    ) match
      case Outcome.Failed(reason) => reason should (include("skia-raster") and include("layer+image"))
      case other                  => fail(s"expected a failure, got $other")
  }

  it should "report a mode that fell back to the terminal as failed, not as a measurement of that mode" in
    List(Candidate.LayerImage, Candidate.LayerPicture, Candidate.SkiaRaster).foreach { candidate =>
      withClue(candidate.label) {
        val fellBack = report(candidate, RenderBackend.Terminal, None)
        RendererTrial.outcomeOf(RendererTrial.line(fellBack), candidate) match
          case Outcome.Failed(reason) => reason should (include("terminal") and include(candidate.label))
          case other                  => fail(s"expected a failure, got $other")
      }
    }

  it should "report a layer mode that ended on the wrong backend as failed, and keep a failed run's own reason" in {
    RendererTrial.outcomeOf(
      RendererTrial.line(report(Candidate.LayerImage, RenderBackend.SkiaRaster)),
      Candidate.LayerImage
    ) match
      case Outcome.Failed(reason) => reason should include("skia-raster")
      case other                  => fail(s"expected a failure, got $other")
    val failed = report(outcome = Outcome.Failed("every page-down sample timed out (20)"))
    RendererTrial.outcomeOf(RendererTrial.line(failed), Candidate.LayerImage) shouldBe
      Outcome.Failed("every page-down sample timed out (20)")
  }
