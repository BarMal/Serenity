package com.serenity.app

import java.nio.file.Path
import java.time.Instant

import scala.collection.mutable.ListBuffer

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.TestTemp
import com.serenity.app.RendererBenchmarkResult.{Key, Stored}
import com.serenity.app.RendererChoice.{Candidate, Outcome, Recommendation, Workload}
import com.serenity.app.RendererCoordinator.{Environment, Spawner}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RendererCoordinatorSpec extends AnyFlatSpec with Matchers:

  private val machine = Key("Mesa Intel", "Linux 6.18", "x11", "3840x2160@2.0", "25.0.5+8", "0.153.0", "ac")
  private val instant = Instant.parse("2026-11-13T18:00:00Z")

  private def backendOf(candidate: Candidate): RenderBackend =
    if candidate == Candidate.SkiaRaster then RenderBackend.SkiaRaster else RenderBackend.Skia

  private def measured(typing: Double, p95: Double): Outcome =
    Outcome.Measured(Workload(typing, p95, 2.0, 1.0, Map("page-down" -> 40.0, "full-redraw" -> 60.0)))

  private def printed(candidate: Candidate, outcome: Outcome): String =
    RendererTrial.line(RendererTrial.Report(candidate, backendOf(candidate), Some("OPENGL"), outcome))

  private class Script(answers: Map[Candidate, List[IO[String]]]):
    val started: ListBuffer[Candidate] = ListBuffer.empty
    private val remaining              = answers.view.mapValues(_.iterator).toMap
    val spawner: Spawner = candidate =>
      IO(started += candidate) *> remaining(candidate).nextOption().getOrElse(IO.pure(""))

  private def environment(path: Path): Environment =
    Environment(key = IO.pure(machine), now = IO.pure(instant), resultPath = path)

  private def optimize(
    script: Script,
    thorough: Boolean = false,
    current: Candidate = Candidate.SkiaRaster,
    progress: ListBuffer[(Int, Int, Candidate)] = ListBuffer.empty
  ): (RendererCoordinator.Finished, Path) =
    val path = TestTemp.directory("renderer-coordinator").resolve("render-benchmark.json")
    val finished = RendererCoordinator
      .optimize(current, thorough, script.spawner, environment(path), (n, of, c) => IO(progress += ((n, of, c))).void)
      .unsafeRunSync()
    finished -> path

  private def twoModesAnswers: Map[Candidate, List[IO[String]]] = Map(
    Candidate.LayerImage -> List(
      IO.pure(printed(Candidate.LayerImage, measured(30.0, 46.0))),
      IO.pure(printed(Candidate.LayerImage, measured(32.0, 50.0)))
    ),
    Candidate.SkiaRaster -> List(
      IO.pure(printed(Candidate.SkiaRaster, measured(50.0, 80.0))),
      IO.pure(printed(Candidate.SkiaRaster, measured(52.0, 84.0)))
    )
  )

  "RendererCoordinator.optimize" should "start the trials in the run order, one at a time, and say which one it is on" in {
    val progress = ListBuffer.empty[(Int, Int, Candidate)]
    val script   = Script(twoModesAnswers)
    optimize(script, progress = progress)
    script.started.toList shouldBe RendererRunOrder.order(thorough = false)
    progress.toList shouldBe RendererRunOrder.order(thorough = false).zipWithIndex.map((c, i) => (i + 1, 4, c))
  }

  it should "gather each mode's runs in the order they ran and recommend from them" in {
    val (finished, _) = optimize(Script(twoModesAnswers))
    finished.runs.map(_.candidate) shouldBe List(Candidate.LayerImage, Candidate.SkiaRaster)
    finished.runs.map(_.runs.size) shouldBe List(2, 2)
    finished.recommendation should matchPattern { case Recommendation.Switch(Candidate.LayerImage, _) => }
  }

  it should "add the 16.7 ms target report to the evidence without changing the recommendation" in {
    val (finished, _) = optimize(Script(twoModesAnswers))
    finished.recommendation.evidence.last should include("No renderer mode reaches")
  }

  it should "run layer+picture too when thorough" in {
    val script = Script(
      Map(
        Candidate.LayerImage   -> List.fill(2)(IO.pure(printed(Candidate.LayerImage, measured(30.0, 46.0)))),
        Candidate.SkiaRaster   -> List.fill(2)(IO.pure(printed(Candidate.SkiaRaster, measured(50.0, 80.0)))),
        Candidate.LayerPicture -> List.fill(2)(IO.pure(printed(Candidate.LayerPicture, measured(120.0, 180.0))))
      )
    )
    val (finished, _) = optimize(script, thorough = true)
    script.started.size shouldBe 6
    finished.runs.map(_.candidate) shouldBe List(Candidate.LayerImage, Candidate.SkiaRaster, Candidate.LayerPicture)
  }

  it should "count a child that printed nothing as a failed run, and never recommend that mode" in {
    val script = Script(
      Map(
        Candidate.LayerImage -> List(IO.pure(""), IO.pure(printed(Candidate.LayerImage, measured(30.0, 46.0)))),
        Candidate.SkiaRaster -> List.fill(2)(IO.pure(printed(Candidate.SkiaRaster, measured(50.0, 80.0))))
      )
    )
    val (finished, _) = optimize(script)
    finished.runs.head.runs.head should matchPattern {
      case Outcome.Failed(reason) if reason.contains("no RENDERER_TRIAL") =>
    }
    finished.recommendation should not matchPattern { case Recommendation.Switch(Candidate.LayerImage, _) => }
  }

  it should "count a trial that could not be started as failed and carry on with the rest" in {
    val script = Script(
      Map(
        Candidate.LayerImage -> List(
          IO.raiseError(new RuntimeException("no java")),
          IO.pure(printed(Candidate.LayerImage, measured(30.0, 46.0)))
        ),
        Candidate.SkiaRaster -> List.fill(2)(IO.pure(printed(Candidate.SkiaRaster, measured(50.0, 80.0))))
      )
    )
    val (finished, _) = optimize(script)
    script.started.size shouldBe 4
    finished.runs.head.runs.head should matchPattern {
      case Outcome.Failed(reason) if reason.contains("could not be run") && reason.contains("no java") =>
    }
  }

  it should "keep the result beside the config, under the key it was measured with" in {
    val (finished, path) = optimize(Script(twoModesAnswers))
    RendererBenchmarkResult.load(path).unsafeRunSync() shouldBe Some(Stored(machine, instant, finished.runs))
    finished.saveError shouldBe None
  }

  it should "still report what it measured when the result cannot be saved, and say so" in {
    val directory = TestTemp.directory("renderer-coordinator-blocked")
    val blocker   = directory.resolve("blocker")
    java.nio.file.Files.writeString(blocker, "a file, not a directory")
    val finished = RendererCoordinator
      .optimize(
        Candidate.SkiaRaster,
        false,
        Script(twoModesAnswers).spawner,
        environment(blocker.resolve("render-benchmark.json")),
        (_, _, _) => IO.unit
      )
      .unsafeRunSync()
    finished.recommendation should matchPattern { case Recommendation.Switch(Candidate.LayerImage, _) => }
    finished.saveError.exists(_.nonEmpty) shouldBe true
  }

  "RendererCoordinator.report" should "say what to set for a recommended switch, and how to have it applied" in {
    val (finished, _) = optimize(Script(twoModesAnswers))
    val lines         = RendererCoordinator.report(finished)
    lines.head should include("layer+image")
    lines.mkString("\n") should (include("ui.render.backend = skia, ui.render.skia_frames = image") and include(
      "--apply"
    ))
  }

  it should "say it is keeping the current mode, and offer nothing to apply, when there is no clear winner" in {
    val close = Script(
      Map(
        Candidate.LayerImage -> List.fill(2)(IO.pure(printed(Candidate.LayerImage, measured(48.0, 70.0)))),
        Candidate.SkiaRaster -> List.fill(2)(IO.pure(printed(Candidate.SkiaRaster, measured(50.0, 72.0))))
      )
    )
    val (finished, _) = optimize(close)
    val text          = RendererCoordinator.report(finished).mkString("\n")
    finished.recommendation should matchPattern { case Recommendation.Keep(_, _) => }
    text should not include "--apply"
  }

  it should "include a note on the target and the file it saved, or why it could not" in {
    val (finished, _) = optimize(Script(twoModesAnswers))
    RendererCoordinator.report(finished).mkString("\n") should include("16.7 ms")
    RendererCoordinator.report(finished.copy(saveError = Some("disk full"))).mkString("\n") should include("disk full")
  }
