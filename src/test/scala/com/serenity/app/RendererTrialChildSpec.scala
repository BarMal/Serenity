package com.serenity.app

import java.io.{ByteArrayOutputStream, PrintStream}

import scala.concurrent.duration.*

import cats.effect.{Deferred, IO}
import com.serenity.app.RendererChoice.{Candidate, Outcome, Workload}
import com.serenity.app.RendererTrialChild.Setup
import com.serenity.app.RendererWorkload.{Plan, Step, Window}
import com.serenity.testkit.VirtualTime.runVirtual
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RendererTrialChildSpec extends AnyFlatSpec with Matchers:

  private val plan = Plan(typedChars = 4, pageDowns = 2, fullRedraws = 2, typingPace = 10.millis, settle = 10.millis)

  private def options(args: String*): LaunchOptions =
    LaunchOptions.parse(args.toList).fold(help => fail(help.toString), identity)

  "RendererTrialChild.refusal" should "refuse a trial outside safe mode, because it types into the window" in {
    RendererTrialChild.refusal(options("--renderer-trial=layer+image")) match
      case Some(text) => text should (include("--renderer-trial") and include("--safe-mode"))
      case None       => fail("a trial outside safe mode must be refused")
  }

  it should "allow a trial in safe mode, and any launch that is not a trial" in {
    RendererTrialChild.refusal(options("--renderer-trial=skia-raster", "--safe-mode")) shouldBe None
    RendererTrialChild.refusal(options("notes.md")) shouldBe None
  }

  private def output(run: PrintStream => IO[Unit]): String =
    val bytes = new ByteArrayOutputStream()
    val out   = new PrintStream(bytes, true, "UTF-8")
    runVirtual(run(out))
    bytes.toString("UTF-8")

  private def setup(window: Window, resolution: IO[Double] = IO.pure(1.5), backend: String = "skia"): Setup =
    Setup(
      candidate = Candidate.LayerImage,
      plan = plan,
      window = window,
      clockResolutionMs = resolution,
      scale = 2.0,
      backendKey = backend,
      renderApi = IO.pure(Some("OPENGL")),
      timeout = 5.minutes
    )

  private def lineOf(text: String): RendererTrial.Report =
    text.linesIterator.toList.filter(_.startsWith(RendererTrial.Marker)) match
      case line :: Nil => RendererTrial.read(line).getOrElse(fail(s"unreadable: $line"))
      case other       => fail(s"expected exactly one trial line, got $other")

  "RendererTrialChild.run" should "run the workload, print one trial line with what it measured, and quit" in {
    val printed = runVirtual {
      for
        quit <- Deferred[IO, Unit]
        text <- IO {
          val bytes = new ByteArrayOutputStream()
          bytes -> new PrintStream(bytes, true, "UTF-8")
        }
        _    <- RendererTrialChild.run(setup(_ => IO.pure(Some(12.0))), quit, text._2)
        done <- quit.tryGet
      yield (text._1.toString("UTF-8"), done)
    }
    val report = lineOf(printed._1)
    report.requested shouldBe Candidate.LayerImage
    report.backend shouldBe RenderBackend.Skia
    report.renderApi shouldBe Some("OPENGL")
    report.outcome shouldBe Outcome.Measured(
      Workload(
        typingP50Ms = 12.0,
        typingP95Ms = 12.0,
        scale = 2.0,
        clockResolutionMs = 1.5,
        fullWindowMs = Map("page-down" -> 12.0, "full-redraw" -> 12.0)
      )
    )
    printed._2 shouldBe Some(())
  }

  it should "prepare the window before it runs the workload" in {
    val order = runVirtual {
      for
        seen <- cats.effect.Ref.of[IO, List[String]](Nil)
        quit <- Deferred[IO, Unit]
        ready = setup(_ => seen.update("step" :: _) *> IO.pure(Some(1.0))).copy(prepare = seen.update("prepare" :: _))
        _    <- RendererTrialChild.run(ready, quit, new PrintStream(new ByteArrayOutputStream()))
        log  <- seen.get
      yield log.reverse
    }
    order.head shouldBe "prepare"
    order.count(_ == "prepare") shouldBe 1
  }

  it should "print a failed trial when the preparation fails, without running the workload" in {
    val text = output { out =>
      Deferred[IO, Unit].flatMap { quit =>
        val failing = setup(_ => IO.pure(Some(1.0))).copy(prepare = IO.raiseError(new IllegalStateException("no notice to dismiss")))
        RendererTrialChild.run(failing, quit, out)
      }
    }
    lineOf(text).outcome should matchPattern { case Outcome.Failed(reason) if reason.contains("no notice to dismiss") => }
  }

  it should "print a failed trial with its reason when every sample of a kind timed out" in {
    val text =
      output(out => Deferred[IO, Unit].flatMap(quit => RendererTrialChild.run(setup(_ => IO.pure(None)), quit, out)))
    lineOf(text).outcome should matchPattern { case Outcome.Failed(reason) if reason.contains("typing") => }
  }

  it should "print a failed trial, not nothing, when the window or the clock throws, and still quit" in {
    val broken: Window = (_: Step) => IO.raiseError(new IllegalStateException("no display"))
    val result = runVirtual {
      for
        quit <- Deferred[IO, Unit]
        bytes = new ByteArrayOutputStream()
        out   = new PrintStream(bytes, true, "UTF-8")
        _    <- RendererTrialChild.run(setup(broken), quit, out)
        done <- quit.tryGet
      yield (bytes.toString("UTF-8"), done)
    }
    lineOf(result._1).outcome should matchPattern { case Outcome.Failed(reason) if reason.contains("no display") => }
    result._2 shouldBe Some(())
  }

  it should "fail the trial when it runs past its timeout, so a hung window cannot hang the coordinator" in {
    val hung: Window = (_: Step) => IO.never
    val result = runVirtual {
      for
        quit <- Deferred[IO, Unit]
        bytes = new ByteArrayOutputStream()
        out   = new PrintStream(bytes, true, "UTF-8")
        _    <- RendererTrialChild.run(setup(hung).copy(timeout = 1.minute), quit, out)
        done <- quit.tryGet
      yield (bytes.toString("UTF-8"), done)
    }
    lineOf(result._1).outcome should matchPattern { case Outcome.Failed(reason) if reason.contains("timed out") => }
    result._2 shouldBe Some(())
  }

  it should "report a backend it does not know as a failed trial rather than guess" in {
    val text = output { out =>
      Deferred[IO, Unit].flatMap(quit =>
        RendererTrialChild.run(setup(_ => IO.pure(Some(1.0)), backend = "java2d"), quit, out)
      )
    }
    lineOf(text).outcome should matchPattern { case Outcome.Failed(reason) if reason.contains("java2d") => }
  }
