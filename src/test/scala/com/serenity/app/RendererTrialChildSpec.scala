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

  private def setup(
    window: Window,
    resolution: IO[Double] = IO.pure(1.5),
    backend: String = "skia",
    prepare: IO[Unit] = IO.unit,
    inApp: Option[Window] = None,
    workload: Plan = plan,
    device: Option[String] = None
  ): Setup =
    Setup(
      candidate = Candidate.LayerImage,
      plan = workload,
      window = window,
      clockResolutionMs = resolution,
      scale = 2.0,
      backendKey = backend,
      renderApi = IO.pure(Some("OPENGL")),
      timeout = 5.minutes,
      prepare = prepare,
      prepareTimeout = 3.seconds,
      inApp = inApp.map(IO.pure),
      device = IO.pure(device)
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
        _   <- RendererTrialChild.run(ready, quit, new PrintStream(new ByteArrayOutputStream()))
        log <- seen.get
      yield log.reverse
    }
    order.head shouldBe "prepare"
    order.count(_ == "prepare") shouldBe 1
  }

  it should "print a failed trial when the preparation fails, without running the workload" in {
    val text = output { out =>
      Deferred[IO, Unit].flatMap { quit =>
        val failing = setup(_ => IO.pure(Some(1.0)))
          .copy(prepare = IO.raiseError(new IllegalStateException("no notice to dismiss")))
        RendererTrialChild.run(failing, quit, out)
      }
    }
    lineOf(text).outcome should matchPattern {
      case Outcome.Failed(reason) if reason.contains("no notice to dismiss") =>
    }
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

  it should "fail the trial, saying so, when the start-up step never finishes, instead of waiting for the ceiling" in {
    val printed = runVirtual {
      for
        quit <- Deferred[IO, Unit]
        text <- IO {
          val bytes = new ByteArrayOutputStream()
          bytes -> new PrintStream(bytes, true, "UTF-8")
        }
        started <- IO.monotonic
        _       <- RendererTrialChild.run(setup(_ => IO.pure(Some(12.0)), prepare = IO.never), quit, text._2)
        ended   <- IO.monotonic
      yield (text._1.toString("UTF-8"), ended - started)
    }
    lineOf(printed._1).outcome should matchPattern {
      case Outcome.Failed(reason) if reason.contains("start-up") && reason.contains("3 s") =>
    }
    printed._2 shouldBe 3.seconds
  }

  private def printed(setup: Setup): String =
    runVirtual {
      for
        quit <- Deferred[IO, Unit]
        text <- IO {
          val bytes = new ByteArrayOutputStream()
          bytes -> new PrintStream(bytes, true, "UTF-8")
        }
        _ <- RendererTrialChild.run(setup, quit, text._2)
      yield text._1.toString("UTF-8")
    }

  it should "measure again by the in-app clock, without the resize, when the screen cannot be read, and say so" in {
    val steps         = new java.util.concurrent.ConcurrentLinkedQueue[Step]()
    val inApp: Window = step => IO(steps.add(step)).as(Some(9.0))
    val report = lineOf(printed(setup(_ => IO.pure(None), inApp = Some(inApp), workload = plan.copy(typedChars = 6))))
    report.outcome match
      case Outcome.Measured(workload) =>
        workload.clock shouldBe RendererChoice.Clock.InApp
        workload.typingP50Ms shouldBe 9.0
        workload.clockResolutionMs shouldBe 0.0
      case other => fail(s"expected an in-app measurement, got $other")
    steps.toArray.toList.collect { case Step.FullRedraw(how) => how } should
      contain only RendererWorkload.Redraw.ThemeToggle
  }

  it should "not use the in-app clock when the screen was read" in {
    val inApp: Window = _ => IO.raiseError(new IllegalStateException("the in-app clock must not run"))
    lineOf(
      printed(setup(_ => IO.pure(Some(12.0)), inApp = Some(inApp), workload = plan.copy(typedChars = 6)))
    ).outcome match
      case Outcome.Measured(workload) => workload.clock shouldBe RendererChoice.Clock.Pixel
      case other                      => fail(s"expected a pixel measurement, got $other")
  }

  it should "fail, saying neither clock saw a keystroke, when the in-app clock cannot see one either" in {
    lineOf(
      printed(setup(_ => IO.pure(None), inApp = Some(_ => IO.pure(None)), workload = plan.copy(typedChars = 6)))
    ).outcome match
      case Outcome.Failed(reason) => reason should (include("screen") and include("in-app"))
      case other                  => fail(s"expected a failure, got $other")
  }

  it should "keep the screen's failure when there is no in-app clock to fall back to" in {
    lineOf(printed(setup(_ => IO.pure(None), workload = plan.copy(typedChars = 6)))).outcome should matchPattern {
      case Outcome.Failed(reason) if reason.contains("cannot be read from here") =>
    }
  }

  it should "print the adapter the window ran on" in {
    lineOf(printed(setup(_ => IO.pure(Some(12.0)), device = Some("llvmpipe")))).device shouldBe Some("llvmpipe")
    lineOf(printed(setup(_ => IO.pure(Some(12.0))))).device shouldBe None
  }
