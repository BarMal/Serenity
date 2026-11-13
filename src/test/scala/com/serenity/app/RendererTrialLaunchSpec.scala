package com.serenity.app

import java.awt.GraphicsEnvironment
import java.io.{ByteArrayOutputStream, PrintStream}

import scala.concurrent.duration.*

import cats.effect.{Deferred, IO}
import cats.effect.unsafe.implicits.global
import com.serenity.app.RendererChoice.{Candidate, Outcome}
import com.serenity.app.RendererWorkload.Plan
import com.serenity.config.WindowChromeMode
import com.serenity.diagnostics.FrameTimings
import com.serenity.testkit.DisplayExclusive
import com.serenity.ui.terminal.{SwingWindow, SwingWindowHost}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The trial's hook at the first frame, against a real window on a display (Xvfb in the Test job). */
class RendererTrialLaunchSpec extends AnyFlatSpec with Matchers:

  private val tiny = Plan(typedChars = 2, pageDowns = 1, fullRedraws = 1, typingPace = 10.millis, settle = 10.millis)

  private def options(args: String*): LaunchOptions =
    LaunchOptions.parse(args.toList).fold(help => fail(help.toString), identity)

  private def withHost[A](use: com.serenity.ui.host.WindowHost => IO[A]): A =
    assume(!GraphicsEnvironment.isHeadless, "A Swing window needs a display")
    DisplayExclusive:
      SwingWindowHost
        .resource(
          SwingWindow.DefaultMetrics,
          SwingWindow.DefaultMetrics,
          WindowChromeMode.Auto,
          None,
          FrameTimings(),
          "launch spec"
        )
        .use(use)
        .unsafeRunSync()

  "RendererTrialLaunch.firstFrame" should "do nothing for a launch that is not a trial" in {
    val quit = withHost { host =>
      for
        quit <- Deferred[IO, Unit]
        _    <- RendererTrialLaunch.firstFrame(options("notes.md"), host, "skia", quit, new PrintStream(new ByteArrayOutputStream()))
        _    <- IO.sleep(300.millis)
        done <- quit.tryGet
      yield done
    }
    quit shouldBe None
  }

  it should "run a trial in the background, print one trial line and quit, even when nothing on the screen changes" in {
    val (text, done) = withHost { host =>
      val bytes = new ByteArrayOutputStream()
      val out   = new PrintStream(bytes, true, "UTF-8")
      for
        quit <- Deferred[IO, Unit]
        _ <- RendererTrialLaunch.firstFrame(
          options("--renderer-trial=skia-raster", "--safe-mode"),
          host,
          "skia-raster",
          quit,
          out,
          plan = tiny,
          stepTimeout = 150.millis,
          warmUp = 10.millis
        )
        _    <- quit.get.timeout(60.seconds)
        done <- quit.tryGet
      yield (bytes.toString("UTF-8"), done)
    }
    done shouldBe Some(())
    text.linesIterator.filter(_.startsWith(RendererTrial.Marker)).toList match
      case line :: Nil =>
        val report = RendererTrial.read(line).getOrElse(fail(s"unreadable: $line"))
        report.requested shouldBe Candidate.SkiaRaster
        report.backend shouldBe RenderBackend.SkiaRaster
        report.outcome should matchPattern { case Outcome.Failed(reason) if reason.contains("typing") => }
      case other => fail(s"expected exactly one trial line, got $other")
  }
