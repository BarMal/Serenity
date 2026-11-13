package com.serenity.app

import scala.concurrent.duration.*

import cats.effect.{IO, Ref}
import cats.syntax.all.*
import com.serenity.app.PixelClock.{Region, Screen}
import com.serenity.app.RendererTrialWindow.Driver
import com.serenity.app.RendererWorkload.{Redraw, Step}
import com.serenity.testkit.VirtualTime.runVirtual
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RendererTrialWindowSpec extends AnyFlatSpec with Matchers:

  private val typing   = Region(0, 0, 40, 3)
  private val scroll   = Region(0, 0, 40, 30)
  private val resized  = Region(100, 50, 8, 8)
  private val themed   = Region(1, 1, 4, 4)
  private val blank    = Vector.fill(4)(0)
  private val inked    = Vector.fill(4)(1)
  private val timeout  = 500.millis
  private val grabCost = 2.millis

  /** What the driver was asked to do, and the regions the screen was asked about, in order. */
  final private class Recorder:
    val log: Ref[IO, Vector[String]] = Ref.unsafe[IO, Vector[String]](Vector.empty)
    val shown: Ref[IO, Vector[Int]]  = Ref.unsafe[IO, Vector[Int]](blank)

    def screen: Screen = region => log.update(_ :+ s"grab $region") *> IO.sleep(grabCost) *> shown.get

    /** A driver whose every action puts ink on the screen after `latency`, or never when it is None. */
    def driver(latency: Option[FiniteDuration]): Driver =
      def act(what: String): IO[Unit] =
        log.update(_ :+ what) *> latency.traverse_(after => (IO.sleep(after) *> shown.set(inked)).start)
      new Driver:
        def typeCharacter(char: Char): IO[Unit] = act(s"type $char")
        def pressPageDown: IO[Unit]             = act("page-down")
        def toggleTheme: IO[Unit]               = act("toggle-theme")
        def dismissNotices: IO[Unit]            = act("dismiss")
        def resize: IO[Unit]                    = act("resize")
        def watchedFor(step: Step): IO[Region] = IO.pure(step match
          case _: Step.Type                        => typing
          case Step.PageDown                       => scroll
          case Step.FullRedraw(Redraw.Resize)      => resized
          case Step.FullRedraw(Redraw.ThemeToggle) => themed)

  private def perform(step: Step, latency: Option[FiniteDuration]): (Option[Double], Vector[String]) =
    runVirtual {
      val recorder = Recorder()
      val window   = RendererTrialWindow(recorder.driver(latency), recorder.screen, timeout)
      for
        answer <- window.perform(step)
        log    <- recorder.log.get
      yield (answer, log)
    }

  "RendererTrialWindow" should "type the character and watch where it lands, answering the time the ink took" in {
    val (answer, log) = perform(Step.Type('q'), Some(7.millis))
    answer shouldBe Some(8.0)
    log.filterNot(_.startsWith("grab")) shouldBe Vector("type q")
    log.filter(_.startsWith("grab")).distinct shouldBe Vector(s"grab $typing")
  }

  it should "press page-down and watch the scrolled area" in {
    val (answer, log) = perform(Step.PageDown, Some(3.millis))
    answer.isDefined shouldBe true
    log should contain("page-down")
    log.filter(_.startsWith("grab")).distinct shouldBe Vector(s"grab $scroll")
  }

  it should "resize the window and watch the place the edge moves through" in {
    val (answer, log) = perform(Step.FullRedraw(Redraw.Resize), Some(3.millis))
    answer.isDefined shouldBe true
    log should contain("resize")
    log.filter(_.startsWith("grab")).distinct shouldBe Vector(s"grab $resized")
  }

  it should "toggle the theme and watch a spot the new colours reach" in {
    val (answer, log) = perform(Step.FullRedraw(Redraw.ThemeToggle), Some(3.millis))
    answer.isDefined shouldBe true
    log should contain("toggle-theme")
    log.filter(_.startsWith("grab")).distinct shouldBe Vector(s"grab $themed")
  }

  it should "answer none, not a number, when nothing shows within the timeout" in {
    perform(Step.Type('q'), latency = None)._1 shouldBe None
  }

  it should "fail when the screen cannot be read, rather than count it as a slow step" in {
    val outcome = runVirtual {
      val recorder       = Recorder()
      val broken: Screen = _ => IO.raiseError(new IllegalStateException("no display"))
      RendererTrialWindow(recorder.driver(Some(1.milli)), broken, timeout).perform(Step.PageDown).attempt
    }
    outcome.left.map(_.getMessage) shouldBe Left("no display")
  }
