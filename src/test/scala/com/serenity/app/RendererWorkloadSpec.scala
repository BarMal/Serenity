package com.serenity.app

import scala.concurrent.duration.*

import cats.effect.{IO, Ref}
import com.serenity.app.RendererChoice.Outcome
import com.serenity.app.RendererWorkload.{Plan, Redraw, Result, Samples, Step}
import com.serenity.testkit.VirtualTime.runVirtual
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RendererWorkloadSpec extends AnyFlatSpec with Matchers:

  private val plan = Plan(typedChars = 6, pageDowns = 3, fullRedraws = 4, typingPace = 80.millis, settle = 400.millis)

  private def typed(steps: Vector[Step]): String = steps.collect { case Step.Type(char) => char }.mkString

  "Plan.standard" should "take 150 typed characters, 20 page-downs and 10 full redraws" in {
    val steps = RendererWorkload.script(Plan.standard)
    steps.count(_.isInstanceOf[Step.Type]) shouldBe 150
    steps.count(_ == Step.PageDown) shouldBe 20
    steps.count(_.isInstanceOf[Step.FullRedraw]) shouldBe 10
  }

  "RendererWorkload.script" should "type first, then page down, then redraw the whole window" in {
    val steps = RendererWorkload.script(plan)
    steps.map {
      case _: Step.Type       => "type"
      case Step.PageDown      => "page-down"
      case _: Step.FullRedraw => "redraw"
    } shouldBe Vector.fill(6)("type") ++ Vector.fill(3)("page-down") ++ Vector.fill(4)("redraw")
  }

  it should "type prose, not one repeated key" in {
    val text = typed(RendererWorkload.script(Plan.standard))
    text.distinct.length should be > 8
    text.forall(_.isLetter) shouldBe true
  }

  it should "type only characters that leave ink, so every keystroke changes pixels the clock can see" in {
    typed(RendererWorkload.script(Plan.standard)).exists(_.isWhitespace) shouldBe false
  }

  it should "force the full redraws by alternating a resize and a theme toggle, starting with the resize" in {
    RendererWorkload.script(plan).collect { case Step.FullRedraw(how) => how } shouldBe
      Vector(Redraw.Resize, Redraw.ThemeToggle, Redraw.Resize, Redraw.ThemeToggle)
  }

  it should "be empty when nothing is asked for" in {
    RendererWorkload.script(Plan(0, 0, 0, 80.millis, 400.millis)) shouldBe empty
  }

  final private class RecordingWindow(answer: Step => Option[Double] = _ => Some(1.0)):
    private val seen = Ref.unsafe[IO, Vector[(Step, FiniteDuration)]](Vector.empty)

    val window: RendererWorkload.Window = step =>
      IO.monotonic.flatMap(now => seen.update(_ :+ (step -> now))).as(answer(step))

    def performed: IO[Vector[(Step, FiniteDuration)]] = seen.get

  "RendererWorkload.run" should "perform every step in script order" in {
    val recording = new RecordingWindow()
    val performed = runVirtual(RendererWorkload.run(plan, recording.window) *> recording.performed)
    performed.map(_._1) shouldBe RendererWorkload.script(plan)
  }

  it should "hold the typing pace between keystrokes and the settle time after every other step" in {
    val recording = new RecordingWindow()
    val performed = runVirtual(RendererWorkload.run(plan, recording.window) *> recording.performed)
    val gaps = performed.sliding(2).collect { case Vector((before, at), (_, next)) => before -> (next - at) }.toVector
    gaps.collect { case (_: Step.Type, gap) => gap }.distinct shouldBe Vector(80.millis)
    gaps.collect { case (step, gap) if !step.isInstanceOf[Step.Type] => gap }.distinct shouldBe Vector(400.millis)
  }

  it should "start the first step at once, so the pace is the only wait" in {
    val recording = new RecordingWindow()
    val performed = runVirtual(RendererWorkload.run(plan, recording.window) *> recording.performed)
    performed.headOption.map(_._2) shouldBe Some(Duration.Zero)
  }

  it should "keep the latencies of the steps that answered, per kind of step" in {
    val latencies = Iterator.from(1).map(_.toDouble)
    val recording = new RecordingWindow(_ => Some(latencies.next()))
    val result    = runVirtual(RendererWorkload.run(plan, recording.window))
    result.typing shouldBe Samples(Vector(1.0, 2.0, 3.0, 4.0, 5.0, 6.0), timedOut = 0)
    result.pageDown shouldBe Samples(Vector(7.0, 8.0, 9.0), timedOut = 0)
    result.fullRedraw shouldBe Samples(Vector(10.0, 11.0, 12.0, 13.0), timedOut = 0)
  }

  it should "count a step that timed out and keep it out of the latencies" in {
    val calls     = Iterator.from(0)
    val recording = new RecordingWindow(_ => if calls.next() % 2 == 0 then None else Some(10.0))
    val result    = runVirtual(RendererWorkload.run(plan, recording.window))
    result.typing shouldBe Samples(Vector(10.0, 10.0, 10.0), timedOut = 3)
    result.pageDown.timedOut shouldBe 2
    result.fullRedraw.timedOut shouldBe 2
  }

  it should "carry on after a timeout rather than stop the run" in {
    val calls     = Iterator.from(0)
    val recording = new RecordingWindow(_ => if calls.next() % 2 == 0 then None else Some(10.0))
    val performed = runVirtual(RendererWorkload.run(plan, recording.window) *> recording.performed)
    performed should have size 13
  }

  it should "stop after the first five typed characters when none of them showed, because the screen cannot be read" in {
    val recording = new RecordingWindow(_ => None)
    val (performed, result) =
      runVirtual(RendererWorkload.run(plan, recording.window).flatMap(result => recording.performed.map(_ -> result)))
    performed.map(_._1) shouldBe RendererWorkload.script(plan).take(5)
    result.unreadableScreen shouldBe true
    result.typing shouldBe Samples(Vector.empty, timedOut = 5)
    result.pageDown shouldBe Samples(Vector.empty, timedOut = 0)
  }

  it should "not wait out the pace after the keystroke that ends the run" in {
    val recording = new RecordingWindow(_ => None)
    val performed = runVirtual(RendererWorkload.run(plan, recording.window) *> recording.performed)
    performed.map(_._2) shouldBe Vector(0, 80, 160, 240, 320).map(_.millis)
  }

  it should "carry on when one of the first five typed characters showed" in {
    val recording = new RecordingWindow(step => if step == Step.Type('t') then None else Some(3.0))
    val result    = runVirtual(RendererWorkload.run(plan, recording.window))
    result.unreadableScreen shouldBe false
    result.typing.timedOut shouldBe 1
    result.fullRedraw.latenciesMs should have size 4
  }

  it should "not call the screen unreadable when fewer characters than that were typed" in {
    val few       = Plan(typedChars = 3, pageDowns = 1, fullRedraws = 1, typingPace = 80.millis, settle = 400.millis)
    val recording = new RecordingWindow(_ => None)
    val result    = runVirtual(RendererWorkload.run(few, recording.window))
    result.unreadableScreen shouldBe false
    result.typing.timedOut shouldBe 3
  }

  it should "say how far it has got every thirty steps, and how many timed out" in {
    val many      = Plan(typedChars = 60, pageDowns = 3, fullRedraws = 1, typingPace = 80.millis, settle = 400.millis)
    val calls     = Iterator.from(1)
    val recording = new RecordingWindow(_ => if calls.next() == 7 then None else Some(2.0))
    val lines = runVirtual {
      Ref.of[IO, Vector[String]](Vector.empty).flatMap { seen =>
        RendererWorkload.run(many, recording.window, line => seen.update(_ :+ line)) *> seen.get
      }
    }
    lines shouldBe Vector("[TRIAL] step 30 of 64, 1 timed out", "[TRIAL] step 60 of 64, 1 timed out")
  }

  "RendererWorkload.outcome" should "report each kind of step by its median, with the rows named as the choice rule reads them" in {
    val result = Result(
      typing = Samples(Vector(30.0, 10.0, 20.0), timedOut = 0),
      pageDown = Samples(Vector(5.0, 7.0, 9.0, 100.0), timedOut = 1),
      fullRedraw = Samples(Vector(40.0), timedOut = 0)
    )
    RendererWorkload.outcome(result, scale = 2.0, clockResolutionMs = 1.5) shouldBe Outcome.Measured(
      RendererChoice.Workload(
        typingP50Ms = 20.0,
        typingP95Ms = 30.0,
        scale = 2.0,
        clockResolutionMs = 1.5,
        fullWindowMs = Map("page-down" -> 8.0, "full-redraw" -> 40.0)
      )
    )
  }

  it should "report the typing p95 of the samples, not of the median, and the scale the window ran at" in {
    val typing = Samples((1 to 100).map(_.toDouble).toVector, timedOut = 0)
    val result = Result(typing, Samples(Vector(5.0), 0), Samples(Vector(40.0), 0))
    RendererWorkload.outcome(result, scale = 1.5, clockResolutionMs = 1.5) match
      case Outcome.Measured(workload) =>
        workload.typingP95Ms shouldBe 95.0
        workload.scale shouldBe 1.5
      case other => fail(s"expected a measurement, got $other")
  }

  it should "fail the run, naming the step, when every sample of a kind timed out" in {
    val result = Result(Samples(Vector(1.0), 0), Samples(Vector.empty, timedOut = 20), Samples(Vector(2.0), 0))
    RendererWorkload.outcome(result, scale = 2.0, clockResolutionMs = 1.5) match
      case Outcome.Failed(reason) => reason should (include("page-down") and include("20"))
      case other                  => fail(s"expected a failed run, got $other")
  }

  it should "fail the run, saying the screen cannot be read, when the run stopped because no keystroke showed" in {
    val result =
      Result(Samples(Vector.empty, timedOut = 5), Samples(Vector.empty, 0), Samples(Vector.empty, 0), true)
    RendererWorkload.outcome(result, scale = 2.0, clockResolutionMs = 1.5) match
      case Outcome.Failed(reason) =>
        reason shouldBe "the screen showed no change after 5 typed characters: it cannot be read from here"
      case other => fail(s"expected a failed run, got $other")
  }

  it should "fail the run when nothing was typed back" in {
    val result = Result(Samples(Vector.empty, timedOut = 150), Samples(Vector(1.0), 0), Samples(Vector(2.0), 0))
    RendererWorkload.outcome(result, scale = 2.0, clockResolutionMs = 1.5) match
      case Outcome.Failed(reason) => reason should (include("typing") and include("150"))
      case other                  => fail(s"expected a failed run, got $other")
  }

  it should "make every full redraw a theme toggle when the plan leaves the resize out" in {
    val noResize = plan.copy(resize = false)
    RendererWorkload.script(noResize).collect { case Step.FullRedraw(how) => how } shouldBe
      Vector.fill(4)(Redraw.ThemeToggle)
    RendererWorkload.script(noResize).size shouldBe RendererWorkload.script(plan).size
  }

  "RendererWorkload.outcome" should "carry the clock the run was measured by" in {
    val result = Result(Samples(Vector(1.0), 0), Samples(Vector(1.0), 0), Samples(Vector(1.0), 0))
    RendererWorkload.outcome(result, scale = 1.0, clockResolutionMs = 0.0, clock = RendererChoice.Clock.InApp) match
      case Outcome.Measured(workload) => workload.clock shouldBe RendererChoice.Clock.InApp
      case other                      => fail(s"expected a measurement, got $other")
    RendererWorkload.outcome(result, scale = 1.0, clockResolutionMs = 1.5) match
      case Outcome.Measured(workload) => workload.clock shouldBe RendererChoice.Clock.Pixel
      case other                      => fail(s"expected a measurement, got $other")
  }
