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
    text.forall(char => char.isLetter || char == ' ') shouldBe true
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
    val recording = new RecordingWindow(_ => None)
    val performed = runVirtual(RendererWorkload.run(plan, recording.window) *> recording.performed)
    performed should have size 13
  }

  "RendererWorkload.outcome" should "report each kind of step by its median, with the rows named as the choice rule reads them" in {
    val result = Result(
      typing = Samples(Vector(30.0, 10.0, 20.0), timedOut = 0),
      pageDown = Samples(Vector(5.0, 7.0, 9.0, 100.0), timedOut = 1),
      fullRedraw = Samples(Vector(40.0), timedOut = 0)
    )
    RendererWorkload.outcome(result, scale = 2.0) shouldBe Outcome.Measured(
      RendererChoice.Workload(
        typingP50Ms = 20.0,
        typingP95Ms = 30.0,
        scale = 2.0,
        fullWindowMs = Map("page-down" -> 8.0, "full-redraw" -> 40.0)
      )
    )
  }

  it should "report the typing p95 of the samples, not of the median, and the scale the window ran at" in {
    val typing = Samples((1 to 100).map(_.toDouble).toVector, timedOut = 0)
    val result = Result(typing, Samples(Vector(5.0), 0), Samples(Vector(40.0), 0))
    RendererWorkload.outcome(result, scale = 1.5) match
      case Outcome.Measured(workload) =>
        workload.typingP95Ms shouldBe 95.0
        workload.scale shouldBe 1.5
      case other => fail(s"expected a measurement, got $other")
  }

  it should "fail the run, naming the step, when every sample of a kind timed out" in {
    val result = Result(Samples(Vector(1.0), 0), Samples(Vector.empty, timedOut = 20), Samples(Vector(2.0), 0))
    RendererWorkload.outcome(result, scale = 2.0) match
      case Outcome.Failed(reason) => reason should (include("page-down") and include("20"))
      case other                  => fail(s"expected a failed run, got $other")
  }

  it should "fail the run when nothing was typed back" in {
    val result = Result(Samples(Vector.empty, timedOut = 150), Samples(Vector(1.0), 0), Samples(Vector(2.0), 0))
    RendererWorkload.outcome(result, scale = 2.0) match
      case Outcome.Failed(reason) => reason should (include("typing") and include("150"))
      case other                  => fail(s"expected a failed run, got $other")
  }
