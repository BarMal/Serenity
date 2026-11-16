package com.serenity.app

import scala.concurrent.duration.*

import cats.Monad
import cats.effect.IO
import com.serenity.app.RendererChoice.{Outcome, Workload}

/** What one renderer trial does to the window, and what it keeps from it (#2618).
  *
  * The script is pure data. The window that performs a step, and the clock that times it, sit behind [[Window]]: the
  * runner only paces the steps and sorts the answers by kind. A step the window could not time before its deadline
  * answers `None`; it is counted and left out of every median.
  */
object RendererWorkload:

  enum Redraw:
    case Resize
    case ThemeToggle

  enum Step:
    case Type(char: Char)
    case PageDown
    case FullRedraw(how: Redraw)

    def kind: Kind = this match
      case _: Type       => Kind.Typing
      case PageDown      => Kind.PageDown
      case _: FullRedraw => Kind.FullRedraw

  enum Kind:
    case Typing
    case PageDown
    case FullRedraw

  /** `typingPace` separates keystrokes; `settle` is the wait after every other step, so a page or a redraw is never
    * still painting when the next one starts. When none of the first `unreadableAfter` keystrokes shows, the screen is
    * taken to be unreadable and the run stops there: every later step would wait out its own timeout for nothing.
    */
  final case class Plan(
      typedChars: Int,
      pageDowns: Int,
      fullRedraws: Int,
      typingPace: FiniteDuration,
      settle: FiniteDuration,
      unreadableAfter: Int = 5,
      resize: Boolean = true
  )

  object Plan:
    /** 150 characters give a typing p95 of more than three samples; 20 pages and 10 redraws are enough for a median. */
    val standard: Plan = Plan(150, 20, 10, typingPace = 90.millis, settle = 400.millis)

  /** Performs one step and answers how long the screen took to show it, in milliseconds, or `None` on a timeout. */
  trait Window:
    def perform(step: Step): IO[Option[Double]]

  final case class Samples(latenciesMs: Vector[Double], timedOut: Int)

  final case class Result(
      typing: Samples,
      pageDown: Samples,
      fullRedraw: Samples,
      unreadableScreen: Boolean = false
  )

  private val prose = "thequickbrownfoxjumpsoverthelazydog"

  def script(plan: Plan): Vector[Step] =
    Vector.tabulate(plan.typedChars)(index => Step.Type(prose(index % prose.length))) ++
      Vector.fill(plan.pageDowns)(Step.PageDown) ++
      Vector.tabulate(plan.fullRedraws)(index =>
        Step.FullRedraw(if plan.resize && index % 2 == 0 then Redraw.Resize else Redraw.ThemeToggle)
      )

  private val ProgressEvery = 30

  /** Runs the script, saying through `progress` how far it has got every [[ProgressEvery]] steps. */
  def run(plan: Plan, window: Window, progress: String => IO[Unit] = _ => IO.unit): IO[Result] =
    val steps = script(plan)
    Monad[IO].tailRecM(Vector.empty[(Step, Option[Double])]) { answers =>
      steps.lift(answers.size) match
        case None => IO.pure(Right(result(answers, unreadable = false)))
        case Some(step) =>
          window.perform(step).flatMap { answer =>
            val done = answers :+ (step -> answer)
            if noKeystrokeShowed(plan, done) then IO.pure(Right(result(done, unreadable = true)))
            else reportProgress(done, steps.size, progress) *> IO.sleep(wait(plan, step)).as(Left(done))
          }
    }

  private def noKeystrokeShowed(plan: Plan, answers: Vector[(Step, Option[Double])]): Boolean =
    plan.unreadableAfter > 0 && answers.size == plan.unreadableAfter &&
      answers.forall((step, answer) => step.kind == Kind.Typing && answer.isEmpty)

  private def reportProgress(
    answers: Vector[(Step, Option[Double])],
    total: Int,
    progress: String => IO[Unit]
  ): IO[Unit] =
    IO.whenA(answers.size % ProgressEvery == 0)(
      progress(s"[TRIAL] step ${answers.size} of $total, ${answers.count(_._2.isEmpty)} timed out")
    )

  private def result(answers: Vector[(Step, Option[Double])], unreadable: Boolean): Result =
    def samples(kind: Kind): Samples =
      val ofKind = answers.filter((step, _) => step.kind == kind).map(_._2)
      Samples(ofKind.flatten, ofKind.count(_.isEmpty))
    Result(
      typing = samples(Kind.Typing),
      pageDown = samples(Kind.PageDown),
      fullRedraw = samples(Kind.FullRedraw),
      unreadableScreen = unreadable
    )

  private def wait(plan: Plan, step: Step): FiniteDuration = step match
    case _: Step.Type => plan.typingPace
    case _            => plan.settle

  def outcome(
    result: Result,
    scale: Double,
    clockResolutionMs: Double,
    clock: RendererChoice.Clock = RendererChoice.Clock.Pixel,
    canvasPixels: Long = 0L
  ): Outcome =
    if result.unreadableScreen then
      Outcome.Failed(
        s"the screen showed no change after ${result.typing.timedOut} typed characters: it cannot be read from here"
      )
    else measured(result, scale, clockResolutionMs, clock, canvasPixels)

  private def measured(
    result: Result,
    scale: Double,
    clockResolutionMs: Double,
    clock: RendererChoice.Clock,
    canvasPixels: Long
  ): Outcome =
    val unanswered = List("typing" -> result.typing, "page-down" -> result.pageDown, "full-redraw" -> result.fullRedraw)
      .filter((_, samples) => samples.latenciesMs.isEmpty)
    if unanswered.nonEmpty then
      Outcome.Failed(
        unanswered
          .map((name, samples) => s"every $name sample timed out (${samples.timedOut})")
          .mkString("; ")
      )
    else
      Outcome.Measured(
        Workload(
          typingP50Ms = RendererChoice.median(result.typing.latenciesMs.toList),
          typingP95Ms = RendererChoice.percentile(result.typing.latenciesMs.toList, 95),
          scale = scale,
          clockResolutionMs = clockResolutionMs,
          clock = clock,
          canvasPixels = canvasPixels,
          fullWindowMs = Map(
            "page-down"   -> RendererChoice.median(result.pageDown.latenciesMs.toList),
            "full-redraw" -> RendererChoice.median(result.fullRedraw.latenciesMs.toList)
          )
        )
      )
