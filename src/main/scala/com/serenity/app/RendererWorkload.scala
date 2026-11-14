package com.serenity.app

import scala.concurrent.duration.*

import cats.effect.IO
import cats.syntax.all.*
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
    * still painting when the next one starts.
    */
  final case class Plan(
      typedChars: Int,
      pageDowns: Int,
      fullRedraws: Int,
      typingPace: FiniteDuration,
      settle: FiniteDuration
  )

  object Plan:
    /** 150 characters give a typing p95 of more than three samples; 20 pages and 10 redraws are enough for a median. */
    val standard: Plan = Plan(150, 20, 10, typingPace = 90.millis, settle = 400.millis)

  /** Performs one step and answers how long the screen took to show it, in milliseconds, or `None` on a timeout. */
  trait Window:
    def perform(step: Step): IO[Option[Double]]

  final case class Samples(latenciesMs: Vector[Double], timedOut: Int)

  final case class Result(typing: Samples, pageDown: Samples, fullRedraw: Samples)

  private val prose = "the quick brown fox jumps over the lazy dog "

  def script(plan: Plan): Vector[Step] =
    Vector.tabulate(plan.typedChars)(index => Step.Type(prose(index % prose.length))) ++
      Vector.fill(plan.pageDowns)(Step.PageDown) ++
      Vector.tabulate(plan.fullRedraws)(index =>
        Step.FullRedraw(if index % 2 == 0 then Redraw.Resize else Redraw.ThemeToggle)
      )

  def run(plan: Plan, window: Window): IO[Result] =
    script(plan)
      .traverse(step => window.perform(step).flatTap(_ => IO.sleep(wait(plan, step))).map(step -> _))
      .map { answers =>
        def samples(kind: Kind): Samples =
          val ofKind = answers.filter((step, _) => step.kind == kind).map(_._2)
          Samples(ofKind.flatten, ofKind.count(_.isEmpty))
        Result(
          typing = samples(Kind.Typing),
          pageDown = samples(Kind.PageDown),
          fullRedraw = samples(Kind.FullRedraw)
        )
      }

  private def wait(plan: Plan, step: Step): FiniteDuration = step match
    case _: Step.Type => plan.typingPace
    case _            => plan.settle

  def outcome(result: Result, scale: Double): Outcome =
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
          fullWindowMs = Map(
            "page-down"   -> RendererChoice.median(result.pageDown.latenciesMs.toList),
            "full-redraw" -> RendererChoice.median(result.fullRedraw.latenciesMs.toList)
          )
        )
      )
