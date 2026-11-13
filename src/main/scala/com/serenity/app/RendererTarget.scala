package com.serenity.app

import java.util.Locale

import com.serenity.app.RendererChoice.{CandidateRuns, Outcome, Recommendation, Workload}

/** How each measured renderer mode stands against the typing target of #2489: input to paint at 2x, p95 at or below
  * 16.7 ms.
  *
  * This only reports. It never changes which mode [[RendererChoice]] recommends, and it says plainly when no mode gets
  * there, because choosing among the modes that exist is not what closes a gap that none of them closes.
  */
object RendererTarget:

  val TargetMs: Double    = 16.7
  val TargetScale: Double = 2.0

  private val ScaleTolerance = 0.05

  /** A clock that resolves more coarsely than a quarter of the target cannot support a verdict on it. */
  val MaxResolutionMs: Double = TargetMs / 4.0

  enum Verdict:
    case Meets(p95Ms: Double)
    case Misses(p95Ms: Double, overByMs: Double)
    case NotJudged(reason: String)

  private def ms(value: Double): String    = "%.1f".formatLocal(Locale.ROOT, value)
  private def times(value: Double): String = s"${ms(value)}x"

  def judge(runs: CandidateRuns): Verdict =
    val measured: List[Workload] = runs.runs.collect { case Outcome.Measured(workload) => workload }
    measured.map(_.scale).distinct match
      case Nil => Verdict.NotJudged("no completed run")
      case scale :: Nil if math.abs(scale - TargetScale) <= ScaleTolerance =>
        val resolution = RendererChoice.median(measured.map(_.clockResolutionMs))
        val p95        = RendererChoice.median(measured.map(_.typingP95Ms))
        if resolution > MaxResolutionMs then
          Verdict.NotJudged(
            s"the clock resolves only to ${ms(resolution)} ms; a verdict on ${ms(TargetMs)} ms needs at most ${ms(MaxResolutionMs)} ms"
          )
        else if p95 <= TargetMs then Verdict.Meets(p95)
        else Verdict.Misses(p95, p95 - TargetMs)
      case scale :: Nil => Verdict.NotJudged(s"measured at ${times(scale)}; the target is defined at 2x")
      case scales =>
        Verdict.NotJudged(s"the runs were measured at different scales (${scales.map(times).mkString(", ")})")

  /** One line for each mode, then, when at least one mode was judged, which of them meet the target or that none does.
    */
  def lines(results: List[CandidateRuns]): List[String] =
    val judged = results.map(runs => runs.candidate -> judge(runs))
    val perMode = judged.map {
      case (candidate, Verdict.Meets(p95)) =>
        s"${candidate.label}: typing p95 ${ms(p95)} ms at 2x, ${ms(TargetMs)} ms target met"
      case (candidate, Verdict.Misses(p95, over)) =>
        s"${candidate.label}: typing p95 ${ms(p95)} ms at 2x, ${ms(TargetMs)} ms target missed by ${ms(over)} ms"
      case (candidate, Verdict.NotJudged(reason)) => s"${candidate.label}: target not judged ($reason)"
    }
    val meeting = judged.collect { case (candidate, _: Verdict.Meets) => candidate.label }
    val anyJudged = judged.exists {
      case (_, _: Verdict.NotJudged) => false
      case _                         => true
    }
    val summary =
      if meeting.nonEmpty then List(s"Meets the ${ms(TargetMs)} ms target: ${meeting.mkString(", ")}.")
      else if anyJudged then
        List(
          s"No renderer mode reaches the ${ms(TargetMs)} ms typing p95 target (#2489) on this machine; " +
            "choosing a mode cannot get there."
        )
      else Nil
    perMode ++ summary

  /** The same recommendation with the target report added to its evidence. */
  def annotate(recommendation: Recommendation, results: List[CandidateRuns]): Recommendation =
    val report = lines(results)
    recommendation match
      case Recommendation.Switch(to, evidence) => Recommendation.Switch(to, evidence ++ report)
      case Recommendation.Keep(why, evidence)  => Recommendation.Keep(why, evidence ++ report)
