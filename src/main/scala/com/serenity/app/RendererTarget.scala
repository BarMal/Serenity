package com.serenity.app

import java.util.Locale

import com.serenity.app.RendererChoice.{Candidate, CandidateRuns, Clock, Outcome, Recommendation, Workload}

/** How each measured renderer mode stands against the typing target of #2489: input to paint at 2x, p95 at or below
  * 16.7 ms.
  *
  * This only reports. It never changes which mode [[RendererChoice]] recommends, and it says plainly when no mode gets
  * there, because choosing among the modes that exist is not what closes a gap that none of them closes.
  */
object RendererTarget:

  val TargetMs: Double = 16.7

  /** What "2x" means here: a canvas of at least this many device pixels, which is what the trial really drew. The scale
    * the JVM reports is not used: under XWayland it says 1.0 on a 2x monitor.
    */
  val MinCanvasPixels: Long = 5_000_000L

  /** A clock that resolves more coarsely than a quarter of the target cannot support a verdict on it. */
  val MaxResolutionMs: Double = TargetMs / 4.0

  enum Verdict:
    case Meets(p95Ms: Double)
    case Misses(p95Ms: Double, overByMs: Double)
    case NotJudged(reason: String)

  private def ms(value: Double): String        = "%.1f".formatLocal(Locale.ROOT, value)
  private def megapixels(pixels: Long): String = ms(pixels / 1_000_000.0)

  def judge(runs: CandidateRuns): Verdict =
    val measured: List[Workload] = runs.runs.collect { case Outcome.Measured(workload) => workload }
    measured.map(_.canvasPixels).minOption match
      case None => Verdict.NotJudged("no completed run")
      case Some(0L) =>
        Verdict.NotJudged("the canvas size was not recorded, so it cannot be said to be 2x")
      case Some(pixels) if pixels < MinCanvasPixels =>
        Verdict.NotJudged(
          s"the canvas had ${megapixels(pixels)} megapixels; the target is defined at 2x, ${megapixels(MinCanvasPixels)} megapixels or more"
        )
      case Some(_) =>
        val resolution = RendererChoice.median(measured.map(_.clockResolutionMs))
        val p95        = RendererChoice.median(measured.map(_.typingP95Ms))
        if resolution > MaxResolutionMs then
          Verdict.NotJudged(
            s"the clock resolves only to ${ms(resolution)} ms; a verdict on ${ms(TargetMs)} ms needs at most ${ms(MaxResolutionMs)} ms"
          )
        else if p95 <= TargetMs then Verdict.Meets(p95)
        else Verdict.Misses(p95, p95 - TargetMs)

  /** One line for each mode, then, when at least one mode was judged, which of them meet the target or that none does.
    */
  def lines(results: List[CandidateRuns]): List[String] =
    val judged = results.map(runs => runs.candidate -> judge(runs))
    def caveat(candidate: Candidate): String =
      val inApp = results
        .find(_.candidate == candidate)
        .exists(_.runs.exists {
          case Outcome.Measured(workload) => workload.clock == Clock.InApp
          case _                          => false
        })
      if inApp then s" (${RendererChoice.InAppCaveat})" else ""
    val perMode = judged.map {
      case (candidate, Verdict.Meets(p95)) =>
        s"${candidate.label}: typing p95 ${ms(p95)} ms at 2x, ${ms(TargetMs)} ms target met${caveat(candidate)}"
      case (candidate, Verdict.Misses(p95, over)) =>
        s"${candidate.label}: typing p95 ${ms(p95)} ms at 2x, ${ms(TargetMs)} ms target missed by ${ms(over)} ms${caveat(candidate)}"
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
