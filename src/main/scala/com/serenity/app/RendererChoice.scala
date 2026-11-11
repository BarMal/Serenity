package com.serenity.app

import java.util.Locale

/** What `serenity --optimize-renderer` recommends from what it measured (#2618): switch to a mode only when it is
  * clearly better at typing and not clearly worse at changing the whole window, and otherwise say so.
  *
  * A mode wins against another when its typing median is at least 15% lower, none of the full-window rows both
  * measured is more than 15% higher, and the typing gap is more than twice the spread between either mode's own runs.
  * A mode must win against every other mode that was measured. A mode with any failed run is reported, never chosen.
  */
object RendererChoice:

  enum Candidate(val label: String):
    case LayerImage   extends Candidate("layer+image")
    case LayerPicture extends Candidate("layer+picture")
    case SkiaRaster   extends Candidate("skia-raster")

  /** One run of the workload: the typing median, and the median of each full-window row it measured. */
  final case class Workload(typingP50Ms: Double, fullWindowMs: Map[String, Double])

  enum Outcome:
    case Measured(workload: Workload)
    case Failed(reason: String)

  final case class CandidateRuns(candidate: Candidate, runs: List[Outcome])

  enum Recommendation:
    case Switch(to: Candidate, lines: List[String])
    case Keep(why: String, lines: List[String])

    def reason: String = this match
      case Switch(to, _) => s"${to.label} is clearly faster here"
      case Keep(why, _)  => why

    def evidence: List[String] = this match
      case Switch(_, lines) => lines
      case Keep(_, lines)   => lines

  private val WinnerMustBeBelowPercent   = 85.0
  private val RowMayBeUpToPercent        = 115.0
  private val GapMustExceedSpreadsFactor = 2.0

  final private case class Measure(
      candidate: Candidate,
      typingMs: Double,
      spreadMs: Double,
      rowsMs: Map[String, Double]
  )

  private def median(values: List[Double]): Double =
    val sorted = values.sorted
    val middle = sorted.size / 2
    if sorted.size % 2 == 1 then sorted(middle) else (sorted(middle - 1) + sorted(middle)) / 2.0

  private def summarise(candidate: Candidate, workloads: List[Workload]): Measure =
    val typing = workloads.map(_.typingP50Ms)
    val rowNames = workloads.flatMap(_.fullWindowMs.keys).distinct
    Measure(
      candidate,
      median(typing),
      (for highest <- typing.maxOption; lowest <- typing.minOption yield highest - lowest).getOrElse(0.0),
      rowNames.map(name => name -> median(workloads.flatMap(_.fullWindowMs.get(name)))).toMap
    )

  private def ms(value: Double): String = "%.1f".formatLocal(Locale.ROOT, value)

  private def evidenceOf(measures: List[Measure]): List[String] =
    measures.sortBy(_.typingMs).map { measure =>
      val rows = measure.rowsMs.toList.sortBy(_._1).map((name, value) => s"$name ${ms(value)} ms")
      val tail = if rows.isEmpty then "" else rows.mkString(", ", ", ", "")
      s"${measure.candidate.label}: typing p50 ${ms(measure.typingMs)} ms$tail"
    }

  /** Why `winner` does not clearly beat `other`, if it does not. */
  private def notClearlyBetter(winner: Measure, other: Measure): Option[String] =
    val worseRows = winner.rowsMs.toList.sortBy(_._1).collect {
      case (name, value) if other.rowsMs.get(name).exists(theirs => value * 100.0 > theirs * RowMayBeUpToPercent) => name
    }
    if winner.typingMs * 100.0 > other.typingMs * WinnerMustBeBelowPercent then
      Some(s"${winner.candidate.label} is within 15% of ${other.candidate.label} at typing")
    else if worseRows.nonEmpty then
      Some(
        s"${winner.candidate.label} is more than 15% slower than ${other.candidate.label} on the full-window row " +
          worseRows.mkString(", ")
      )
    else if other.typingMs - winner.typingMs <= GapMustExceedSpreadsFactor * math.max(winner.spreadMs, other.spreadMs)
    then
      Some(
        s"the typing gap between ${winner.candidate.label} and ${other.candidate.label} is within twice the spread " +
          "between a mode's own runs"
      )
    else None

  def recommend(current: Candidate, results: List[CandidateRuns]): Recommendation =
    val (failedRuns, usable) =
      results.partition(candidate => candidate.runs.isEmpty || candidate.runs.exists(_.isInstanceOf[Outcome.Failed]))
    val failures = failedRuns.map { runs =>
      val reasons = runs.runs.collect { case Outcome.Failed(reason) => reason }
      s"${runs.candidate.label} failed (${if reasons.isEmpty then "no run" else reasons.mkString("; ")})"
    }
    val measures = usable.map { runs =>
      summarise(runs.candidate, runs.runs.collect { case Outcome.Measured(workload) => workload })
    }
    val evidence = evidenceOf(measures) ++ failures
    measures.sortBy(_.typingMs) match
      case Nil =>
        Recommendation.Keep(s"no mode could be measured: ${failures.mkString("; ")}", evidence)
      case _ :: Nil =>
        val also = if failures.isEmpty then "" else s" (${failures.mkString("; ")})"
        Recommendation.Keep(s"only one mode could be measured$also, so there is nothing to compare it with", evidence)
      case winner :: others =>
        val refusals = others.flatMap(notClearlyBetter(winner, _))
        refusals.headOption match
          case Some(refusal) =>
            val failed = if failures.isEmpty then "" else s"; ${failures.mkString("; ")}"
            Recommendation.Keep(s"no clear winner: $refusal$failed", evidence)
          case None if winner.candidate == current =>
            Recommendation.Keep(s"${current.label} is already the best mode here", evidence)
          case None => Recommendation.Switch(winner.candidate, evidence)
