package com.serenity.app

import java.nio.file.Path
import java.time.Instant

import cats.effect.IO
import cats.syntax.all.*
import com.serenity.app.RendererBenchmarkResult.{Key, Stored}
import com.serenity.app.RendererChoice.{Candidate, CandidateRuns, Outcome, Recommendation}

/** What `serenity --optimize-renderer` does once it has a way to start a trial (#2618): run the trials in order, read
  * what each child printed, recommend, keep the result, and say what it found.
  *
  * Starting a child is behind [[Spawner]], so everything here is exercised without a window. A trial that could not
  * start, or whose child printed nothing usable, is a failed run for that mode; the rest still run.
  */
object RendererCoordinator:

  /** Starts one trial child for a mode and answers everything it printed. */
  trait Spawner:
    def run(candidate: Candidate): IO[String]

  /** What the machine says about itself when the trials are done, and where the result is kept. */
  final case class Environment(key: IO[Key], now: IO[Instant], resultPath: Path)

  /** The trial number (from 1), the number of trials, and the mode about to run. */
  type Progress = (Int, Int, Candidate) => IO[Unit]

  final case class Finished(
      runs: List[CandidateRuns],
      recommendation: Recommendation,
      current: Candidate,
      saveError: Option[String]
  )

  def optimize(
    current: Candidate,
    thorough: Boolean,
    spawner: Spawner,
    environment: Environment,
    progress: Progress
  ): IO[Finished] =
    val order = RendererRunOrder.order(thorough)
    for
      outcomes <- order.zipWithIndex.traverse { (candidate, index) =>
        progress(index + 1, order.size, candidate) *> trial(spawner, candidate).map(candidate -> _)
      }
      runs = RendererRunOrder.candidates(thorough).map { candidate =>
        CandidateRuns(candidate, outcomes.collect { case (`candidate`, outcome) => outcome })
      }
      recommendation = RendererTarget.annotate(RendererChoice.recommend(current, runs), runs)
      saveError <- save(environment, runs)
    yield Finished(runs, recommendation, current, saveError)

  private def trial(spawner: Spawner, candidate: Candidate): IO[Outcome] =
    spawner
      .run(candidate)
      .map(RendererTrial.outcomeOf(_, candidate))
      .handleError(error => Outcome.Failed(s"the trial could not be run: ${error.getMessage}"))

  private def save(environment: Environment, runs: List[CandidateRuns]): IO[Option[String]] =
    (for
      key <- environment.key
      now <- environment.now
      _   <- RendererBenchmarkResult.save(environment.resultPath, Stored(key, now, runs))
    yield Option.empty[String]).handleError(error => Some(String.valueOf(error.getMessage)))

  /** What to print: the recommendation and its evidence, what to set for a switch and how to have it applied, and where
    * the result went, or why it did not.
    */
  def report(finished: Finished): List[String] =
    val headline = s"Recommendation: ${finished.recommendation.reason}."
    val advice = finished.recommendation match
      case Recommendation.Switch(to, _) =>
        List(
          s"To use it, set ${RendererSettings.line(to)} in config.conf, or run again with --apply to have it written.",
          s"You are now on ${finished.current.label}."
        )
      case Recommendation.Keep(_, _) => List(s"Nothing to change: staying on ${finished.current.label}.")
    val kept = finished.saveError.fold(
      List(s"The measurements are kept in ${RendererBenchmarkResult.fileName}, beside config.conf.")
    )(error => List(s"The measurements could not be saved: $error"))
    headline :: finished.recommendation.evidence ::: advice ::: kept
