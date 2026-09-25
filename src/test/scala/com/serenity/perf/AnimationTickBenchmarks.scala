package com.serenity.perf

import com.serenity.document.CommentRendering
import com.serenity.state.models.{AppState, AppStateValidation}

/** The cost `ModelCommit.advanceTick` (#1697 Wave 4) added to the render tick's hot path: every frame's animation
  * advance now runs through `StateManagerOperationBoundary.prepareCommit` -- `AppStateValidation` plus the floating
  * comment-lens sync -- instead of committing unvalidated. `prepareCommit` itself is `private[manager]`, so this
  * benchmarks its two component calls directly on a large, representative editing state; together they are exactly the
  * added per-tick cost this change should keep cheap. Kept apart from [[PerformanceBenchmarks]] for the same reason
  * [[DamageBenchmarks]] is: a fixture edit and a measurement edit stay separate.
  */
private[perf] object AnimationTickBenchmarks:

  def benchmarks(editingState: AppState): List[BenchmarkRunner.Benchmark] =
    List(
      BenchmarkRunner.Benchmark(
        "animation.tick_validated_commit",
        3,
        BenchmarkIterationCounts.Reducer,
        () => assert(AppStateValidation.validated(editingState).isRight),
        () =>
          AppStateValidation
            .validated(editingState)
            .map(CommentRendering.syncFloatingLensWithCursor(_, editingState))
      )
    )
