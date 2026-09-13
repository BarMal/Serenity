package com.serenity.state.undo

import java.lang.management.ManagementFactory

import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Deterministic regression guard for #1455's steady-state gap: below the cap, `boundedPush` was already a cheap cons
  * (the prior partial fix), but every push once the stack sits AT `maxUndoDepth` -- the steady state for any session
  * longer than that many edits -- still rebuilt the whole stack via `List.take`. A wall-clock assertion would catch
  * this, but this repo has already tried that twice for other hot paths and both attempts produced false failures under
  * CI load (see `CommandRunnerRenderPerformanceSpec`'s doc for the history); this instead measures bytes allocated per
  * push via the JVM's per-thread allocation counter, the same technique `perf.BenchmarkRunner` uses to separate a real
  * cost signal from scheduling noise. Allocation is deterministic and hardware-independent: it does not depend on how
  * fast or how busy the CI runner is.
  */
class UndoStatePerformanceSpec extends AnyFlatSpec with Matchers:

  private def dummyEntry(marker: Int): HistoryEntry =
    HistoryEntry.PanelChange(
      uiSurfaces = Nil,
      workspaceTree = None,
      maximizedWorkspaceNodeId = None,
      focus = Focus.EditorPane(PaneId(marker))
    )

  private def atCap(maxUndoDepth: Int): UndoState =
    (1 to maxUndoDepth).foldLeft(UndoState(maxUndoDepth = maxUndoDepth))((s, i) => s.pushUndo(dummyEntry(i)))

  private val allocationBean = ManagementFactory.getThreadMXBean match
    case bean: com.sun.management.ThreadMXBean if bean.isThreadAllocatedMemorySupported =>
      if !bean.isThreadAllocatedMemoryEnabled then bean.setThreadAllocatedMemoryEnabled(true)
      Some(bean)
    case _ => None

  /** Bytes allocated by a single `pushUndo` from `state`, which must already be at its cap -- the steady-state case.
    * Warms up first so the measured sample reflects JIT-compiled code, not the interpreter, matching
    * `perf.BenchmarkRunner`'s own warmup discipline.
    */
  private def allocatedBytesForOneSteadyStatePush(state: UndoState): Option[Long] =
    allocationBean.map { bean =>
      val threadId = Thread.currentThread().threadId()
      (1 to 2_000).foreach(_ => assert(state.pushUndo(dummyEntry(0)).undoStack.nonEmpty))
      val before = bean.getThreadAllocatedBytes(threadId)
      val result = state.pushUndo(dummyEntry(0))
      val after  = bean.getThreadAllocatedBytes(threadId)
      assert(result.undoStack.nonEmpty)
      after - before
    }

  "pushUndo at the cap (steady state)" should "allocate a bounded amount of memory independent of maxUndoDepth" in {
    val smallCapAllocated = allocatedBytesForOneSteadyStatePush(atCap(50))
    val largeCapAllocated = allocatedBytesForOneSteadyStatePush(atCap(50_000))

    (smallCapAllocated, largeCapAllocated) match
      case (Some(small), Some(large)) =>
        // Under the pre-fix `List.take(effectiveMaxUndoDepth - 1)` behavior, every push at the cap rebuilds
        // effectiveMaxUndoDepth - 1 cons cells, so allocation scales linearly with maxUndoDepth: a push into a
        // 50,000-deep stack would allocate roughly 1000x what a 50-deep stack does. A genuinely O(1)-amortized push
        // allocates a small, bounded amount regardless of maxUndoDepth.
        withClue(s"small-cap push allocated ${small}B, large-cap push allocated ${large}B: ") {
          large should be < small * 20L
          large should be < 200_000L
        }
      case _ =>
        info("JVM per-thread allocation counter unsupported on this runtime -- skipping the allocation assertion")
  }
