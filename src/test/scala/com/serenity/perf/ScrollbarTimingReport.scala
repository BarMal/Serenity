package com.serenity.perf

import com.serenity.state.manager.ScrollCountExecutor

/** The first to the last of four panes' scrollbars, beside the row `scrollbar.four_panes.all_bars`, which keeps only
  * the last (#2478): the counts of panes shown together queue on one thread, so if that is the cost the waits step up
  * in the order the bars arrive. Not part of the perf gate. Run it with
  * `sbt "Test / runMain com.serenity.perf.ScrollbarTimingReport"`.
  */
private[perf] object ScrollbarTimingReport:

  private val Warmups     = 2
  private val Repetitions = 10

  def main(args: Array[String]): Unit =
    val state = ScrollbarTiming.withScrollbar(
      MultiBufferFixture.split(
        MultiBufferFixture.state(MultiBufferFixture.Kind.Prose, 4, MultiBufferFixture.LateCountSize),
        4
      )
    )
    val runs = (1 to Warmups + Repetitions).toList.map(_ =>
      ScrollbarTiming.arrivalOrder(ScrollbarTiming.waits(state, ScrollCountExecutor.shared))
    )
    val kept = runs.drop(Warmups)
    println(
      s"time to each of four panes' bars, in the order they arrive, over $Repetitions runs (container figures unless on a laptop)"
    )
    println(f"${"bar"}%5s ${"min ms"}%9s ${"median ms"}%10s ${"max ms"}%9s")
    (0 until 4).foreach { rank =>
      val waits = kept.map(_(rank)).sorted.map(_ / 1_000_000.0)
      println(f"${rank + 1}%5d ${waits.head}%9.1f ${waits(waits.size / 2)}%10.1f ${waits.last}%9.1f")
    }
    val steps = kept.map(run => (run.last - run.head) / 1_000_000.0).sorted
    println(f"last minus first: median ${steps(steps.size / 2)}%.1f ms")
