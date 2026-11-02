package com.serenity.perf

import java.util.concurrent.atomic.AtomicInteger

import cats.effect.unsafe.IORuntime
import com.serenity.keystroke.events.{PageDown, ScrollDown}
import com.serenity.rope.Balance
import com.serenity.state.manager.{RenderCaches, ScrollCountExecutor}
import com.serenity.state.models.{AppState, BufferId}
import com.serenity.ui.layout.LayoutEngine

/** Rows for how performance scales with many open buffers and with split panes (#2478), each with a hypothesis it can
  * refute. They only record: `scripts/perf_record_only.txt` keeps the gate off them until the numbers have been read.
  *
  *   - `buffers.switch_cold_frame.open_N.<kind>`: the cold frame after a tab switch, with `N` buffers open and the same
  *     content on screen, so the difference between `open_1` and `open_20` is the cost of the buffers that are not
  *     shown.
  *   - `panes.split{1,2,4}.<frame>`: a typing, a one-line scroll and a page-down frame with a different buffer in each
  *     pane; `split1` is the control, built from the same code.
  *   - `scrollbar.four_panes.all_bars`: the first frame of four panes together, to the moment the last bar is counted.
  */
private[perf] object ScalingBenchmarks:

  given Balance = Balance.default

  import MultiBufferFixture.Kind

  // The laptop render rows' counts, which these rows' frames are drawn the same way as.
  private val Warmups    = 2
  private val Iterations = 12

  private lazy val image = LaptopFrameBenchmarks.frameImage()

  def benchmarks()(using IORuntime): List[BenchmarkRunner.Benchmark] =
    coldFrames ++ splitFrames ++ List(lateBars)

  private def coldFrames: List[BenchmarkRunner.Benchmark] =
    for
      open <- List(1, 20)
      kind <- Kind.values.toList
    yield switchColdFrame(kind, open)

  private def switchColdFrame(kind: Kind, open: Int): BenchmarkRunner.Benchmark =
    val base = MultiBufferFixture.state(kind, open, MultiBufferFixture.LaptopSize)
    // Every buffer but the first, one after another; the single buffer when only one is open.
    val shown =
      if open == 1 then Vector(base)
      else (2 to open).map(index => MultiBufferFixture.switchTo(base, BufferId(index))).toVector
    val next                       = new AtomicInteger(0)
    def coldFrame(state: AppState) = LaptopFrameBenchmarks.renderedFrame(state, RenderCaches.create(), image)
    BenchmarkRunner.Benchmark(
      s"buffers.switch_cold_frame.open_$open.${kind.label}",
      Warmups,
      Iterations,
      () =>
        assert(shown.forall(_.focusedBufferId.isDefined), "a switch left no buffer shown")
        assert(
          shown.map(_.focusedBufferId).distinct.size == shown.size,
          s"the switches did not each show a different buffer: ${shown.map(_.focusedBufferId)}"
        )
        assert(LaptopFrameBenchmarks.hasPixels(coldFrame(shown.head)), "the cold frame drew nothing")
      ,
      () => coldFrame(shown(next.getAndIncrement() % shown.size))
    )

  private val frames: List[(String, MultiPaneSession => (AppState, AppState))] =
    List(
      "typing_frame"      -> (_.typing()),
      "scroll_line_frame" -> (_.transition(ScrollDown(1))),
      "page_down_frame"   -> (_.transition(PageDown))
    )

  private def splitFrames(using IORuntime): List[BenchmarkRunner.Benchmark] =
    for
      panes            <- List(1, 2, 4)
      (action, change) <- frames
    yield splitFrame(panes, action, change)

  private def splitFrame(
    panes: Int,
    action: String,
    change: MultiPaneSession => (AppState, AppState)
  )(using IORuntime): BenchmarkRunner.Benchmark =
    val name            = s"panes.split$panes.$action"
    val session         = MultiPaneSession.open(panes, MultiBufferFixture.LaptopSize)
    val (before, after) = change(session)
    val transition      = LaptopFrameBenchmarks.AlternatingTransition(before, after)
    BenchmarkRunner.Benchmark(
      name,
      Warmups,
      Iterations,
      () => verifySplitFrames(name, transition, before, after),
      () => transition.nextFrame()
    )

  /** A damaged frame must match a cold one pixel for pixel. With two or more panes, pixels within one cell of the edge
    * between two panes are let through and reported (#2481): a one-line scroll leaves a band of five device columns
    * there that a cold frame draws differently. Anything elsewhere fails.
    */
  private def verifySplitFrames(
    name: String,
    transition: LaptopFrameBenchmarks.AlternatingTransition,
    before: AppState,
    after: AppState
  ): Unit =
    List(after, before).foreach { expected =>
      val drawn   = transition.nextFrame()
      val cold    = LaptopFrameBenchmarks.renderedFrame(expected, RenderCaches.create(), image, cursorVisible = false)
      val bands   = paneEdgeBands(expected)
      val outside = LaptopFrameBenchmarks.differingPixels(drawn, cold).filterNot((x, _) => bands.exists(_.contains(x)))
      assert(
        outside.isEmpty,
        s"$name: the damaged frame differs from a full frame in ${outside.size} pixels away from the pane edges: " +
          outside.take(8).mkString(" ")
      )
    }

  /** The device columns within one cell of an edge that two panes share. */
  private def paneEdgeBands(state: AppState): List[Range] =
    val layout = LayoutEngine.calculateLayout(state, LaptopFrameBenchmarks.viewport)
    val panes  = LayoutEngine.calculatePaneLayouts(state, layout).values.toList
    val cell   = LaptopFrameBenchmarks.cellMetrics.charWidth * LaptopFrameBenchmarks.DeviceScale.toInt
    val lefts  = panes.map(_.x).toSet
    val shared = panes.map(_.right).filter(lefts.contains).distinct
    shared.map(edge => (edge * cell - cell) to (edge * cell + cell))

  private def lateBars: BenchmarkRunner.Benchmark =
    val state = ScrollbarTiming.withScrollbar(
      MultiBufferFixture.split(MultiBufferFixture.state(Kind.Prose, 4, MultiBufferFixture.LateCountSize), 4)
    )
    BenchmarkRunner.Benchmark(
      "scrollbar.four_panes.all_bars",
      Warmups,
      Iterations,
      () =>
        val waits = ScrollbarTiming.waits(state, ScrollCountExecutor.shared)
        assert(waits.size == 4, s"only ${waits.size} of four bars were counted late")
      ,
      () => ScrollbarTiming.waits(state, ScrollCountExecutor.shared)
    )
