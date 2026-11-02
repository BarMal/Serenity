package com.serenity.perf

import com.serenity.state.manager.ScrollCountExecutor
import com.serenity.state.models.AppState
import com.serenity.ui.layout.PaneScroll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ScrollbarTimingSpec extends AnyFlatSpec with Matchers:

  import MultiBufferFixture.{Kind, Size}

  private def panesOf(kind: Kind, size: Size, panes: Int): AppState =
    ScrollbarTiming.withScrollbar(MultiBufferFixture.split(MultiBufferFixture.state(kind, panes, size), panes))

  /** Just under `PaneScroll.ExactLineLimit`, so each pane's bar waits for a late count. */
  private def fourPanes: AppState = panesOf(Kind.Prose, MultiBufferFixture.LateCountSize, 4)

  "ScrollbarTiming.waits" should "report one wait for each of the four panes, counted on the real count thread" in {
    val state = fourPanes

    val waits = ScrollbarTiming.waits(state, ScrollCountExecutor.shared)

    waits.keySet shouldBe state.persisted.layout.orderedPaneIds.toSet
    waits.values.foreach(_ should be > 0L)
    waits.size shouldBe 4
  }

  it should "report the same panes when the counts run on the calling thread, where every wait is part of the frame" in {
    val state = fourPanes

    ScrollbarTiming.waits(state, _.run()).keySet shouldBe state.persisted.layout.orderedPaneIds.toSet
  }

  it should "report no wait for a document over the exact limit, whose bar is estimated at once" in {
    val long = panesOf(Kind.Code, Size(PaneScroll.ExactLineLimit + 500), 2)

    ScrollbarTiming.waits(long, ScrollCountExecutor.shared) shouldBe empty
  }

  it should "give each wait from a fresh start, so repeating it measures the counting again" in {
    val state = fourPanes

    val first  = ScrollbarTiming.waits(state, ScrollCountExecutor.shared)
    val second = ScrollbarTiming.waits(state, ScrollCountExecutor.shared)

    first.keySet shouldBe second.keySet
    second.values.foreach(_ should be > 0L)
  }

  "ScrollbarTiming.lastBarNanos" should "be the longest wait" in {
    ScrollbarTiming.lastBarNanos(Map("a" -> 5L, "b" -> 9L, "c" -> 7L)) shouldBe 9L
  }
