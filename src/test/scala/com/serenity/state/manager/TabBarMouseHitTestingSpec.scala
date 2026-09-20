package com.serenity.state.manager

import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.{LayoutRect, ViewportSize}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Coverage for `TabBarMouseHitTesting` -- resolving a click's cell coordinate to the `BufferId` of the tab it landed
  * on (issue #1075: Foundation), and resolving a click against live `AppState` into a buffer switch (issue #1077:
  * "Click a tab to switch buffer").
  */
class TabBarMouseHitTestingSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private def entry(id: Int, title: String): TabListEntry = TabListEntry(BufferId(id), title, isDirty = false)

  private val entries = List(entry(0, "one"), entry(1, "two"), entry(2, "three"))
  // Widths 6/6/5 (see TabBarSurfaceCompositionSpec): tab 0 at [0,6), gap [6,8), tab 1 at [8,14), gap [14,16),
  // tab 2 at [16,21).
  private val rect = LayoutRect(0, 0, 21, 1)

  "hitAt" should "resolve a click inside the first tab to its BufferId" in {
    TabBarMouseHitTesting.hitAt(entries, None, rect, col = 3, row = 0) shouldBe Some(BufferId(0))
  }

  it should "resolve a click inside the second tab to its BufferId" in {
    TabBarMouseHitTesting.hitAt(entries, None, rect, col = 10, row = 0) shouldBe Some(BufferId(1))
  }

  it should "resolve a click inside the third tab to its BufferId" in {
    TabBarMouseHitTesting.hitAt(entries, None, rect, col = 20, row = 0) shouldBe Some(BufferId(2))
  }

  it should "resolve no tab for a click on the separator glyph between tabs" in {
    TabBarMouseHitTesting.hitAt(entries, None, rect, col = 7, row = 0) shouldBe None
  }

  it should "resolve no tab for a click outside the strip's row" in {
    TabBarMouseHitTesting.hitAt(entries, None, rect, col = 3, row = 1) shouldBe None
  }

  it should "resolve no tab when there are no open buffers" in {
    TabBarMouseHitTesting.hitAt(Nil, None, rect, col = 3, row = 0) shouldBe None
  }

  // Widths 10/9 (19 content columns over 2 tabs, GapColumns=2 reserved for the one gap): tab 0 (BufferId(0), the
  // pane's already-active buffer) at [0,10), gap [10,12), tab 1 (BufferId(1)) at [12,21).
  private val twoTabViewport = ViewportSize(21, 5)

  private def twoBufferState: AppState =
    val second = Buffer.fromString(BufferId(1), "second")
    val base   = AppState.initial
    base.copy(
      runtime = base.runtime.copy(viewportSize = Some(twoTabViewport)),
      persisted = base.persisted.copy(
        buffers = base.persisted.buffers + (second.id -> second),
        bufferOrder = base.persisted.bufferOrder :+ second.id
      )
    )

  "clickTarget" should "resolve a click on a tab other than the active one to Some(Some(bufferId))" in {
    TabBarMouseHitTesting.clickTarget(twoBufferState, col = 15, row = 0) shouldBe Some(Some(BufferId(1)))
  }

  it should "swallow a click on the already-active tab as a no-op" in {
    TabBarMouseHitTesting.clickTarget(twoBufferState, col = 3, row = 0) shouldBe Some(None)
  }

  it should "swallow a click on the gap between tabs" in {
    TabBarMouseHitTesting.clickTarget(twoBufferState, col = 11, row = 0) shouldBe Some(None)
  }

  it should "miss for a click outside the strip's row, leaving it for another mouse target" in {
    TabBarMouseHitTesting.clickTarget(twoBufferState, col = 3, row = 1) shouldBe None
  }

  it should "miss when there is no tab bar this frame (a single open buffer)" in {
    TabBarMouseHitTesting.clickTarget(AppState.initial, col = 3, row = 0) shouldBe None
  }

  it should "miss when the viewport size is not yet known" in {
    val noViewport = twoBufferState.copy(runtime = twoBufferState.runtime.copy(viewportSize = None))

    TabBarMouseHitTesting.clickTarget(noViewport, col = 3, row = 0) shouldBe None
  }

  "handleClick" should "switch the active pane to the clicked tab's buffer and keep it focused" in {
    val state = twoBufferState

    val updated = TabBarMouseHitTesting.handleClick(state, col = 15, row = 0).getOrElse(fail("expected a click hit"))

    updated.persisted.layout.editorPanes(PaneId(0)).bufferId shouldBe Some(BufferId(1))
    updated.focusedBufferId shouldBe Some(BufferId(1))
    updated.persisted.focus shouldBe Focus.EditorPane(PaneId(0))
    updated.persisted.bufferOrder shouldBe state.persisted.bufferOrder
  }

  it should "leave state completely unchanged when clicking the already-active tab" in {
    val state = twoBufferState

    TabBarMouseHitTesting.handleClick(state, col = 3, row = 0) shouldBe Some(state)
  }

  it should "return None for a click outside the strip" in {
    TabBarMouseHitTesting.handleClick(twoBufferState, col = 3, row = 1) shouldBe None
  }

  "closeHitAt" should "resolve a click on a tab's close affordance to its BufferId (issue #1078)" in {
    // Close regions sit at the rightmost 2 columns of each tab's own cell: [4,6), [12,14), [19,21) (see
    // TabBarSurfaceCompositionSpec's `closeAffordances` coverage).
    TabBarMouseHitTesting.closeHitAt(entries, rect, col = 5, row = 0) shouldBe Some(BufferId(0))
    TabBarMouseHitTesting.closeHitAt(entries, rect, col = 13, row = 0) shouldBe Some(BufferId(1))
    TabBarMouseHitTesting.closeHitAt(entries, rect, col = 20, row = 0) shouldBe Some(BufferId(2))
  }

  it should "resolve no close hit for a click inside a tab but outside its close affordance" in {
    TabBarMouseHitTesting.closeHitAt(entries, rect, col = 3, row = 0) shouldBe None
  }

  it should "resolve no close hit for a click outside the strip's row" in {
    TabBarMouseHitTesting.closeHitAt(entries, rect, col = 5, row = 1) shouldBe None
  }

  it should "resolve no close hit when there are no open buffers" in {
    TabBarMouseHitTesting.closeHitAt(Nil, rect, col = 5, row = 0) shouldBe None
  }
