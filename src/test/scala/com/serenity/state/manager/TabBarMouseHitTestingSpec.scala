package com.serenity.state.manager

import com.serenity.rope.Balance
import com.serenity.state.core.EditorState
import com.serenity.state.models.*
import com.serenity.ui.layout.{LayoutRect, ViewportSize}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Coverage for `TabBarMouseHitTesting` -- resolving a click's cell coordinate to the `BufferId` of the tab it landed
  * on (issue #1075: Foundation), resolving a click against live `AppState` into a buffer switch (issue #1077: "Click a
  * tab to switch buffer"), and resolving a click against the trailing new-tab (+) affordance into opening a new tab
  * (issue #1080: "New-tab affordance").
  */
class TabBarMouseHitTestingSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private def entry(id: Int, title: String): TabListEntry = TabListEntry(BufferId(id), title, isDirty = false)

  private val entries = List(entry(0, "one"), entry(1, "two"), entry(2, "three"))
  // Widths 5/5/5 (see TabBarSurfaceCompositionSpec -- 21 columns available, minus 2 reserved for the trailing
  // new-tab affordance, issue #1080): tab 0 at [0,5), gap [5,7), tab 1 at [7,12), gap [12,14), tab 2 at [14,19),
  // then the new-tab affordance at [19,21).
  private val rect = LayoutRect(0, 0, 21, 1)

  "hitAt" should "resolve a click inside the first tab to its BufferId" in {
    TabBarMouseHitTesting.hitAt(entries, None, rect, col = 3, row = 0) shouldBe Some(BufferId(0))
  }

  it should "resolve a click inside the second tab to its BufferId" in {
    TabBarMouseHitTesting.hitAt(entries, None, rect, col = 10, row = 0) shouldBe Some(BufferId(1))
  }

  it should "resolve a click inside the third tab to its BufferId" in {
    TabBarMouseHitTesting.hitAt(entries, None, rect, col = 18, row = 0) shouldBe Some(BufferId(2))
  }

  it should "resolve no tab for a click on the separator glyph between tabs" in {
    TabBarMouseHitTesting.hitAt(entries, None, rect, col = 6, row = 0) shouldBe None
  }

  it should "resolve no tab for a click outside the strip's row" in {
    TabBarMouseHitTesting.hitAt(entries, None, rect, col = 3, row = 1) shouldBe None
  }

  it should "resolve no tab when there are no open buffers" in {
    TabBarMouseHitTesting.hitAt(Nil, None, rect, col = 3, row = 0) shouldBe None
  }

  // Widths 9/8 (21 columns available, minus 2 reserved for the trailing new-tab affordance -> 19 columns for tabs,
  // minus 1 gap reserving 2 columns -> 17 content columns, issue #1080): tab 0 (BufferId(0), the pane's
  // already-active buffer) at [0,9), gap [9,11), tab 1 (BufferId(1)) at [11,19), then the new-tab affordance at
  // [19,21).
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
    TabBarMouseHitTesting.clickTarget(twoBufferState, col = 10, row = 0) shouldBe Some(None)
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
    // Close regions sit at the rightmost 2 columns of each tab's own cell: [3,5), [10,12), [17,19) (see
    // TabBarSurfaceCompositionSpec's `closeAffordances` coverage).
    TabBarMouseHitTesting.closeHitAt(entries, rect, col = 4, row = 0) shouldBe Some(BufferId(0))
    TabBarMouseHitTesting.closeHitAt(entries, rect, col = 11, row = 0) shouldBe Some(BufferId(1))
    TabBarMouseHitTesting.closeHitAt(entries, rect, col = 18, row = 0) shouldBe Some(BufferId(2))
  }

  it should "resolve no close hit for a click inside a tab but outside its close affordance" in {
    TabBarMouseHitTesting.closeHitAt(entries, rect, col = 1, row = 0) shouldBe None
  }

  it should "resolve no close hit for a click outside the strip's row" in {
    TabBarMouseHitTesting.closeHitAt(entries, rect, col = 5, row = 1) shouldBe None
  }

  it should "resolve no close hit when there are no open buffers" in {
    TabBarMouseHitTesting.closeHitAt(Nil, rect, col = 5, row = 0) shouldBe None
  }

  // Same state as `twoBufferState`, but with `nextBufferId` corrected to `BufferId(2)` -- `twoBufferState` adds its
  // second buffer directly at `BufferId(1)` without advancing `nextBufferId` past it (fine for the switch/close
  // coverage above, which never allocates a new buffer id), but `EditorState.openNewTab` needs a fixture where the
  // next id it hands out doesn't collide with an already-open buffer.
  private def twoBufferStateForNewTab: AppState =
    twoBufferState.copy(runtime = twoBufferState.runtime.copy(nextBufferId = BufferId(2)))

  "newTabClickTarget" should "resolve true for a click on the trailing new-tab (+) affordance (issue #1080)" in {
    TabBarMouseHitTesting.newTabClickTarget(twoBufferStateForNewTab, col = 20, row = 0) shouldBe true
  }

  it should "resolve false for a click on a tab" in {
    TabBarMouseHitTesting.newTabClickTarget(twoBufferStateForNewTab, col = 3, row = 0) shouldBe false
  }

  it should "resolve false for a click outside the strip's row" in {
    TabBarMouseHitTesting.newTabClickTarget(twoBufferStateForNewTab, col = 20, row = 1) shouldBe false
  }

  it should "resolve false when there is no tab bar this frame (a single open buffer)" in {
    TabBarMouseHitTesting.newTabClickTarget(AppState.initial, col = 20, row = 0) shouldBe false
  }

  "handleNewTabClick" should
    "open a new tab identically to Ctrl+T and focus it when the affordance is clicked (issue #1080)" in {
      val state = twoBufferStateForNewTab

      val updated =
        TabBarMouseHitTesting.handleNewTabClick(state, col = 20, row = 0).getOrElse(fail("expected a click hit"))

      updated.persisted.buffers should have size 3
      updated.persisted.bufferOrder should contain(BufferId(2))
      updated.focusedBufferId shouldBe Some(BufferId(2))
      // Mouse and keyboard (Ctrl+T) share EditorState.openNewTab, so the two produce identical resulting state.
      updated shouldBe EditorState.openNewTab(state)
    }

  it should "return None for a click that misses the affordance" in {
    TabBarMouseHitTesting.handleNewTabClick(twoBufferStateForNewTab, col = 3, row = 0) shouldBe None
  }
