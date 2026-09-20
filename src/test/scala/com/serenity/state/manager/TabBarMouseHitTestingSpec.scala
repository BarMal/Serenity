package com.serenity.state.manager

import com.serenity.state.models.{BufferId, TabListEntry}
import com.serenity.ui.layout.LayoutRect
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Coverage for `TabBarMouseHitTesting` (issue #1075: Foundation) -- resolving a click's cell coordinate to the
  * `BufferId` of the tab it landed on. Wiring an actual click into switch/close/reorder behaviour is #1077 onward, out
  * of scope here (see the object's own doc comment).
  */
class TabBarMouseHitTestingSpec extends AnyFlatSpec with Matchers:

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
