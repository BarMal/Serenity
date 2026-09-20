package com.serenity.ui.layout

import com.serenity.state.models.{BufferId, TabListEntry}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Coverage for `TabBarSurfaceComposition`'s overflow handling (issue #1081) -- what happens once more tabs are open
  * than the strip can show even at a minimum legible width. Kept apart from `TabBarSurfaceCompositionSpec` (the
  * per-repo convention for a new, cohesive test scenario, rather than growing that file towards the 600-line
  * architecture ratchet).
  */
class TabBarSurfaceCompositionOverflowSpec extends AnyFlatSpec with Matchers:

  private def entry(id: Int, title: String): TabListEntry = TabListEntry(BufferId(id), title, isDirty = false)

  private def entries(count: Int): List[TabListEntry] = List.tabulate(count)(i => entry(i, s"tab-$i"))

  "visibleWindow" should "show every tab with no hidden count when they all fit at the minimum width" in {
    val window = TabBarSurfaceComposition.visibleWindow(entries(3), None, availableWidth = 40)

    window.visible shouldBe entries(3)
    window.hiddenCount shouldBe 0
  }

  it should "hide tabs once the count exceeds what the minimum width allows, reporting how many are hidden" in {
    // Only a handful of tabs can fit at MinTabWidth=4 (plus 2-column gaps) in 24 columns; far fewer than 20 tabs.
    val window = TabBarSurfaceComposition.visibleWindow(entries(20), None, availableWidth = 24)

    window.visible.size should be < 20
    window.hiddenCount shouldBe (20 - window.visible.size)
    window.visible.size should be > 0
  }

  it should "always keep the active tab inside the visible window" in {
    val all = entries(20)

    all.indices.foreach { activeIndex =>
      val active = Some(all(activeIndex).bufferId)
      val window = TabBarSurfaceComposition.visibleWindow(all, active, availableWidth = 24)

      window.visible.map(_.bufferId) should contain(all(activeIndex).bufferId)
    }
  }

  it should "keep the visible tabs in their original relative order" in {
    val all    = entries(20)
    val window = TabBarSurfaceComposition.visibleWindow(all, Some(all(15).bufferId), availableWidth = 24)

    val indices = window.visible.map(v => all.indexOf(v))
    indices shouldBe indices.sorted
  }

  it should "default to keeping the first tab visible when no tab is active" in {
    val all    = entries(20)
    val window = TabBarSurfaceComposition.visibleWindow(all, None, availableWidth = 24)

    window.visible.headOption shouldBe Some(all.head)
  }

  it should "treat an active buffer id that is not in the entry list the same as no active tab" in {
    val all     = entries(20)
    val window1 = TabBarSurfaceComposition.visibleWindow(all, None, availableWidth = 24)
    val window2 = TabBarSurfaceComposition.visibleWindow(all, Some(BufferId(999)), availableWidth = 24)

    window2.visible shouldBe window1.visible
  }

  it should "never report a hidden count larger than the full tab list" in {
    val window = TabBarSurfaceComposition.visibleWindow(entries(50), None, availableWidth = 24)

    window.hiddenCount should be < 50
    (window.visible.size + window.hiddenCount) shouldBe 50
  }

  "forTabBar" should "keep every painted/hit-tested tab's rect within the strip's own rect under overflow" in {
    val all      = entries(30)
    val rect     = LayoutRect(0, 0, 40, 1)
    val resolved = TabBarSurfaceComposition.forTabBar(all, Some(all(10).bufferId), rect)

    resolved.hitRegions.foreach { region =>
      (region.rect.x >= rect.x) shouldBe true
      (region.rect.x + region.rect.width <= rect.x + rect.width) shouldBe true
    }
  }

  it should "keep the active tab's hit region present under overflow" in {
    val all      = entries(30)
    val rect     = LayoutRect(0, 0, 40, 1)
    val active   = all(10).bufferId
    val resolved = TabBarSurfaceComposition.forTabBar(all, Some(active), rect)

    resolved.hitRegions.map(_.focusId) should contain(TabBarSurfaceComposition.focusId(active))
  }

  it should "produce fewer hit regions than open tabs once the strip overflows" in {
    val all      = entries(30)
    val rect     = LayoutRect(0, 0, 40, 1)
    val resolved = TabBarSurfaceComposition.forTabBar(all, Some(all(10).bufferId), rect)

    resolved.hitRegions.size should be < all.size
  }

  it should "not produce a hit region for a scrolled-out tab" in {
    val all      = entries(30)
    val rect     = LayoutRect(0, 0, 40, 1)
    val active   = all(0).bufferId // window centers on the first tab, pushing the last tabs out of view
    val resolved = TabBarSurfaceComposition.forTabBar(all, Some(active), rect)

    resolved.hitRegions.map(_.focusId) should not contain TabBarSurfaceComposition.focusId(all.last.bufferId)
  }

  it should "append a non-interactive overflow indicator segment once tabs are hidden" in {
    val all      = entries(30)
    val rect     = LayoutRect(0, 0, 40, 1)
    val resolved = TabBarSurfaceComposition.forTabBar(all, Some(all(10).bufferId), rect)

    val box = resolved.paintBoxes.head
    box.segments.size shouldBe resolved.hitRegions.size + 1
    box.segments.last.text should startWith("+")
  }

  it should "add no overflow indicator segment when every tab already fits" in {
    val all      = entries(3)
    val rect     = LayoutRect(0, 0, 40, 1)
    val resolved = TabBarSurfaceComposition.forTabBar(all, None, rect)

    resolved.paintBoxes.head.segments.size shouldBe all.size
  }

  "closeAffordances" should "produce no close region for a tab scrolled out of view" in {
    val all     = entries(30)
    val rect    = LayoutRect(0, 0, 40, 1)
    val active  = all(0).bufferId
    val regions = TabBarSurfaceComposition.closeAffordances(all, Some(active), rect)

    regions.map(_.focusId) should not contain TabBarSurfaceComposition.closeFocusId(all.last.bufferId)
  }

  it should "keep every close region's rect within the strip's own rect under overflow" in {
    val all     = entries(30)
    val rect    = LayoutRect(0, 0, 40, 1)
    val regions = TabBarSurfaceComposition.closeAffordances(all, Some(all(10).bufferId), rect)

    regions.foreach { region =>
      (region.rect.x >= rect.x) shouldBe true
      (region.rect.x + region.rect.width <= rect.x + rect.width) shouldBe true
    }
  }
