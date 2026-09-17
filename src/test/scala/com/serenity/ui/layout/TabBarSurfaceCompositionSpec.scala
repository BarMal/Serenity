package com.serenity.ui.layout

import com.serenity.state.models.{BufferId, TabListEntry}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Coverage for `TabBarSurfaceComposition` (issue #1075: Foundation) -- the always-visible tab strip's width
  * allocation, truncation, and its mapping into a `ResolvedSurfaceComposition`. Painting is covered separately in
  * `TextOverlayRendererLayoutSpec` (issue #1076: Render), and per-tab hit-testing in `TabBarMouseHitTestingSpec`.
  */
class TabBarSurfaceCompositionSpec extends AnyFlatSpec with Matchers:

  private def entry(id: Int, title: String, dirty: Boolean = false): TabListEntry =
    TabListEntry(BufferId(id), title, isDirty = dirty)

  "allocate" should "return no allocations for an empty tab list" in {
    TabBarSurfaceComposition.allocate(Nil, availableWidth = 40) shouldBe Nil
  }

  it should "split the available width evenly across tabs, reserving two columns per inter-tab gap" in {
    val entries = List(entry(0, "a"), entry(1, "b"), entry(2, "c"))

    // 21 columns available, 2 gaps reserving 2 columns each -> 17 columns of tab content: base 5 with 2 remainder
    // columns going to the first two tabs, so 6/6/5.
    val allocations = TabBarSurfaceComposition.allocate(entries, availableWidth = 21)

    allocations.map(_.allocatedWidth) shouldBe List(6, 6, 5)
    allocations.map(_.allocatedWidth).sum shouldBe 17
  }

  it should "carry the active flag and dirty glyph through as the entry's own fields, not into the title" in {
    val entries = List(entry(0, "clean.txt"), entry(1, "dirty.txt", dirty = true))

    val allocations = TabBarSurfaceComposition.allocate(entries, availableWidth = 40)

    allocations.map(_.entry.isDirty) shouldBe List(false, true)
    allocations(1).displayTitle should include("dirty.txt")
    allocations(1).displayTitle should include("●")
    allocations(0).displayTitle shouldBe "clean.txt"
  }

  it should "truncate a title with an ellipsis once it no longer fits its allocated width" in {
    val entries = List(entry(0, "a-very-long-buffer-name.txt"), entry(1, "b"))

    // 12 columns available, 1 gap reserving 2 columns -> 10 columns of content, 5/5.
    val allocations = TabBarSurfaceComposition.allocate(entries, availableWidth = 12)

    allocations.head.allocatedWidth shouldBe 5
    allocations.head.displayTitle shouldBe "a-..."
    allocations.head.displayTitle.length shouldBe 5
  }

  it should "never allocate less than one column to a tab even when width is exhausted" in {
    val entries = List.tabulate(5)(i => entry(i, s"tab-$i"))

    val allocations = TabBarSurfaceComposition.allocate(entries, availableWidth = 3)

    allocations should have size 5
    allocations.foreach(_.allocatedWidth should be >= 1)
  }

  "forTabBar" should "map each tab to one segment of a single Distributed paint box" in {
    val entries  = List(entry(0, "one"), entry(1, "two"), entry(2, "three"))
    val resolved = TabBarSurfaceComposition.forTabBar(entries, Some(BufferId(1)), LayoutRect(0, 0, 30, 1))

    resolved.paintBoxes should have size 1
    val box = resolved.paintBoxes.head
    box.layout shouldBe SurfacePaintLayout.Distributed
    box.segments should have size 3
    box.segments.map(_.allocatedWidth.isDefined) shouldBe List(true, true, true)
    box.segments.map(_.trailingSeparator) shouldBe List(true, true, false)
  }

  it should "mark exactly the active buffer's segment as selected" in {
    val entries  = List(entry(0, "one"), entry(1, "two"))
    val resolved = TabBarSurfaceComposition.forTabBar(entries, Some(BufferId(1)), LayoutRect(0, 0, 20, 1))

    resolved.paintBoxes.head.segments.map(_.selected) shouldBe List(false, true)
  }

  it should "produce one hit region per tab, each addressed by that tab's own BufferId" in {
    val entries  = List(entry(0, "one"), entry(1, "two"), entry(2, "three"))
    val resolved = TabBarSurfaceComposition.forTabBar(entries, None, LayoutRect(0, 0, 21, 1))

    // Widths 6/6/5 (see the `allocate` spec above); each tab starts two columns past the previous tab's cell (one
    // for its separator glyph, one for the blank column after it): 0, then 0+6+2=8, then 8+6+2=16.
    resolved.hitRegions.map(_.focusId) shouldBe entries.map(e => TabBarSurfaceComposition.focusId(e.bufferId))
    resolved.hitRegions.map(_.rect.width) shouldBe List(6.0, 6.0, 5.0)
    resolved.hitRegions.map(_.rect.x) shouldBe List(0.0, 8.0, 16.0)
  }

  it should "resolve an absolute on-screen rect's offset into the hit regions' x positions" in {
    val entries  = List(entry(0, "one"), entry(1, "two"))
    val resolved = TabBarSurfaceComposition.forTabBar(entries, None, LayoutRect(5, 2, 20, 1))

    // 18 content columns split 9/9; the strip itself starts at x=5, so the second tab starts at 5+9+2=16.
    resolved.hitRegions.map(_.rect.x) shouldBe List(5.0, 16.0)
    resolved.hitRegions.foreach(_.rect.y shouldBe 2.0)
  }

  it should "produce no paint boxes or hit regions for an empty tab list" in {
    val resolved = TabBarSurfaceComposition.forTabBar(Nil, None, LayoutRect(0, 0, 30, 1))

    resolved.paintBoxes shouldBe Nil
    resolved.hitRegions shouldBe Nil
  }
