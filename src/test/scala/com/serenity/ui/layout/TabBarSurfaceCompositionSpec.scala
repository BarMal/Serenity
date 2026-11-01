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

  it should "size each tab to its content: a cell of padding, the title, the close box and a cell of padding" in {
    val entries = List(entry(0, "a"), entry(1, "bb"), entry(2, "ccc"))

    // Plenty of room (40 columns, 2 gaps of 2), so every tab is as wide as its own title plus 4: 5, 6 and 7.
    val allocations = TabBarSurfaceComposition.allocate(entries, availableWidth = 40)

    allocations.map(_.allocatedWidth) shouldBe List(5, 6, 7)
    allocations.map(_.displayTitle) shouldBe List("a", "bb", "ccc")
  }

  it should "count the dirty marker as part of the title when sizing a tab" in {
    val entries = List(entry(0, "ab"), entry(1, "ab", dirty = true))

    val allocations = TabBarSurfaceComposition.allocate(entries, availableWidth = 40)

    // "ab" is 2 cells, "ab ●" is 4: 6 and 8.
    allocations.map(_.allocatedWidth) shouldBe List(6, 8)
  }

  it should "cut a title that would make a tab wider than the maximum, with an ellipsis" in {
    val entries = List(entry(0, "x" * 40))

    val standard = TabBarSurfaceComposition.allocate(entries, availableWidth = 100)
    val narrower = TabBarSurfaceComposition.allocate(entries, availableWidth = 100, maxTabWidth = 12)

    // The default maximum is 28 cells, leaving 24 for the title: 21 letters and "...".
    standard.map(_.allocatedWidth) shouldBe List(28)
    standard.head.displayTitle shouldBe ("x" * 21) + "..."
    narrower.map(_.allocatedWidth) shouldBe List(12)
    narrower.head.displayTitle shouldBe "xxxxx..."
  }

  it should "shrink only the widest tabs, to a common width, once the tabs no longer fit" in {
    val entries = List(entry(0, "a" * 10), entry(1, "b" * 10), entry(2, "c"))

    // 30 columns less 2 gaps of 2 leaves 26 for tabs that want 14, 14 and 5. Cut to 10 they take 25; the spare cell
    // goes to the leftmost cut tab, so 11, 10 and 5. The short tab keeps its own width.
    val allocations = TabBarSurfaceComposition.allocate(entries, availableWidth = 30)

    allocations.map(_.allocatedWidth) shouldBe List(11, 10, 5)
    allocations.map(_.displayTitle) shouldBe List("aaaa...", "bbb...", "c")
  }

  it should "give every tab the title room it shows plus four cells of padding and close box" in {
    val entries = List(entry(0, "a" * 10), entry(1, "b" * 10), entry(2, "c"))

    val allocations = TabBarSurfaceComposition.allocate(entries, availableWidth = 30)

    allocations.foreach(allocation => allocation.allocatedWidth shouldBe allocation.displayTitle.length + 4)
  }

  it should "carry the active flag and dirty glyph through as the entry's own fields, not into the title" in {
    val entries = List(entry(0, "clean.txt"), entry(1, "dirty.txt", dirty = true))

    val allocations = TabBarSurfaceComposition.allocate(entries, availableWidth = 60)

    allocations.map(_.entry.isDirty) shouldBe List(false, true)
    allocations(1).displayTitle should include("dirty.txt")
    allocations(1).displayTitle should include("●")
    allocations(0).displayTitle shouldBe "clean.txt"
  }

  it should "truncate a title with an ellipsis once it no longer fits its allocated width" in {
    val entries = List(entry(0, "a-very-long-buffer-name.txt"), entry(1, "b"))

    // 16 columns, 1 gap of 2 -> 14. The short tab wants 5 and the long one 28, the most a tab may take, so it is cut
    // to 9. Four of those are padding and the close box, which leaves 5 cells for its title: "a-...".
    val allocations = TabBarSurfaceComposition.allocate(entries, availableWidth = 16)

    allocations.head.allocatedWidth shouldBe 9
    allocations.head.displayTitle shouldBe "a-..."
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

    // Paint box 1 is the trailing new-tab (+) affordance (issue #1080); boxes 2-4 are the three tabs' own close (x)
    // affordances (issue #1078) -- see their own coverage below.
    resolved.paintBoxes should have size 5
    val box = resolved.paintBoxes.head
    box.layout shouldBe SurfacePaintLayout.Distributed
    box.segments should have size 3
    box.segments.map(_.allocatedWidth.isDefined) shouldBe List(true, true, true)
    box.segments.map(_.trailingSeparator) shouldBe List(true, true, false)
  }

  it should "paint the trailing new-tab (+) affordance as its own plain-text box, addressed by NewTabFocusId" in {
    val entries  = List(entry(0, "one"), entry(1, "two"), entry(2, "three"))
    val resolved = TabBarSurfaceComposition.forTabBar(entries, Some(BufferId(1)), LayoutRect(0, 0, 30, 1))

    val newTabBox = resolved.paintBoxes(1)
    newTabBox.text shouldBe Some(" +")
    newTabBox.focusId shouldBe Some(TabBarSurfaceComposition.NewTabFocusId)
  }

  it should
    "paint each tab's close (x) affordance as its own plain-text box, positioned at its own close hit region" in {
      val entries  = List(entry(0, "one"), entry(1, "two"), entry(2, "three"))
      val rect     = LayoutRect(0, 0, 40, 1)
      val resolved = TabBarSurfaceComposition.forTabBar(entries, None, rect)
      val closes   = TabBarSurfaceComposition.closeAffordances(entries, None, rect)

      // Paint boxes: [0] the tabs' own Distributed row, [1] the new-tab (+) affordance, [2..] one close-glyph box
      // per tab in entry order -- derived from the exact same `closeAffordances` regions used for hit-testing, so
      // painted glyph and clickable region can never drift apart.
      val closeBoxes = resolved.paintBoxes.drop(2)
      closeBoxes should have size closes.size
      closeBoxes.map(_.rect) shouldBe closes.map(_.rect)
      closeBoxes.map(_.focusId) shouldBe closes.map(region => Some(region.focusId))
      closeBoxes.map(_.actionId) shouldBe closes.map(_.actionId)
      closeBoxes.foreach(_.text shouldBe Some(" x"))
    }

  it should "paint the active tab's close box as selected, so it sits inside the tab's pill, and no other" in {
    val entries  = List(entry(0, "one"), entry(1, "two"), entry(2, "three"))
    val rect     = LayoutRect(0, 0, 40, 1)
    val active   = TabBarSurfaceComposition.forTabBar(entries, Some(BufferId(1)), rect).paintBoxes.drop(2)
    val inactive = TabBarSurfaceComposition.forTabBar(entries, None, rect).paintBoxes.drop(2)

    active.map(_.selected) shouldBe List(false, true, false)
    inactive.map(_.selected) shouldBe List(false, false, false)
    active.map(_.rect) shouldBe inactive.map(_.rect)
  }

  it should "pad each tab's title so that it sits one cell in from the tab's left edge, clear of the close box" in {
    val entries  = List(entry(0, "one"), entry(1, "three"))
    val resolved = TabBarSurfaceComposition.forTabBar(entries, None, LayoutRect(0, 0, 40, 1))

    // The row centres each segment's text in its cell. Two trailing spaces make that a cell in from the left edge:
    // a cell of title padding on the left, and the close box's blank and `x` and a cell of padding on the right.
    resolved.paintBoxes.head.segments.map(_.text) shouldBe List("one  ", "three  ")
  }

  it should "produce no close-glyph paint boxes for a tab too narrow to leave room for one" in {
    val entries  = List.tabulate(5)(i => entry(i, s"tab-$i"))
    val rect     = LayoutRect(0, 0, 3, 1)
    val resolved = TabBarSurfaceComposition.forTabBar(entries, None, rect)

    resolved.paintBoxes.drop(2) shouldBe Nil
  }

  it should "mark exactly the active buffer's segment as selected" in {
    val entries  = List(entry(0, "one"), entry(1, "two"))
    val resolved = TabBarSurfaceComposition.forTabBar(entries, Some(BufferId(1)), LayoutRect(0, 0, 20, 1))

    resolved.paintBoxes.head.segments.map(_.selected) shouldBe List(false, true)
  }

  it should "produce one hit region per tab, each addressed by that tab's own BufferId" in {
    val entries  = List(entry(0, "one"), entry(1, "two"), entry(2, "three"))
    val resolved = TabBarSurfaceComposition.forTabBar(entries, None, LayoutRect(0, 0, 40, 1))

    // Each tab is as wide as its title plus 4: 7, 7 and 9. Each starts two columns past the previous tab's cell (one
    // for its separator glyph, one for the blank column after it): 0, then 0+7+2=9, then 9+7+2=18.
    resolved.hitRegions.map(_.focusId) shouldBe entries.map(e => TabBarSurfaceComposition.focusId(e.bufferId))
    resolved.hitRegions.map(_.rect.width) shouldBe List(7.0, 7.0, 9.0)
    resolved.hitRegions.map(_.rect.x) shouldBe List(0.0, 9.0, 18.0)
  }

  it should "resolve an absolute on-screen rect's offset into the hit regions' x positions" in {
    val entries  = List(entry(0, "one"), entry(1, "two"))
    val resolved = TabBarSurfaceComposition.forTabBar(entries, None, LayoutRect(5, 2, 40, 1))

    // Both tabs are 7 wide; the strip itself starts at x=5, so the second tab starts at 5+7+2=14.
    resolved.hitRegions.map(_.rect.x) shouldBe List(5.0, 14.0)
    resolved.hitRegions.foreach(_.rect.y shouldBe 2.0)
  }

  it should "produce no paint boxes or hit regions for an empty tab list" in {
    val resolved = TabBarSurfaceComposition.forTabBar(Nil, None, LayoutRect(0, 0, 30, 1))

    resolved.paintBoxes shouldBe Nil
    resolved.hitRegions shouldBe Nil
  }

  "closeAffordances" should "produce no regions for an empty tab list" in {
    TabBarSurfaceComposition.closeAffordances(Nil, None, LayoutRect(0, 0, 30, 1)) shouldBe Nil
  }

  it should
    "produce one close hit region per wide-enough tab, addressed by closeFocusId, before its right padding" in {
      val entries = List(entry(0, "one"), entry(1, "two"), entry(2, "three"))
      // Widths 7/7/9, starting at 0, 9 and 18 (see `forTabBar`'s hit-region spec above).
      val regions = TabBarSurfaceComposition.closeAffordances(entries, None, LayoutRect(0, 0, 40, 1))

      regions.map(_.focusId) shouldBe entries.map(e => TabBarSurfaceComposition.closeFocusId(e.bufferId))
      // Each region is the 2 columns before the cell's last column, which is padding: [4,6), [13,15), [24,26).
      regions.map(_.rect.x) shouldBe List(4.0, 13.0, 24.0)
      regions.foreach(_.rect.width shouldBe 2.0)
    }

  it should "resolve an absolute on-screen rect's offset into the close regions' x positions" in {
    val entries = List(entry(0, "one"), entry(1, "two"))
    val regions = TabBarSurfaceComposition.closeAffordances(entries, None, LayoutRect(5, 2, 40, 1))

    // Both tabs are 7 wide (see `forTabBar`'s own offset spec above); the strip starts at x=5, so the first tab's
    // close region is [5+7-3,...)=[9,11) and the second, which starts at 14, is [14+7-3,...)=[18,20).
    regions.map(_.rect.x) shouldBe List(9.0, 18.0)
    regions.foreach(_.rect.y shouldBe 2.0)
  }

  it should "produce no close region for a tab too narrow to leave room for one" in {
    val entries = List.tabulate(5)(i => entry(i, s"tab-$i"))

    val regions = TabBarSurfaceComposition.closeAffordances(entries, None, LayoutRect(0, 0, 3, 1))

    regions shouldBe Nil
  }

  it should "not touch forTabBar's own hit regions -- switch (#1077) and close (#1078) resolve from disjoint sets" in {
    val entries  = List(entry(0, "one"), entry(1, "two"), entry(2, "three"))
    val resolved = TabBarSurfaceComposition.forTabBar(entries, None, LayoutRect(0, 0, 40, 1))

    resolved.hitRegions.map(_.focusId) shouldBe entries.map(e => TabBarSurfaceComposition.focusId(e.bufferId))
  }

  "newTabAffordance" should "produce no region for an empty tab list" in {
    TabBarSurfaceComposition.newTabAffordance(Nil, LayoutRect(0, 0, 30, 1)) shouldBe None
  }

  it should "produce a 2-column hit region straight after the last tab, addressed by NewTabFocusId" in {
    val entries = List(entry(0, "one"), entry(1, "two"), entry(2, "three"))

    val region = TabBarSurfaceComposition
      .newTabAffordance(entries, LayoutRect(0, 0, 40, 1))
      .getOrElse(fail("expected a new-tab affordance region"))

    // The tabs end at 18+9=27; the affordance's own blank column is the gap before its `+`.
    region.focusId shouldBe TabBarSurfaceComposition.NewTabFocusId
    region.rect.x shouldBe 27.0
    region.rect.width shouldBe 2.0
  }

  it should "sit flush against the strip's right edge once the tabs overflow the strip" in {
    val entries = List(entry(0, "one"), entry(1, "two"), entry(2, "three"))

    val region = TabBarSurfaceComposition
      .newTabAffordance(entries, LayoutRect(0, 0, 21, 1))
      .getOrElse(fail("expected a new-tab affordance region"))

    region.rect.x shouldBe 19.0
    region.rect.width shouldBe 2.0
  }

  it should "sit flush against the strip's right edge when shrunk tabs fill the strip exactly" in {
    val entries = List(entry(0, "a" * 10), entry(1, "b" * 10), entry(2, "c"))

    val region = TabBarSurfaceComposition
      .newTabAffordance(entries, LayoutRect(0, 0, 32, 1))
      .getOrElse(fail("expected a new-tab affordance region"))

    // 32 less 2 for the affordance is the 30 the tabs shrank to fill (11 + 2 + 10 + 2 + 5).
    region.rect.x shouldBe 30.0
  }

  it should "resolve an absolute on-screen rect's offset into its own x position" in {
    val entries = List(entry(0, "one"), entry(1, "two"))

    val region = TabBarSurfaceComposition
      .newTabAffordance(entries, LayoutRect(5, 2, 40, 1))
      .getOrElse(fail("expected a new-tab affordance region"))

    // Tabs of 7 and 7 from x=5 with a gap of 2: they end at 5+7+2+7=21.
    region.rect.x shouldBe 21.0
    region.rect.y shouldBe 2.0
  }

  it should "not touch forTabBar's own hit regions or closeAffordances -- three disjoint region sets (issue #1080)" in {
    val entries  = List(entry(0, "one"), entry(1, "two"), entry(2, "three"))
    val rect     = LayoutRect(0, 0, 40, 1)
    val resolved = TabBarSurfaceComposition.forTabBar(entries, None, rect)
    val closes   = TabBarSurfaceComposition.closeAffordances(entries, None, rect)

    val newTabFocusId = TabBarSurfaceComposition.NewTabFocusId
    resolved.hitRegions.map(_.focusId) should not contain newTabFocusId
    closes.map(_.focusId) should not contain newTabFocusId
  }
