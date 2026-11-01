package com.serenity.ui.layout

import com.serenity.state.models.{BufferId, TabListEntry}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Coverage for `TabBarSurfaceComposition` (issue #1075: Foundation) -- the always-visible tab strip's width
  * allocation, truncation, and its mapping into a `ResolvedSurfaceComposition`. Painting is covered separately in
  * `TextOverlayRendererLayoutSpec` (issue #1076: Render), and per-tab hit-testing in `TabBarMouseHitTestingSpec`.
  *
  * A tab is `pad + title + pad` (2 cells of chrome) and its close box shows only on the active tab, which adds a blank
  * and the `x` (4 cells of chrome, 3 once it is squeezed); an inactive tab's `x` is drawn in its right padding cell
  * when asked for. The default measure is a cell grid's: a title is as many cells as it has characters, and a cut ends
  * in "...", with 8 title cells as the floor.
  */
class TabBarSurfaceCompositionSpec extends AnyFlatSpec with Matchers:

  private def entry(id: Int, title: String, dirty: Boolean = false): TabListEntry =
    TabListEntry(BufferId(id), title, isDirty = dirty)

  /** A proportional font's measure, in miniature: M, W, m and w take two cells, everything else one. */
  private val wide = TabTitleMeasure(_.map(c => if "MWmw".contains(c) then 2 else 1).sum, "…", 6)

  private def settings(
    maxTabWidth: Int = 28,
    measure: TabTitleMeasure = TabTitleMeasure.CellGrid,
    reveal: CloseReveal = CloseReveal.ActiveOnly
  ): TabBarSettings = TabBarSettings(maxTabWidth, measure, reveal)

  private def allocate(
    entries: List[TabListEntry],
    availableWidth: Int,
    active: Option[Int] = None,
    withSettings: TabBarSettings = settings()
  ) =
    TabBarSurfaceComposition.allocate(entries, availableWidth, active.map(BufferId.apply), withSettings)

  "allocate" should "return no allocations for an empty tab list" in {
    allocate(Nil, availableWidth = 40) shouldBe Nil
  }

  it should "size an inactive tab to its content: a cell of padding either side of the title" in {
    val entries = List(entry(0, "a"), entry(1, "bb"), entry(2, "ccc"))

    val allocations = allocate(entries, availableWidth = 40)

    allocations.map(_.allocatedWidth) shouldBe List(3, 4, 5)
    allocations.map(_.displayTitle) shouldBe List("a", "bb", "ccc")
  }

  it should "give the active tab room for its close box: two more cells than an inactive tab" in {
    val entries = List(entry(0, "one"), entry(1, "two"), entry(2, "six"))

    val allocations = allocate(entries, availableWidth = 40, active = Some(1))

    allocations.map(_.allocatedWidth) shouldBe List(5, 7, 5)
    allocations.map(_.active) shouldBe List(false, true, false)
  }

  it should "count the dirty marker as part of the title when sizing a tab" in {
    val entries = List(entry(0, "ab"), entry(1, "ab", dirty = true))

    allocate(entries, availableWidth = 40).map(_.allocatedWidth) shouldBe List(4, 6)
  }

  it should "size a tab from the measure, so wide letters take the room they need" in {
    val entries = List(entry(0, "WWmw"), entry(1, "abcd"))

    val allocations = allocate(entries, availableWidth = 60, withSettings = settings(measure = wide))

    // "WWmw" measures 8 cells, "abcd" 4; each adds 2 cells of padding.
    allocations.map(_.allocatedWidth) shouldBe List(10, 6)
    allocations.map(_.displayTitle) shouldBe List("WWmw", "abcd")
  }

  it should "cut a title that would make a tab wider than the maximum, with the measure's ellipsis" in {
    val entries = List(entry(0, "x" * 40))

    val standard = allocate(entries, availableWidth = 100)
    val narrower = allocate(entries, availableWidth = 100, withSettings = settings(maxTabWidth = 12))
    val wideCut  = allocate(entries, availableWidth = 100, withSettings = settings(measure = wide))

    // The default maximum is 28 cells, leaving 26 for the title: 23 letters and "...".
    standard.map(_.allocatedWidth) shouldBe List(28)
    standard.head.displayTitle shouldBe ("x" * 23) + "..."
    narrower.map(_.allocatedWidth) shouldBe List(12)
    narrower.head.displayTitle shouldBe "xxxxxxx..."
    // With a one-cell ellipsis the same 26 cells hold 25 letters and "…".
    wideCut.head.displayTitle shouldBe ("x" * 25) + "…"
  }

  it should "shrink only the widest tabs, to a common width, once the tabs no longer fit" in {
    val entries = List(entry(0, "a" * 10), entry(1, "b" * 10), entry(2, "c"))

    // 30 columns less 2 gaps of 2 leaves 26 for tabs that want 12, 12 and 3. Cut to 11 they take 25; the spare cell
    // goes to the leftmost cut tab, so 12, 11 and 3. The short tab keeps its own width.
    val allocations = allocate(entries, availableWidth = 30)

    allocations.map(_.allocatedWidth) shouldBe List(12, 11, 3)
    allocations.map(_.displayTitle) shouldBe List("a" * 10, "bbbbbb...", "c")
  }

  it should "squeeze the active tab's close box before cutting any title" in {
    val entries = List(entry(0, "a" * 10), entry(1, "b" * 10))

    // 27 columns less one gap leaves 25 for tabs that want 14 (active) and 12: too many. With the blank before the `x`
    // dropped the active tab wants 13, and 13 + 12 fits, so no title is cut.
    val allocations = allocate(entries, availableWidth = 27, active = Some(0))

    allocations.map(_.allocatedWidth) shouldBe List(13, 12)
    allocations.map(_.snug) shouldBe List(true, true)
    allocations.map(_.displayTitle) shouldBe List("a" * 10, "b" * 10)
  }

  it should "leave the close box roomy when everything fits without squeezing it" in {
    val entries = List(entry(0, "a" * 10), entry(1, "b" * 10))

    val allocations = allocate(entries, availableWidth = 40, active = Some(0))

    allocations.map(_.snug) shouldBe List(false, false)
    allocations.head.allocatedWidth shouldBe 14
  }

  it should "cut titles once squeezing the close box is not enough, never below the floor" in {
    val entries = List(entry(0, "a" * 10), entry(1, "b" * 10))

    // 23 less a gap leaves 21: the floor is 8 title cells, so an active tab of 3 + 8 = 11 and an inactive of 2 + 8 = 10.
    // They take the 21 exactly.
    val allocations = allocate(entries, availableWidth = 23, active = Some(0))

    allocations.map(_.allocatedWidth) shouldBe List(11, 10)
    allocations.map(_.displayTitle) shouldBe List("aaaaa...", "bbbbb...")
  }

  it should "keep at least five letters and the ellipsis of a title when the measure is proportional" in {
    val entries = List.tabulate(4)(i => entry(i, "abcdefghij"))

    // Floor: 6 title cells + 2 of padding = 8 per tab; 4 tabs and 3 gaps of 2 take 38 exactly.
    val allocations = allocate(entries, availableWidth = 38, withSettings = settings(measure = wide))

    allocations.map(_.allocatedWidth) shouldBe List(8, 8, 8, 8)
    allocations.map(_.displayTitle) shouldBe List.fill(4)("abcde…")
  }

  it should "carry the dirty glyph into the title, not into the entry" in {
    val entries = List(entry(0, "clean.txt"), entry(1, "dirty.txt", dirty = true))

    val allocations = allocate(entries, availableWidth = 60)

    allocations.map(_.entry.isDirty) shouldBe List(false, true)
    allocations(1).displayTitle should include("dirty.txt")
    allocations(1).displayTitle should include("●")
    allocations(0).displayTitle shouldBe "clean.txt"
  }

  it should "never allocate less than one column to a tab even when width is exhausted" in {
    val entries = List.tabulate(5)(i => entry(i, s"tab-$i"))

    val allocations = allocate(entries, availableWidth = 3)

    allocations should have size 5
    allocations.foreach(_.allocatedWidth should be >= 1)
  }

  "visibleWindow" should "show every tab when each fits at its floor" in {
    val entries = List.tabulate(3)(i => entry(i, "title"))

    val window = TabBarSurfaceComposition.visibleWindow(entries, None, availableWidth = 40, settings())

    window.visible shouldBe entries
    window.hiddenCount shouldBe 0
  }

  it should "centre the window on the active tab and count the tabs it hides" in {
    val entries = List.tabulate(10)(i => entry(i, "title-" + i))

    // "title-N" is 7 cells, so an inactive tab's floor is 9 cells and the active tab's 10. 40 columns less the "+N"
    // marker and a gap leaves 34: the active tab (10), a neighbour on the left (21) and one on the right (32).
    val window = TabBarSurfaceComposition.visibleWindow(entries, Some(BufferId(5)), availableWidth = 40, settings())

    window.visible.map(_.bufferId.value) shouldBe List(4, 5, 6)
    window.hiddenCount shouldBe 7
  }

  it should "keep the active tab in view at the end of the list" in {
    val entries = List.tabulate(10)(i => entry(i, "title-" + i))

    val window = TabBarSurfaceComposition.visibleWindow(entries, Some(BufferId(9)), availableWidth = 40, settings())

    window.visible.map(_.bufferId.value) shouldBe List(7, 8, 9)
    window.hiddenCount shouldBe 7
  }

  it should "keep at least six title cells in every visible tab before it hides any" in {
    val entries = List.tabulate(24)(i => entry(i, "file-number-" + i))
    val rect    = LayoutRect(0, 0, 100, 1)

    val layout = TabBarSurfaceComposition.forTabBar(entries, Some(BufferId(11)), rect, settings(measure = wide))

    val widths = layout.hitRegions.map(_.rect.width.toInt)
    widths.foreach(_ should be >= 8)
    layout.hitRegions.map(_.focusId) should contain(TabBarSurfaceComposition.focusId(BufferId(11)))
    layout.paintBoxes.head.segments.last.text should startWith("+")
  }

  "forTabBar" should "map each tab to one segment of a single Distributed paint box" in {
    val entries  = List(entry(0, "one"), entry(1, "two"), entry(2, "three"))
    val resolved = TabBarSurfaceComposition.forTabBar(entries, Some(BufferId(1)), LayoutRect(0, 0, 30, 1))

    // Paint box 1 is the trailing new-tab (+) affordance (issue #1080); box 2 is the active tab's own close (x).
    resolved.paintBoxes should have size 3
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

  it should "paint only the active tab's close (x) box, as selected, so it sits inside the tab's pill" in {
    val entries  = List(entry(0, "one"), entry(1, "two"), entry(2, "three"))
    val rect     = LayoutRect(0, 0, 40, 1)
    val resolved = TabBarSurfaceComposition.forTabBar(entries, Some(BufferId(1)), rect)

    val closeBoxes = resolved.paintBoxes.drop(2)
    closeBoxes should have size 1
    closeBoxes.head.focusId shouldBe Some(TabBarSurfaceComposition.closeFocusId(BufferId(1)))
    closeBoxes.head.selected shouldBe true
    closeBoxes.head.text shouldBe Some(" x")
  }

  it should "paint no close box at all when no tab is active and none is revealed" in {
    val entries  = List(entry(0, "one"), entry(1, "two"))
    val resolved = TabBarSurfaceComposition.forTabBar(entries, None, LayoutRect(0, 0, 40, 1))

    resolved.paintBoxes.drop(2) shouldBe Nil
  }

  it should "paint a revealed inactive tab's x in its right padding cell, not selected" in {
    val entries = List(entry(0, "one"), entry(1, "two"))
    val rect    = LayoutRect(0, 0, 40, 1)
    val revealed = TabBarSurfaceComposition
      .forTabBar(entries, Some(BufferId(1)), rect, settings(reveal = CloseReveal.Hovered(BufferId(0))))
      .paintBoxes
      .drop(2)

    revealed.map(_.focusId) shouldBe List(
      Some(TabBarSurfaceComposition.closeFocusId(BufferId(0))),
      Some(TabBarSurfaceComposition.closeFocusId(BufferId(1)))
    )
    revealed.map(_.selected) shouldBe List(false, true)
    revealed.map(_.text) shouldBe List(Some("x"), Some(" x"))
    // The first tab is 5 wide at 0: its padding cell is column 4.
    revealed.head.rect.x shouldBe 4.0
    revealed.head.rect.width shouldBe 1.0
  }

  it should "derive every close box from closeAffordances, so a painted glyph and its click target cannot drift" in {
    val entries = List(entry(0, "one"), entry(1, "two"), entry(2, "three"))
    val rect    = LayoutRect(0, 0, 40, 1)
    val all     = settings(reveal = CloseReveal.All)
    val resolved =
      TabBarSurfaceComposition.forTabBar(entries, Some(BufferId(1)), rect, all)
    val closes = TabBarSurfaceComposition.closeAffordances(entries, Some(BufferId(1)), rect, all)

    val closeBoxes = resolved.paintBoxes.drop(2)
    closeBoxes.map(_.rect) shouldBe closes.map(_.rect)
    closeBoxes.map(_.focusId) shouldBe closes.map(region => Some(region.focusId))
    closeBoxes.map(_.actionId) shouldBe closes.map(_.actionId)
  }

  it should "start each tab's title one cell in from the tab's left edge and say how many cells it has" in {
    val entries  = List(entry(0, "one"), entry(1, "three"))
    val resolved = TabBarSurfaceComposition.forTabBar(entries, Some(BufferId(1)), LayoutRect(0, 0, 40, 1))

    val segments = resolved.paintBoxes.head.segments
    segments.map(_.text) shouldBe List("one", "three")
    // "one" is a 5-cell inactive tab, "three" a 9-cell active tab (4 cells of chrome); the title room is what is left.
    segments.map(_.titleFit.map(fit => (fit.insetCells, fit.cells))) shouldBe List(Some((1, 3)), Some((1, 5)))
    segments.flatMap(_.titleFit).map(_.ellipsis).distinct shouldBe List("...")
  }

  it should "mark exactly the active buffer's segment as selected" in {
    val entries  = List(entry(0, "one"), entry(1, "two"))
    val resolved = TabBarSurfaceComposition.forTabBar(entries, Some(BufferId(1)), LayoutRect(0, 0, 20, 1))

    resolved.paintBoxes.head.segments.map(_.selected) shouldBe List(false, true)
  }

  it should "produce one hit region per tab, each addressed by that tab's own BufferId" in {
    val entries  = List(entry(0, "one"), entry(1, "two"), entry(2, "three"))
    val resolved = TabBarSurfaceComposition.forTabBar(entries, None, LayoutRect(0, 0, 40, 1))

    // No tab is active, so each is its title plus 2: 5, 5 and 7. Each starts two columns past the previous tab's cell
    // (one for its separator glyph, one for the blank column after it): 0, then 0+5+2=7, then 7+5+2=14.
    resolved.hitRegions.map(_.focusId) shouldBe entries.map(e => TabBarSurfaceComposition.focusId(e.bufferId))
    resolved.hitRegions.map(_.rect.width) shouldBe List(5.0, 5.0, 7.0)
    resolved.hitRegions.map(_.rect.x) shouldBe List(0.0, 7.0, 14.0)
  }

  it should "lay the strip out the same whichever close boxes are revealed" in {
    val entries = List(entry(0, "one"), entry(1, "two"), entry(2, "three"))
    val rect    = LayoutRect(0, 0, 40, 1)
    val active  = Some(BufferId(1))

    val plain = TabBarSurfaceComposition.forTabBar(entries, active, rect, settings())
    val hover =
      TabBarSurfaceComposition.forTabBar(entries, active, rect, settings(reveal = CloseReveal.Hovered(BufferId(2))))
    val all = TabBarSurfaceComposition.forTabBar(entries, active, rect, settings(reveal = CloseReveal.All))

    hover.hitRegions shouldBe plain.hitRegions
    all.hitRegions shouldBe plain.hitRegions
    hover.paintBoxes.head.segments shouldBe plain.paintBoxes.head.segments
  }

  it should "resolve an absolute on-screen rect's offset into the hit regions' x positions" in {
    val entries  = List(entry(0, "one"), entry(1, "two"))
    val resolved = TabBarSurfaceComposition.forTabBar(entries, None, LayoutRect(5, 2, 40, 1))

    // Both tabs are 5 wide; the strip itself starts at x=5, so the second tab starts at 5+5+2=12.
    resolved.hitRegions.map(_.rect.x) shouldBe List(5.0, 12.0)
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

  it should "give only the active tab a region by default, the two cells before its right padding" in {
    val entries = List(entry(0, "one"), entry(1, "two"), entry(2, "three"))
    // Widths 5, 7 (active) and 7, starting at 0, 7 and 16.
    val regions = TabBarSurfaceComposition.closeAffordances(entries, Some(BufferId(1)), LayoutRect(0, 0, 40, 1))

    regions.map(_.focusId) shouldBe List(TabBarSurfaceComposition.closeFocusId(BufferId(1)))
    // The active tab is [7,14): its padding is column 13, so the region is the two columns before it: [11,13).
    regions.map(_.rect.x) shouldBe List(11.0)
    regions.map(_.rect.width) shouldBe List(2.0)
  }

  it should "shrink the active tab's region to the `x` alone once its close box is squeezed" in {
    val entries = List(entry(0, "a" * 10), entry(1, "b" * 10))

    val regions = TabBarSurfaceComposition.closeAffordances(entries, Some(BufferId(0)), LayoutRect(0, 0, 29, 1))

    // 29 less 2 for the new-tab affordance leaves 27, less a gap 25: the active tab squeezes to 13 and sits at [0,13),
    // so its padding is column 12 and its `x` column 11.
    regions should have size 1
    regions.head.rect.x shouldBe 11.0
    regions.head.rect.width shouldBe 1.0
  }

  it should "add a one-cell region, in the right padding, for the hovered inactive tab" in {
    val entries = List(entry(0, "one"), entry(1, "two"))
    val rect    = LayoutRect(0, 0, 40, 1)

    val regions = TabBarSurfaceComposition.closeAffordances(
      entries,
      Some(BufferId(1)),
      rect,
      settings(reveal = CloseReveal.Hovered(BufferId(0)))
    )

    regions.map(_.focusId) shouldBe List(
      TabBarSurfaceComposition.closeFocusId(BufferId(0)),
      TabBarSurfaceComposition.closeFocusId(BufferId(1))
    )
    // The first tab is [0,5): its padding cell is column 4.
    regions.head.rect.x shouldBe 4.0
    regions.head.rect.width shouldBe 1.0
  }

  it should "give every tab a region when asked for all, as the accessibility tree does" in {
    val entries = List(entry(0, "one"), entry(1, "two"), entry(2, "three"))

    val regions = TabBarSurfaceComposition.closeAffordances(
      entries,
      Some(BufferId(1)),
      LayoutRect(0, 0, 40, 1),
      settings(reveal = CloseReveal.All)
    )

    regions.map(_.focusId) shouldBe entries.map(e => TabBarSurfaceComposition.closeFocusId(e.bufferId))
  }

  it should "give no region to an inactive tab nobody is hovering, or to any tab when none is active" in {
    val entries = List(entry(0, "one"), entry(1, "two"))
    val rect    = LayoutRect(0, 0, 40, 1)

    TabBarSurfaceComposition.closeAffordances(entries, None, rect) shouldBe Nil
    TabBarSurfaceComposition
      .closeAffordances(entries, None, rect, settings(reveal = CloseReveal.Hovered(BufferId(9))))
      .shouldBe(Nil)
  }

  it should "resolve an absolute on-screen rect's offset into the close regions' x positions" in {
    val entries = List(entry(0, "one"), entry(1, "two"))
    val regions = TabBarSurfaceComposition.closeAffordances(entries, Some(BufferId(0)), LayoutRect(5, 2, 40, 1))

    // The active tab is 7 wide from x=5, so its region is the two columns before its padding: [5+7-3,...)=[9,11).
    regions.map(_.rect.x) shouldBe List(9.0)
    regions.foreach(_.rect.y shouldBe 2.0)
  }

  it should "not touch forTabBar's own hit regions -- switch (#1077) and close (#1078) resolve from disjoint sets" in {
    val entries  = List(entry(0, "one"), entry(1, "two"), entry(2, "three"))
    val resolved = TabBarSurfaceComposition.forTabBar(entries, Some(BufferId(1)), LayoutRect(0, 0, 40, 1))

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

    // The tabs end at 14+7=21; the affordance's own blank column is the gap before its `+`.
    region.focusId shouldBe TabBarSurfaceComposition.NewTabFocusId
    region.rect.x shouldBe 21.0
    region.rect.width shouldBe 2.0
  }

  it should "sit flush against the strip's right edge once the tabs overflow the strip" in {
    val entries = List.tabulate(6)(i => entry(i, "title-" + i))

    val region = TabBarSurfaceComposition
      .newTabAffordance(entries, LayoutRect(0, 0, 30, 1))
      .getOrElse(fail("expected a new-tab affordance region"))

    region.rect.x shouldBe 28.0
    region.rect.width shouldBe 2.0
  }

  it should "resolve an absolute on-screen rect's offset into its own x position" in {
    val entries = List(entry(0, "one"), entry(1, "two"))

    val region = TabBarSurfaceComposition
      .newTabAffordance(entries, LayoutRect(5, 2, 40, 1))
      .getOrElse(fail("expected a new-tab affordance region"))

    // Tabs of 5 and 5 from x=5 with a gap of 2: they end at 5+5+2+5=17.
    region.rect.x shouldBe 17.0
    region.rect.y shouldBe 2.0
  }

  it should "not touch forTabBar's own hit regions or closeAffordances -- three disjoint region sets (issue #1080)" in {
    val entries  = List(entry(0, "one"), entry(1, "two"), entry(2, "three"))
    val rect     = LayoutRect(0, 0, 40, 1)
    val resolved = TabBarSurfaceComposition.forTabBar(entries, Some(BufferId(1)), rect)
    val closes   = TabBarSurfaceComposition.closeAffordances(entries, Some(BufferId(1)), rect)

    val newTabFocusId = TabBarSurfaceComposition.NewTabFocusId
    resolved.hitRegions.map(_.focusId) should not contain newTabFocusId
    closes.map(_.focusId) should not contain newTabFocusId
  }
