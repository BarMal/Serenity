package com.serenity.state.manager

import com.serenity.frontend.FrontendCapabilities
import com.serenity.rope.Balance
import com.serenity.state.core.EditorState
import com.serenity.state.models.*
import com.serenity.ui.layout.{
  CloseReveal,
  LayoutEngine,
  LayoutRect,
  SurfaceHitRegion,
  TabBarSettings,
  TabBarSurfaceComposition,
  ViewportSize
}
import com.serenity.ui.renderer.TabBarLayoutSettings
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Coverage for `TabBarMouseHitTesting` -- resolving a click's cell coordinate to the `BufferId` of the tab it landed
  * on (issue #1075: Foundation), resolving a click against live `AppState` into a buffer switch (issue #1077: "Click a
  * tab to switch buffer"), the trailing new-tab (+) affordance into opening a new tab (issue #1080: "New-tab
  * affordance"), and the close box a tab shows: the active tab's always, an inactive tab's while the pointer is over it.
  *
  * The `AppState` cases take their geometry from the very settings the code under test uses, so they hold whatever
  * fonts the machine has.
  */
class TabBarMouseHitTestingSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private def entry(id: Int, title: String): TabListEntry = TabListEntry(BufferId(id), title, isDirty = false)

  private val entries = List(entry(0, "one"), entry(1, "two"), entry(2, "three"))
  // No tab is active, so each is its title plus 2 (see TabBarSurfaceCompositionSpec): tab 0 at [0,5), gap [5,7), tab 1
  // at [7,12), gap [12,14), tab 2 at [14,21), then the new-tab affordance (issue #1080) at [21,23).
  private val rect = LayoutRect(0, 0, 40, 1)

  "hitAt" should "resolve a click inside the first tab to its BufferId" in {
    TabBarMouseHitTesting.hitAt(entries, None, rect, col = 2, row = 0) shouldBe Some(BufferId(0))
  }

  it should "resolve a click inside the second tab to its BufferId" in {
    TabBarMouseHitTesting.hitAt(entries, None, rect, col = 9, row = 0) shouldBe Some(BufferId(1))
  }

  it should "resolve a click inside the third tab to its BufferId" in {
    TabBarMouseHitTesting.hitAt(entries, None, rect, col = 17, row = 0) shouldBe Some(BufferId(2))
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

  private val twoTabViewport = ViewportSize(60, 5)

  private def twoBufferState: AppState =
    val second = Buffer.fromString(BufferId(1), "second")
    val base   = AppState.initial(com.serenity.ChromeOn.config)
    base.copy(
      runtime = base.runtime.copy(viewportSize = Some(twoTabViewport)),
      persisted = base.persisted.copy(
        buffers = base.persisted.buffers + (second.id -> second),
        bufferOrder = base.persisted.bufferOrder :+ second.id
      )
    )

  private def hovering(state: AppState, tab: Option[BufferId]): AppState =
    state.copy(runtime = state.runtime.copy(pointerGesture = state.runtime.pointerGesture.copy(hoveredTab = tab)))

  private def inTerminal(state: AppState): AppState =
    state.copy(runtime = state.runtime.copy(capabilities = FrontendCapabilities.tui()))

  /** The strip as `state` lays it out: its rect, its tabs' hit regions and the new-tab region. */
  final private case class Strip(rect: LayoutRect, tabs: List[SurfaceHitRegion], newTab: SurfaceHitRegion)

  private def stripOf(state: AppState): Strip =
    val surface = state.tabBarSurface.getOrElse(fail("expected a tab bar"))
    val (tabEntries, active) = surface.content match
      case SurfaceContent.TabBar(listed, activeBufferId) => (listed, activeBufferId)
      case _                                             => fail("expected tab bar content")
    val stripRect = LayoutEngine
      .calculateLayoutWithUI(state, twoTabViewport)
      .tabBarRect
      .getOrElse(fail("expected a tab bar rect"))
    val settings = TabBarLayoutSettings.of(state)
    Strip(
      stripRect,
      TabBarSurfaceComposition.forTabBar(tabEntries, active, stripRect, settings).hitRegions,
      TabBarSurfaceComposition
        .newTabAffordance(tabEntries, stripRect, active, settings)
        .getOrElse(fail("expected a new-tab region"))
    )

  private def colOf(region: SurfaceHitRegion): Int = region.rect.x.toInt

  "clickTarget" should "resolve a click on a tab other than the active one to Some(Some(bufferId))" in {
    val state = twoBufferState
    val strip = stripOf(state)

    TabBarMouseHitTesting.clickTarget(state, colOf(strip.tabs(1)) + 1, row = 0) shouldBe Some(Some(BufferId(1)))
  }

  it should "swallow a click on the already-active tab as a no-op" in {
    val state = twoBufferState
    val strip = stripOf(state)

    TabBarMouseHitTesting.clickTarget(state, colOf(strip.tabs.head) + 1, row = 0) shouldBe Some(None)
  }

  it should "swallow a click on the gap between tabs" in {
    val state = twoBufferState
    val strip = stripOf(state)

    TabBarMouseHitTesting.clickTarget(state, colOf(strip.tabs.head) + strip.tabs.head.rect.width.toInt, row = 0) shouldBe
      Some(None)
  }

  it should "miss for a click outside the strip's row, leaving it for another mouse target" in {
    TabBarMouseHitTesting.clickTarget(twoBufferState, col = 3, row = 1) shouldBe None
  }

  it should "miss when there is no tab bar this frame (a single open buffer)" in {
    TabBarMouseHitTesting.clickTarget(
      AppState.initial(com.serenity.ChromeOn.config),
      col = 3,
      row = 0
    ) shouldBe None
  }

  it should "miss when the viewport size is not yet known" in {
    val noViewport = twoBufferState.copy(runtime = twoBufferState.runtime.copy(viewportSize = None))

    TabBarMouseHitTesting.clickTarget(noViewport, col = 3, row = 0) shouldBe None
  }

  "handleClick" should "switch the active pane to the clicked tab's buffer and keep it focused" in {
    val state = twoBufferState
    val strip = stripOf(state)

    val updated =
      TabBarMouseHitTesting
        .handleClick(state, colOf(strip.tabs(1)) + 1, row = 0)
        .getOrElse(fail("expected a click hit"))

    updated.persisted.layout.editorPanes(PaneId(0)).bufferId shouldBe Some(BufferId(1))
    updated.focusedBufferId shouldBe Some(BufferId(1))
    updated.persisted.focus shouldBe Focus.EditorPane(PaneId(0))
    updated.persisted.bufferOrder shouldBe state.persisted.bufferOrder
  }

  it should "leave state completely unchanged when clicking the already-active tab" in {
    val state = twoBufferState
    val strip = stripOf(state)

    TabBarMouseHitTesting.handleClick(state, colOf(strip.tabs.head) + 1, row = 0) shouldBe Some(state)
  }

  it should "return None for a click outside the strip" in {
    TabBarMouseHitTesting.handleClick(twoBufferState, col = 3, row = 1) shouldBe None
  }

  "closeHitAt" should "resolve a click on a revealed tab's close affordance to its BufferId (issue #1078)" in {
    // Revealed for every tab, each tab's close region is its last (padding) column: 4, 11 and 20.
    val all = TabBarSettings(reveal = CloseReveal.All)

    TabBarMouseHitTesting.closeHitAt(entries, None, rect, col = 4, row = 0, all) shouldBe Some(BufferId(0))
    TabBarMouseHitTesting.closeHitAt(entries, None, rect, col = 11, row = 0, all) shouldBe Some(BufferId(1))
    TabBarMouseHitTesting.closeHitAt(entries, None, rect, col = 20, row = 0, all) shouldBe Some(BufferId(2))
  }

  it should "resolve the active tab's close affordance with nothing revealed" in {
    // The active tab (id 1) is [7,14): its close region is the two columns before its padding, [11,13).
    TabBarMouseHitTesting.closeHitAt(entries, Some(BufferId(1)), rect, col = 12, row = 0) shouldBe Some(BufferId(1))
  }

  it should "resolve no close hit on an inactive tab nothing reveals" in {
    TabBarMouseHitTesting.closeHitAt(entries, Some(BufferId(1)), rect, col = 4, row = 0) shouldBe None
  }

  it should "resolve no close hit for a click inside a tab but outside its close affordance" in {
    TabBarMouseHitTesting.closeHitAt(
      entries,
      None,
      rect,
      col = 1,
      row = 0,
      TabBarSettings(reveal = CloseReveal.All)
    ) shouldBe None
  }

  it should "resolve no close hit for a click outside the strip's row" in {
    TabBarMouseHitTesting.closeHitAt(
      entries,
      None,
      rect,
      col = 4,
      row = 1,
      TabBarSettings(reveal = CloseReveal.All)
    ) shouldBe None
  }

  it should "resolve no close hit when there are no open buffers" in {
    TabBarMouseHitTesting.closeHitAt(Nil, None, rect, col = 5, row = 0) shouldBe None
  }

  "closeClickTarget" should "resolve the active tab's close box, hovered or not" in {
    val state = twoBufferState
    val close = TabBarSurfaceComposition
      .closeAffordances(
        stateEntries(state),
        Some(BufferId(0)),
        stripOf(state).rect,
        TabBarLayoutSettings.of(state)
      )
      .headOption
      .getOrElse(fail("expected the active tab's close region"))

    TabBarMouseHitTesting.closeClickTarget(state, colOf(close), row = 0) shouldBe Some(BufferId(0))
  }

  it should "resolve an inactive tab's close box only while the pointer is over that tab" in {
    val state   = twoBufferState
    val hovered = hovering(state, Some(BufferId(1)))
    val strip   = stripOf(hovered)
    val closeCell =
      TabBarSurfaceComposition
        .closeAffordances(stateEntries(hovered), Some(BufferId(0)), strip.rect, TabBarLayoutSettings.of(hovered))
        .find(_.focusId == TabBarSurfaceComposition.closeFocusId(BufferId(1)))
        .map(colOf)
        .getOrElse(fail("expected the hovered tab's close region"))

    TabBarMouseHitTesting.closeClickTarget(hovered, closeCell, row = 0) shouldBe Some(BufferId(1))
    TabBarMouseHitTesting.closeClickTarget(state, closeCell, row = 0) shouldBe None
  }

  it should "switch tabs, not close, on the cell of a close box nobody has revealed" in {
    val state   = twoBufferState
    val hovered = hovering(state, Some(BufferId(1)))
    val strip   = stripOf(hovered)
    val closeCell =
      TabBarSurfaceComposition
        .closeAffordances(stateEntries(hovered), Some(BufferId(0)), strip.rect, TabBarLayoutSettings.of(hovered))
        .find(_.focusId == TabBarSurfaceComposition.closeFocusId(BufferId(1)))
        .map(colOf)
        .getOrElse(fail("expected the hovered tab's close region"))

    TabBarMouseHitTesting.clickTarget(state, closeCell, row = 0) shouldBe Some(Some(BufferId(1)))
  }

  "hoveredTabAt" should "name the tab under the pointer in a GUI, the active one included" in {
    val state = twoBufferState
    val strip = stripOf(state)

    TabBarMouseHitTesting.hoveredTabAt(state, colOf(strip.tabs.head) + 1, row = 0) shouldBe Some(BufferId(0))
    TabBarMouseHitTesting.hoveredTabAt(state, colOf(strip.tabs(1)) + 1, row = 0) shouldBe Some(BufferId(1))
  }

  it should "name no tab over a gap, off the strip, or in a terminal, which has no hover" in {
    val state = twoBufferState
    val strip = stripOf(state)
    val gap   = colOf(strip.tabs.head) + strip.tabs.head.rect.width.toInt

    TabBarMouseHitTesting.hoveredTabAt(state, gap, row = 0) shouldBe None
    TabBarMouseHitTesting.hoveredTabAt(state, colOf(strip.tabs(1)) + 1, row = 3) shouldBe None
    TabBarMouseHitTesting.hoveredTabAt(inTerminal(state), 3, row = 0) shouldBe None
  }

  "pointerTargetAt" should "show the hand over a close box that is showing, and not over one that is hidden" in {
    val state   = twoBufferState
    val hovered = hovering(state, Some(BufferId(1)))
    val strip   = stripOf(hovered)
    val closeCell =
      TabBarSurfaceComposition
        .closeAffordances(stateEntries(hovered), Some(BufferId(0)), strip.rect, TabBarLayoutSettings.of(hovered))
        .find(_.focusId == TabBarSurfaceComposition.closeFocusId(BufferId(1)))
        .map(colOf)
        .getOrElse(fail("expected the hovered tab's close region"))

    TabBarMouseHitTesting.pointerTargetAt(hovered, closeCell, row = 0) shouldBe Some(PointerHitTarget.Control)
    TabBarMouseHitTesting.pointerTargetAt(state, closeCell, row = 0) shouldBe Some(PointerHitTarget.Inert)
  }

  private def stateEntries(state: AppState): List[TabListEntry] =
    state.tabBarSurface.map(_.content) match
      case Some(SurfaceContent.TabBar(listed, _)) => listed
      case _                                      => Nil

  // Same state as `twoBufferState`, but with `nextBufferId` corrected to `BufferId(2)` -- `twoBufferState` adds its
  // second buffer directly at `BufferId(1)` without advancing `nextBufferId` past it (fine for the switch/close
  // coverage above, which never allocates a new buffer id), but `EditorState.openNewTab` needs a fixture where the
  // next id it hands out doesn't collide with an already-open buffer.
  private def twoBufferStateForNewTab: AppState =
    twoBufferState.copy(runtime = twoBufferState.runtime.copy(nextBufferId = BufferId(2)))

  "newTabClickTarget" should "resolve true for a click on the trailing new-tab (+) affordance (issue #1080)" in {
    val state = twoBufferStateForNewTab

    TabBarMouseHitTesting.newTabClickTarget(state, colOf(stripOf(state).newTab) + 1, row = 0) shouldBe true
  }

  it should "resolve false for a click on a tab" in {
    TabBarMouseHitTesting.newTabClickTarget(twoBufferStateForNewTab, col = 3, row = 0) shouldBe false
  }

  it should "resolve false for a click outside the strip's row" in {
    val state = twoBufferStateForNewTab

    TabBarMouseHitTesting.newTabClickTarget(state, colOf(stripOf(state).newTab) + 1, row = 1) shouldBe false
  }

  it should "resolve false when there is no tab bar this frame (a single open buffer)" in {
    TabBarMouseHitTesting.newTabClickTarget(
      AppState.initial(com.serenity.ChromeOn.config),
      col = 20,
      row = 0
    ) shouldBe false
  }

  "handleNewTabClick" should
    "open a new tab identically to Ctrl+T and focus it when the affordance is clicked (issue #1080)" in {
      val state = twoBufferStateForNewTab

      val updated =
        TabBarMouseHitTesting
          .handleNewTabClick(state, colOf(stripOf(state).newTab) + 1, row = 0)
          .getOrElse(fail("expected a click hit"))

      updated.persisted.buffers should have size 3
      updated.persisted.bufferOrder should contain(BufferId(2))
      updated.focusedBufferId shouldBe Some(BufferId(2))
      // Mouse and keyboard (Ctrl+T) share EditorState.openNewTab, so the two produce identical resulting state.
      updated shouldBe EditorState.openNewTab(state)
    }

  it should "return None for a click that misses the affordance" in {
    TabBarMouseHitTesting.handleNewTabClick(twoBufferStateForNewTab, col = 3, row = 0) shouldBe None
  }
