package com.serenity.ui.layout

import scala.annotation.tailrec

import com.serenity.state.models.{BufferId, TabListEntry}

/** Declarative composition plan for the always-visible tab strip (issue #1074 epic, #1075: Foundation / #1076: Render)
  * -- one horizontal row, painted the same way `TextOverlayRenderer.drawComposition` already paints any other
  * `SurfacePaintLayout.Distributed` box (segment text, allocated width, and the inter-segment `│` separator), the same
  * pattern `ModalSurfaceComposition`/`ContextMenuSurfaceComposition`/`CommandRunnerSurfaceComposition` establish for
  * their own surfaces.
  *
  * Unlike those siblings, this composition is built directly from `TabListEntry`/`BufferId` data rather than from a
  * `UiSurface` already placed on screen -- where a `TabBar`-content surface gets its on-screen `rect` from (a fixed
  * top-of-window strip vs. an existing floating/docked placement) is a layout-engine question this composition
  * deliberately does not answer; callers supply whatever `rect` they resolve.
  */
object TabBarSurfaceComposition:

  private val FocusIdPrefix = "tab-bar-"

  /** Dirty-buffer glyph, matching `PanelContentResolver.resolveTabList`'s existing convention for the same data. */
  private val DirtyGlyph = " ●"

  def focusId(bufferId: BufferId): SurfaceFocusId = SurfaceFocusId(s"$FocusIdPrefix${bufferId.value}")

  /** Recovers the `BufferId` a `hitAt` hit's focus id addresses, the inverse of `focusId`. */
  def bufferIdOf(id: SurfaceFocusId): Option[BufferId] =
    id.value.stripPrefix(FocusIdPrefix).toIntOption.map(BufferId.apply)

  private val CloseFocusIdPrefix = "tab-bar-close-"

  /** Columns of the active tab's close (x) affordance (issue #1078): a blank and the `x`, kept apart from `forTabBar`'s
    * own per-tab hit region (used to switch, #1077) so a close click and a switch click always resolve from two
    * disjoint region sets. Squeezed, the blank goes and the region is the `x` alone.
    */
  private val ActiveCloseCells = 2

  private val SnugCloseCells = 1

  /** The close glyph for a region of that width, following the same "leading space, glyph flush against the region's
    * trailing edge" convention [[NewTabGlyph]] uses for its own 2-column affordance.
    */
  private def closeGlyph(cells: Int): String = if cells >= ActiveCloseCells then " x" else "x"

  def closeFocusId(bufferId: BufferId): SurfaceFocusId = SurfaceFocusId(s"$CloseFocusIdPrefix${bufferId.value}")

  /** Recovers the `BufferId` a `closeAffordances` hit's focus id addresses, the inverse of `closeFocusId`. */
  def closeBufferIdOf(id: SurfaceFocusId): Option[BufferId] =
    id.value.stripPrefix(CloseFocusIdPrefix).toIntOption.map(BufferId.apply)

  /** Stable focus id for the trailing new-tab (+) affordance (issue #1080) -- not keyed to a `BufferId` like
    * `focusId`/`closeFocusId` since it addresses no existing tab.
    */
  val NewTabFocusId: SurfaceFocusId = SurfaceFocusId("tab-bar-new-tab")

  /** Columns reserved at the strip's trailing edge for the new-tab (+) affordance (issue #1080) -- carved out of the
    * row's total width before `allocate` sizes the tabs, so tabs never paint underneath it. Painted as `" +"`: the
    * blank column is the gap after the last tab, so the `+` sits one column past it, either straight after the last tab
    * or, once the tabs fill the strip, against its right edge.
    */
  private val NewTabAffordanceWidth = 2

  private val NewTabGlyph = " +"

  /** The width `allocate` should split across `entries`, with [[NewTabAffordanceWidth]] already carved off the strip's
    * trailing edge for the new-tab affordance -- shared by every entry point so all keep deriving tab cells from the
    * exact same reduced width.
    */
  private def tabsAvailableWidth(rect: LayoutRect): Int = math.max(0, rect.width - NewTabAffordanceWidth)

  /** The trailing new-tab (+) affordance's hit region (issue #1080): a single, buffer-independent target straight after
    * the last tab, or against the strip's right edge once the tabs fill the strip or overflow it (where the "+N" marker
    * has the room after the tabs). Kept apart from `forTabBar`'s per-tab hit regions and `closeAffordances`' per-tab
    * close regions -- a switch click, a close click, and a new-tab click always resolve from three disjoint region
    * sets. `None` when there is no tab strip to append it to (an empty tab list, mirroring `closeAffordances`).
    * `activeBufferId` must be the value passed to `forTabBar`, so both resolve the same visible window.
    */
  def newTabAffordance(
    entries: List[TabListEntry],
    rect: LayoutRect,
    activeBufferId: Option[BufferId] = None,
    settings: TabBarSettings = TabBarSettings.Default
  ): Option[SurfaceHitRegion] =
    if entries.isEmpty then None else Some(newTabRegion(layOut(entries, activeBufferId, rect, settings), rect))

  private def newTabRegion(layout: TabBarLayout, rect: LayoutRect): SurfaceHitRegion =
    val rightEdge = rect.x + rect.width - NewTabAffordanceWidth
    val afterTabs = layout.positions.lastOption.map((start, width) => start + width)
    val x =
      if layout.hiddenCount > 0 then rightEdge
      else afterTabs.fold(rightEdge)(end => math.min(end, rightEdge))
    SurfaceHitRegion(
      rect = LogicalPixelRect(
        x.toDouble,
        rect.y.toDouble,
        NewTabAffordanceWidth.toDouble,
        math.min(1.0, rect.height.toDouble)
      ),
      focusId = NewTabFocusId,
      actionId = Some(SurfaceActionId(NewTabFocusId.value)),
      semanticLabel = "New tab"
    )

  /** One tab's resolved geometry: the width allocated to it, its (possibly truncated) display title, whether it is the
    * active tab (which carries a close box), and whether that close box was squeezed.
    */
  final case class TabAllocation(
      entry: TabListEntry,
      allocatedWidth: Int,
      displayTitle: String,
      active: Boolean = false,
      snug: Boolean = false
  )

  /** Columns each inter-tab gap consumes beyond the tabs' own allocated widths: one for the separator glyph
    * `OverlaySegmentRowRenderer.renderCompactDistributedRow` draws, and one more for the blank column its cursor walk
    * always steps afterwards. `allocate` must reserve both, or the walk's own clamping (`remainingWidth`) would shrink
    * a later tab below the width this allocation promised it.
    */
  private val GapColumns = 2

  val PaddingCells = 1

  /** A tab's chrome around its title. An inactive tab is a cell of padding either side. The active tab adds its close
    * box between the title and the right padding: a blank and the `x`, or the `x` alone once squeezed. An inactive
    * tab's `x`, when shown, takes the right padding cell it already has.
    */
  final private case class Chrome(active: Boolean, snug: Boolean):
    def closeCells: Int = if !active then 0 else if snug then SnugCloseCells else ActiveCloseCells
    def cells: Int      = 2 * PaddingCells + closeCells

  final private case class Tab(entry: TabListEntry, label: String, active: Boolean)

  private def tabsOf(entries: List[TabListEntry], activeBufferId: Option[BufferId]): List[Tab] =
    entries.map(entry => Tab(entry, labelFor(entry), activeBufferId.contains(entry.bufferId)))

  /** What a tab wants: its title and its chrome, never more than the maximum (or its own floor, if that is wider). */
  private def naturalWidth(tab: Tab, snug: Boolean, settings: TabBarSettings): Int =
    val chrome = Chrome(tab.active, snug).cells
    math.min(
      chrome + settings.measure.cells(tab.label),
      math.max(settings.maxTabWidth, chrome + settings.measure.minTitleCells)
    )

  /** The narrowest a tab is squeezed to, before tabs are dropped from view: its chrome (close box squeezed) and the
    * title cells below which a cut title no longer leaves a legible fragment (issue #1081). A tab whose own title is
    * shorter keeps its natural width.
    */
  private def floorWidth(tab: Tab, settings: TabBarSettings): Int =
    math.min(
      naturalWidth(tab, snug = true, settings),
      Chrome(tab.active, snug = true).cells + settings.measure.minTitleCells
    )

  /** Size each tab to its content, up to the maximum width, reserving [[GapColumns]] per inter-tab gap out of
    * `availableWidth`. When the tabs want more than fits: first the active tab's close box is squeezed, then only the
    * widest tabs are cut, to the widest common width that fits (see [[fitWidths]]), never below a floor. Each cut label
    * (dirty glyph included) is cut to the title room its tab has left, ending in the measure's ellipsis.
    */
  def allocate(
    entries: List[TabListEntry],
    availableWidth: Int,
    activeBufferId: Option[BufferId] = None,
    settings: TabBarSettings = TabBarSettings.Default
  ): List[TabAllocation] =
    if entries.isEmpty then Nil
    else
      val tabs   = tabsOf(entries, activeBufferId)
      val budget = math.max(0, availableWidth - (entries.size - 1) * GapColumns)
      val roomy  = tabs.map(naturalWidth(_, snug = false, settings))
      val tight  = tabs.map(naturalWidth(_, snug = true, settings))
      val (widths, snug) =
        if roomy.sum <= budget then (roomy, false)
        else if tight.sum <= budget then (tight, true)
        else (fitWidths(tight, tabs.map(floorWidth(_, settings)), budget), true)
      tabs.zip(widths).map { (tab, width) =>
        val room = math.max(1, width - Chrome(tab.active, snug).cells)
        TabAllocation(tab.entry, width, settings.measure.fit(tab.label, room), tab.active, snug)
      }

  /** `wanted` widths made to fit `budget`. Every tab wider than a common cap is cut to it, never below its own floor,
    * the cap being the widest at which the tabs fit; the cells left over go to the leftmost tabs that were cut. If not
    * even the floors fit, the budget is shared out evenly and no tab gets less than one column.
    */
  private def fitWidths(wanted: List[Int], floors: List[Int], budget: Int): List[Int] =
    def at(cap: Int): List[Int] = wanted.zip(floors).map((width, floor) => math.max(floor, math.min(width, cap)))
    if floors.sum > budget then
      val share = budget / wanted.size
      val extra = budget % wanted.size
      List.tabulate(wanted.size)(index => math.max(1, share + (if index < extra then 1 else 0)))
    else
      val widest = wanted.foldLeft(0)(math.max)
      val cap    = Iterator.range(widest, -1, -1).find(at(_).sum <= budget).getOrElse(0)
      val fitted = at(cap)
      val spare  = budget - fitted.sum
      val cut =
        wanted.zip(fitted).zipWithIndex.collect { case ((want, got), index) if want > got => index }.take(spare).toSet
      fitted.zipWithIndex.map((width, index) => if cut(index) then width + 1 else width)

  private def labelFor(entry: TabListEntry): String =
    if entry.isDirty then s"${entry.title}$DirtyGlyph" else entry.title

  /** Columns reserved for the trailing "+N" overflow marker (issue #1081), painted as one extra, non-interactive
    * `OverlaySegment` appended after the visible tabs -- large enough for a two- or three-digit hidden count.
    */
  private val OverflowIndicatorWidth = 4

  /** The visible subset of `entries` and how many are hidden (issue #1081). A contiguous, order-preserving window of
    * the tabs that fit at their floors, grown from the active tab outwards so it is never scrolled out of view.
    * Reachable hidden tabs are not this composition's concern: the existing keyboard-toggled `SurfaceContent.TabList`
    * overlay (`TabListContent.build`, `AppEventReducer.toggleTabList`) already lists every open buffer regardless of
    * strip width, so overflow here does not need its own dropdown/menu affordance to keep hidden tabs reachable.
    */
  final case class VisibleTabWindow(visible: List[TabListEntry], hiddenCount: Int)

  def visibleWindow(
    entries: List[TabListEntry],
    activeBufferId: Option[BufferId],
    availableWidth: Int,
    settings: TabBarSettings = TabBarSettings.Default
  ): VisibleTabWindow =
    if entries.isEmpty then VisibleTabWindow(Nil, 0)
    else
      val floors = tabsOf(entries, activeBufferId).map(floorWidth(_, settings)).toVector
      if floors.sum + (entries.size - 1) * GapColumns <= availableWidth then VisibleTabWindow(entries, 0)
      else
        val content = math.max(0, availableWidth - OverflowIndicatorWidth - GapColumns)
        val activeIndex = activeBufferId
          .map(id => entries.indexWhere(_.bufferId == id))
          .filter(_ >= 0)
          .getOrElse(0)

        // Grows the window one tab at a time, towards the side that is shorter so it stays centred on the active tab,
        // and to the other side where that one has no tab left or no room.
        @tailrec def grow(low: Int, high: Int, used: Int): (Int, Int) =
          def fits(index: Int): Boolean = used + GapColumns + floors(index) <= content
          val canLeft                   = low > 0 && fits(low - 1)
          val canRight                  = high < floors.size - 1 && fits(high + 1)
          val preferLeft                = activeIndex - low <= high - activeIndex
          if canLeft && (preferLeft || !canRight) then grow(low - 1, high, used + GapColumns + floors(low - 1))
          else if canRight then grow(low, high + 1, used + GapColumns + floors(high + 1))
          else (low, high)

        val (low, high) = grow(activeIndex, activeIndex, floors(activeIndex))
        VisibleTabWindow(entries.slice(low, high + 1), entries.size - (high - low + 1))

  /** One resolved layout pass shared by `forTabBar`, `closeAffordances` and `newTabAffordance` (issue #1081): the
    * visible tabs' own allocations and positions, plus how many tabs the window left hidden. Reserves
    * [[OverflowIndicatorWidth]] + [[GapColumns]] from `rect.width` before allocating the visible tabs whenever any are
    * hidden, so the "+N" marker `forTabBar` paints always has room within `rect` alongside them.
    */
  final private case class TabBarLayout(
      allocations: List[TabAllocation],
      positions: List[(Int, Int)],
      hiddenCount: Int
  )

  private def layOut(
    entries: List[TabListEntry],
    activeBufferId: Option[BufferId],
    rect: LayoutRect,
    settings: TabBarSettings
  ): TabBarLayout =
    val availableWidth = tabsAvailableWidth(rect)
    val window         = visibleWindow(entries, activeBufferId, availableWidth, settings)
    val tabsWidth =
      if window.hiddenCount > 0 then math.max(0, availableWidth - OverflowIndicatorWidth - GapColumns)
      else availableWidth
    val allocations = allocate(window.visible, tabsWidth, activeBufferId, settings)
    TabBarLayout(allocations, tabPositions(rect.x, allocations), window.hiddenCount)

  /** Resolve the tab strip's paint/hit-test plan for one absolute on-screen `rect`. The row paints as a single
    * `SurfacePaintLayout.Distributed` box (one segment per tab), but each tab still gets its own `SurfaceHitRegion`
    * with its own rect, computed by the same left-to-right walk `OverlaySegmentRowRenderer.renderCompactDistributedRow`
    * uses to place segments, so a click resolves to the tab it visually lands on.
    *
    * Once more tabs are open than fit at their floors (issue #1081), only a contiguous, active-tab-centred window of
    * them is allocated at all -- the rest get neither a paint segment nor a hit region, so a scrolled-out tab is never
    * clickable. A trailing, non-interactive "+N" segment is appended to the paint box (but not to `hitRegions`) so the
    * row still reflects how many tabs are hidden; those tabs stay reachable through the existing
    * `SurfaceContent.TabList` overlay rather than through any new affordance here.
    */
  def forTabBar(
    entries: List[TabListEntry],
    activeBufferId: Option[BufferId],
    rect: LayoutRect,
    settings: TabBarSettings = TabBarSettings.Default
  ): ResolvedSurfaceComposition =
    val bounds = LogicalPixelRect(rect.x.toDouble, rect.y.toDouble, rect.width.toDouble, rect.height.toDouble)
    if entries.isEmpty then
      ResolvedSurfaceComposition(
        bounds = bounds,
        intrinsicSize = SurfaceIntrinsicSize(bounds.width, bounds.height),
        paintBoxes = Nil,
        hitRegions = Nil,
        focusOrder = Nil
      )
    else
      val layout      = layOut(entries, activeBufferId, rect, settings)
      val allocations = layout.allocations
      val overflowing = layout.hiddenCount > 0

      val tabSegments = allocations.zipWithIndex.map {
        case (allocation, index) =>
          val room = allocation.allocatedWidth - Chrome(allocation.active, allocation.snug).cells
          OverlaySegment(
            text = allocation.displayTitle,
            selected = allocation.active,
            trailingSeparator = index < allocations.size - 1 || overflowing,
            allocatedWidth = Some(allocation.allocatedWidth),
            titleFit = Some(SegmentTitleFit(PaddingCells, room, settings.measure.ellipsis))
          )
      }
      val overflowSegment =
        if overflowing then
          List(
            OverlaySegment(
              text = s"+${layout.hiddenCount}".take(OverflowIndicatorWidth),
              allocatedWidth = Some(OverflowIndicatorWidth)
            )
          )
        else Nil
      val segments = tabSegments ++ overflowSegment

      val rowBox = SurfacePaintBox(
        kind = SurfacePaintKind.Text,
        rect = LogicalPixelRect(bounds.x, bounds.y, bounds.width, math.min(1.0, bounds.height)),
        text = Some(segments.map(_.text).mkString(" ")),
        segments = segments,
        layout = SurfacePaintLayout.Distributed
      )

      // The new-tab (+) affordance paints as its own plain-text box at its own reserved rect (issue #1080), rather
      // than as another `Distributed` segment of `rowBox` -- it addresses no `BufferId`, so it has no place in a row
      // whose segments are otherwise one-to-one with `entries`.
      val newTabBox =
        val region = newTabRegion(layout, rect)
        SurfacePaintBox(
          kind = SurfacePaintKind.Text,
          rect = region.rect,
          text = Some(NewTabGlyph),
          focusId = Some(region.focusId),
          actionId = region.actionId,
          semanticLabel = Some(region.semanticLabel)
        )

      // Each revealed close (x) affordance paints as its own plain-text box at its own reserved rect, the same way
      // `newTabBox` does above, rather than as another `Distributed` segment of `rowBox` -- and is derived from
      // `closeRegions` itself (not re-walked here) so the painted glyph and its click target can never drift apart
      // (issue #1078).
      val closeBoxes = closeRegions(layout, rect, settings.reveal).map { region =>
        SurfacePaintBox(
          kind = SurfacePaintKind.Text,
          rect = region.rect,
          text = Some(closeGlyph(region.rect.width.toInt)),
          selected = closeBufferIdOf(region.focusId).exists(activeBufferId.contains),
          focusId = Some(region.focusId),
          actionId = region.actionId,
          semanticLabel = Some(region.semanticLabel)
        )
      }

      val hitRegions = allocations.zip(layout.positions).map {
        case (allocation, (startX, width)) =>
          val id = focusId(allocation.entry.bufferId)
          SurfaceHitRegion(
            rect = LogicalPixelRect(startX.toDouble, rect.y.toDouble, width.toDouble, math.min(1.0, bounds.height)),
            focusId = id,
            actionId = Some(SurfaceActionId(id.value)),
            semanticLabel = allocation.entry.title
          )
      }

      ResolvedSurfaceComposition(
        bounds = bounds,
        intrinsicSize = SurfaceIntrinsicSize(bounds.width, bounds.height),
        paintBoxes = List(rowBox, newTabBox) ++ closeBoxes,
        hitRegions = hitRegions,
        focusOrder = hitRegions.map(_.focusId)
      )

  /** Mirrors `OverlaySegmentRowRenderer.renderCompactDistributedRow`'s cursor walk exactly: each tab occupies
    * `allocatedWidth` columns, then the walk steps one column for the trailing separator glyph and one more blank
    * column before the next tab starts -- together, [[GapColumns]], which `allocate` already reserved.
    */
  private def tabPositions(x: Int, allocations: List[TabAllocation]): List[(Int, Int)] =
    allocations.zipWithIndex
      .foldLeft((x, List.empty[(Int, Int)])) {
        case ((cursorX, acc), (allocation, index)) =>
          val cellWidth   = allocation.allocatedWidth
          val afterCell   = cursorX + cellWidth
          val hasTrailing = index < allocations.size - 1
          val nextCursorX = if hasTrailing then afterCell + GapColumns else afterCell
          (nextCursorX, acc :+ (cursorX, cellWidth))
      }
      ._2

  /** Where a tab's close box sits, as its first column and width, if it has one to show. The active tab's is the
    * columns before its right padding. An inactive tab's, when `reveal` asks for it, is that right padding cell itself,
    * so no tab's width ever depends on it.
    */
  private def closeBoxOf(
    allocation: TabAllocation,
    startX: Int,
    width: Int,
    reveal: CloseReveal
  ): Option[(Int, Int)] =
    if allocation.active then
      val cells = Chrome(allocation.active, allocation.snug).closeCells
      Option.when(width > cells + PaddingCells)((startX + width - PaddingCells - cells, cells))
    else
      val revealed = reveal match
        case CloseReveal.All               => true
        case CloseReveal.Hovered(bufferId) => bufferId == allocation.entry.bufferId
        case CloseReveal.ActiveOnly        => false
      Option.when(revealed && width >= 3)((startX + width - PaddingCells, PaddingCells))

  private def closeRegions(layout: TabBarLayout, rect: LayoutRect, reveal: CloseReveal): List[SurfaceHitRegion] =
    layout.allocations.zip(layout.positions).flatMap {
      case (allocation, (startX, width)) =>
        closeBoxOf(allocation, startX, width, reveal).map { (closeX, cells) =>
          val id = closeFocusId(allocation.entry.bufferId)
          SurfaceHitRegion(
            rect = LogicalPixelRect(
              closeX.toDouble,
              rect.y.toDouble,
              cells.toDouble,
              math.min(1.0, rect.height.toDouble)
            ),
            focusId = id,
            actionId = Some(SurfaceActionId(id.value)),
            semanticLabel = s"Close ${allocation.entry.title}"
          )
        }
    }

  /** Per-tab close (x) affordance hit regions (issue #1078): the active tab's, and those of the tabs `settings.reveal`
    * asks for, from the exact same `layOut` (`allocate` + `tabPositions`) `forTabBar` uses for its own hit regions --
    * including its overflow window (issue #1081), so a scrolled-out tab has no close region either. Deliberately not
    * part of `forTabBar`'s own `ResolvedSurfaceComposition` -- resolved as its own list so a close click (here) and a
    * switch click (`forTabBar`'s existing hit regions, issue #1077) always come from two disjoint region sets rather
    * than one overloaded one. `forTabBar` paints each of these regions' glyph by calling the same code, so the painted
    * glyph and its click target can never drift apart.
    *
    * `activeBufferId` must be the same value passed to `forTabBar` for this same `rect`/`entries`, so both resolve the
    * same visible window under overflow.
    */
  def closeAffordances(
    entries: List[TabListEntry],
    activeBufferId: Option[BufferId],
    rect: LayoutRect,
    settings: TabBarSettings = TabBarSettings.Default
  ): List[SurfaceHitRegion] =
    if entries.isEmpty then Nil
    else closeRegions(layOut(entries, activeBufferId, rect, settings), rect, settings.reveal)
