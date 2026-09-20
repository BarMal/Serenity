package com.serenity.ui.layout

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

  /** Columns reserved at a tab's right edge for its close (x) affordance's click target (issue #1078) -- kept apart
    * from `forTabBar`'s own per-tab hit region (used to switch, #1077) rather than folded into the same list, so a
    * close click and a switch click always resolve from two disjoint region sets instead of one overloaded one.
    */
  private val CloseHitWidth = 2

  def closeFocusId(bufferId: BufferId): SurfaceFocusId = SurfaceFocusId(s"$CloseFocusIdPrefix${bufferId.value}")

  /** Recovers the `BufferId` a `closeAffordances` hit's focus id addresses, the inverse of `closeFocusId`. */
  def closeBufferIdOf(id: SurfaceFocusId): Option[BufferId] =
    id.value.stripPrefix(CloseFocusIdPrefix).toIntOption.map(BufferId.apply)

  /** Stable focus id for the trailing new-tab (+) affordance (issue #1080) -- not keyed to a `BufferId` like
    * `focusId`/`closeFocusId` since it addresses no existing tab.
    */
  val NewTabFocusId: SurfaceFocusId = SurfaceFocusId("tab-bar-new-tab")

  /** Columns reserved at the strip's trailing edge for the new-tab (+) affordance (issue #1080) -- carved out of the
    * row's total width before `allocate` splits the rest across tabs, the same way [[CloseHitWidth]] is carved out of a
    * tab's own cell, so tabs never paint underneath it. Painted as `" +"` so the glyph itself lands flush against the
    * strip's right edge, one blank column short of the last tab's cell -- the same visual gap `GapColumns` reserves
    * between two tabs, without a second, separate reservation.
    */
  private val NewTabAffordanceWidth = 2

  private val NewTabGlyph = " +"

  /** The width `allocate` should split across `entries`, with [[NewTabAffordanceWidth]] already carved off the strip's
    * trailing edge for the new-tab affordance -- shared by `forTabBar` and `closeAffordances` so both keep deriving tab
    * cells from the exact same reduced width.
    */
  private def tabsAvailableWidth(rect: LayoutRect): Int = math.max(0, rect.width - NewTabAffordanceWidth)

  /** The trailing new-tab (+) affordance's hit region (issue #1080): a single, buffer-independent target at the strip's
    * right edge, kept apart from `forTabBar`'s per-tab hit regions and `closeAffordances`' per-tab close regions -- a
    * switch click, a close click, and a new-tab click always resolve from three disjoint region sets. `None` when there
    * is no tab strip to append it to (an empty tab list, mirroring `closeAffordances`).
    */
  def newTabAffordance(entries: List[TabListEntry], rect: LayoutRect): Option[SurfaceHitRegion] =
    if entries.isEmpty then None
    else
      val x = rect.x + rect.width - NewTabAffordanceWidth
      Some(
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
      )

  /** One tab's resolved geometry: the width allocated to it and its (possibly truncated) display title. */
  final case class TabAllocation(entry: TabListEntry, allocatedWidth: Int, displayTitle: String)

  /** Columns each inter-tab gap consumes beyond the tabs' own allocated widths: one for the separator glyph
    * `OverlaySegmentRowRenderer.renderCompactDistributedRow` draws, and one more for the blank column its cursor walk
    * always steps afterwards. `allocate` must reserve both, or the walk's own clamping (`remainingWidth`) would shrink
    * a later tab below the width this allocation promised it.
    */
  private val GapColumns = 2

  /** Split `availableWidth` cell-columns evenly across `entries`, reserving [[GapColumns]] per inter-tab gap, then
    * truncate each tab's label (dirty glyph included) to fit -- following the same `text.take(width - 3) + "..."`
    * ellipsis convention `RendererPaneContent.renderBufferHeader` already uses for a pane's own buffer title.
    */
  def allocate(entries: List[TabListEntry], availableWidth: Int): List[TabAllocation] =
    if entries.isEmpty then Nil
    else
      val gapCount     = entries.size - 1
      val contentWidth = math.max(0, availableWidth - gapCount * GapColumns)
      val baseWidth    = contentWidth / entries.size
      val remainder    = contentWidth % entries.size
      entries.zipWithIndex.map {
        case (entry, index) =>
          val width = math.max(1, baseWidth + (if index < remainder then 1 else 0))
          TabAllocation(entry, width, truncate(labelFor(entry), width))
      }

  private def labelFor(entry: TabListEntry): String =
    if entry.isDirty then s"${entry.title}$DirtyGlyph" else entry.title

  private def truncate(text: String, width: Int): String =
    val maxWidth = math.max(1, width)
    if text.length <= maxWidth then text
    else if maxWidth <= 3 then text.take(maxWidth)
    else text.take(maxWidth - 3) + "..."

  /** Resolve the tab strip's paint/hit-test plan for one absolute on-screen `rect`. The row paints as a single
    * `SurfacePaintLayout.Distributed` box (one segment per tab), but each tab still gets its own `SurfaceHitRegion`
    * with its own rect, computed by the same left-to-right walk `OverlaySegmentRowRenderer.renderCompactDistributedRow`
    * uses to place segments, so a click resolves to the tab it visually lands on.
    */
  def forTabBar(
    entries: List[TabListEntry],
    activeBufferId: Option[BufferId],
    rect: LayoutRect
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
      val allocations = allocate(entries, tabsAvailableWidth(rect))
      val positions   = tabPositions(rect.x, allocations)

      val segments = allocations.zipWithIndex.map {
        case (allocation, index) =>
          OverlaySegment(
            text = allocation.displayTitle,
            selected = activeBufferId.contains(allocation.entry.bufferId),
            trailingSeparator = index < allocations.size - 1,
            allocatedWidth = Some(allocation.allocatedWidth)
          )
      }

      val rowBox = SurfacePaintBox(
        kind = SurfacePaintKind.Text,
        rect = LogicalPixelRect(bounds.x, bounds.y, bounds.width, math.min(1.0, bounds.height)),
        text = Some(allocations.map(_.displayTitle).mkString(" ")),
        segments = segments,
        layout = SurfacePaintLayout.Distributed
      )

      // The new-tab (+) affordance paints as its own plain-text box at its own reserved rect (issue #1080), rather
      // than as another `Distributed` segment of `rowBox` -- it addresses no `BufferId`, so it has no place in a row
      // whose segments are otherwise one-to-one with `entries`.
      val newTabBox = newTabAffordance(entries, rect).map { region =>
        SurfacePaintBox(
          kind = SurfacePaintKind.Text,
          rect = region.rect,
          text = Some(NewTabGlyph),
          focusId = Some(region.focusId),
          actionId = region.actionId,
          semanticLabel = Some(region.semanticLabel)
        )
      }

      val hitRegions = allocations.zip(positions).map {
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
        paintBoxes = List(rowBox) ++ newTabBox,
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

  /** Per-tab close (x) affordance hit regions (issue #1078), one per tab wide enough to leave room for one -- the
    * rightmost [[CloseHitWidth]] columns of that tab's own cell, from the exact same `allocate`/`tabPositions` layout
    * `forTabBar` uses for its own hit regions. Deliberately not part of `forTabBar`'s own `ResolvedSurfaceComposition`
    * -- resolved as its own list so a close click (here) and a switch click (`forTabBar`'s existing hit regions, issue
    * #1077) always come from two disjoint region sets rather than one overloaded one; painting the glyph itself into
    * the shared `Distributed`-row renderer is not yet wired up (tracked on the issue, not a silent gap).
    */
  def closeAffordances(entries: List[TabListEntry], rect: LayoutRect): List[SurfaceHitRegion] =
    if entries.isEmpty then Nil
    else
      val allocations = allocate(entries, tabsAvailableWidth(rect))
      val positions   = tabPositions(rect.x, allocations)
      allocations.zip(positions).collect {
        case (allocation, (startX, width)) if width > CloseHitWidth =>
          val closeX = startX + width - CloseHitWidth
          val id     = closeFocusId(allocation.entry.bufferId)
          SurfaceHitRegion(
            rect = LogicalPixelRect(
              closeX.toDouble,
              rect.y.toDouble,
              CloseHitWidth.toDouble,
              math.min(1.0, rect.height.toDouble)
            ),
            focusId = id,
            actionId = Some(SurfaceActionId(id.value)),
            semanticLabel = s"Close ${allocation.entry.title}"
          )
      }
