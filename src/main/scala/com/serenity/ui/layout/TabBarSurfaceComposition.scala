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
      val allocations = allocate(entries, rect.width)
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
        paintBoxes = List(rowBox),
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
