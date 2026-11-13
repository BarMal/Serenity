package com.serenity.ui.layout

import com.serenity.config.{SurfaceConfig, TextAreaInsets}
import com.serenity.state.models.*

final case class LayoutRect(x: Int, y: Int, width: Int, height: Int):
  def right: Int   = x + width
  def bottom: Int  = y + height
  def centerX: Int = x + width / 2
  def centerY: Int = y + height / 2

  def contains(cellX: Int, cellY: Int): Boolean =
    cellX >= x && cellX < right && cellY >= y && cellY < bottom

  def containsRect(rect: LayoutRect): Boolean =
    rect.x >= x && rect.y >= y && rect.right <= right && rect.bottom <= bottom

final case class EditorPaneLayout(
    paneRect: LayoutRect,
    headerRect: LayoutRect,
    titleRect: LayoutRect,
    contentRect: LayoutRect,
    topSpacerRect: LayoutRect = LayoutRect(0, 0, 0, 0),
    bottomSpacerRect: LayoutRect = LayoutRect(0, 0, 0, 0),
    leftSpacerRect: LayoutRect = LayoutRect(0, 0, 0, 0),
    rightSpacerRect: LayoutRect = LayoutRect(0, 0, 0, 0),
    lineNumberRect: Option[LayoutRect] = None,
    rightLineNumberRect: Option[LayoutRect] = None,
    // The column a terminal pane gives its scrollbar, taken out of `contentRect`; empty in the GUI, where the bar is
    // an overlay (see ScrollbarLayout).
    scrollbarRect: LayoutRect = LayoutRect(0, 0, 0, 0)
):
  def lineNumberRowSlots(itemCount: Int): List[SurfaceContentRowSlot] = rowSlotsFor(lineNumberRect, itemCount)

  def rightLineNumberRowSlots(itemCount: Int): List[SurfaceContentRowSlot] =
    rowSlotsFor(rightLineNumberRect, itemCount)

  private def rowSlotsFor(rect: Option[LayoutRect], itemCount: Int): List[SurfaceContentRowSlot] =
    rect.toList.flatMap(rect =>
      SurfaceFrameLayout.contentRowSlotsFor(content = rect, itemCount = itemCount, hasHeader = false, hasFooter = false)
    )

final case class EditorWorkspaceLayout(
    editorPanelRect: LayoutRect,
    gutterRect: Option[LayoutRect],
    paneLayouts: Map[PaneId, EditorPaneLayout]
):
  def activePaneLayout(state: AppState): Option[EditorPaneLayout] =
    state.persisted.layout.activeEditorPaneId.flatMap(paneLayouts.get)

  def activeHeaderRect(state: AppState): Option[LayoutRect] =
    activePaneLayout(state).map(_.headerRect)

  def activeContentRect(state: AppState): Option[LayoutRect] =
    activePaneLayout(state).map(_.contentRect)

final case class CalculatedLayout(
    editorPanelRect: LayoutRect,
    textAreaInsets: TextAreaInsets = TextAreaInsets(),
    pinnedPanelRects: Map[PanelPosition, LayoutRect] = Map.empty,
    pinnedSurfaceRects: Map[SurfaceId, LayoutRect] = Map.empty,
    floatingPanelRect: Option[LayoutRect] = None,
    expandedPanelRect: Option[LayoutRect] = None,
    aboveCursorOverlayRect: Option[LayoutRect] = None,
    belowCursorOverlayRect: Option[LayoutRect] = None,
    aboveCursorOverlayStack: List[(SurfaceId, LayoutRect)] = Nil,
    belowCursorOverlayStack: List[(SurfaceId, LayoutRect)] = Nil,
    cornerOverlayStack: List[(SurfaceId, LayoutRect)] = Nil,
    collapsedFloatingSurfaceIds: Set[SurfaceId] = Set.empty,
    floatingOverlayOffsetRows: Map[SurfaceId, Double] = Map.empty,
    gutterRect: Option[LayoutRect] = None,
    tabBarRect: Option[LayoutRect] = None
)

object LayoutEngine:

  private[layout] val DefaultSpacerPercentage   = 0.0
  private[layout] val MinimumVerticalPaneHeight = 5
  private[layout] val EditorPaneHeaderHeight    = 1

  /** The always-visible tab strip's fixed height (issue #1074 epic, #1075-1077) -- one row, matching
    * `TabBarSurfaceComposition`'s single-row `Distributed` composition.
    */
  private[layout] val TabBarHeight = 1

  // Public API for panel placement/resize, floating-surface stacking, and pane splitting lives in these
  // sibling objects (600-line architecture ratchet split); exported here so existing `LayoutEngine.foo`
  // call sites keep working unchanged.
  export PinnedPanelLayoutEngine.{pinnedPanelResizeFromDrag, PinnedPanelDragResize}
  export FloatingSurfaceLayout.{resolveFrozenCursorPeekStack, FrozenPeekSlot, FrozenPeekPlacement}
  export OverlayStackLayout.{calculateCornerOverlayStack, CornerPanelSlot, CornerOverlayLayout}
  export EditorPaneLayoutEngine.{
    calculatePaneLayouts,
    calculatePaneLayoutsWithMinWidth,
    calculateEditorPaneLayouts,
    directionalPaneNeighbor,
    calculateEditorWorkspaceLayout,
    calculateEditorPaneLayoutsWithMinWidth
  }

  def calculateLayout(
    state: AppState,
    viewportSize: ViewportSize,
    spacerPercentage: Double = DefaultSpacerPercentage
  ): CalculatedLayout =
    calculateLayoutWithUI(state, viewportSize, spacerPercentage)

  /** The panes of `calculatedLayout` with the text each lays out to, from which a floating surface finds its caret. */
  def laidOutPanes(state: AppState, calculatedLayout: CalculatedLayout): LaidOutPanes =
    LaidOutPanes(state, calculateEditorPaneLayouts(state, calculatedLayout), state.runtime.anchorWraps.lines)

  def calculateLayoutWithUI(
    state: AppState,
    viewportSize: ViewportSize,
    spacerPercentage: Double = DefaultSpacerPercentage
  ): CalculatedLayout =
    state.runtime.chromeLayouts.layoutFor(state, viewportSize, spacerPercentage)(
      calculateLayoutWithUIUncached(state, viewportSize, spacerPercentage)
    )

  private[serenity] def calculateLayoutWithUIUncached(
    state: AppState,
    viewportSize: ViewportSize,
    spacerPercentage: Double
  ): CalculatedLayout =
    val frame         = ChromeFrame.of(state, viewportSize)
    val bounds        = frame.panelBounds
    val contentHeight = bounds.height
    val uiElementGap  = math.ceil(math.max(0.0, state.effectiveUiElementGap)).toInt
    val textAreaInsets =
      if spacerPercentage == DefaultSpacerPercentage then state.persisted.config.surfaceConfig.textAreaInsets.normalized
      else TextAreaInsets.percent(left = spacerPercentage, right = spacerPercentage).normalized
    val minimumEditorWorkspaceWidth =
      val minimumPaneWidth = state.persisted.config.editorConfig.minimumPaneWidth
      state.persisted.layout.editorPanes.keys
        .map(PaneTextArea.minimumWidth(state, _, textAreaInsets, minimumPaneWidth))
        .maxOption
        .getOrElse(minimumPaneWidth.max(1))
    // An expanded/maximized panel (issue #817: `Layout.maximizedWorkspaceNodeId`, not a separate presentation) takes
    // over the whole central editor workspace via `expandedPanelRect` below, so its own dock space is excluded here
    // rather than still being reserved for it as an ordinary docked panel.
    val panelLayoutTree =
      state.persisted.layout.workspaceTree.map { tree =>
        state.expandedPanelSurface.flatMap(surface => tree.removeSurface(surface.id)).getOrElse(tree)
      }
    val pinnedPanelLayout =
      panelLayoutTree
        .map(
          PinnedPanelLayoutEngine.calculateDockedPanelLayout(
            _,
            state.pinnedSurfaces,
            // Bounds start below the reserved tab bar strip (issue #1074/#1075/#1076/#1077), not at the frame's own
            // top -- unlike the bottom gutter (which only shrinks a total nothing sits below), a top strip must shift
            // everything beneath it down by its own height.
            bounds,
            minimumEditorWorkspaceWidth,
            MinimumVerticalPaneHeight,
            uiElementGap
          )
        )
        .getOrElse(PinnedPanelLayoutEngine.PinnedPanelLayout(Map.empty, Map.empty))
    val pinnedPanelRects = pinnedPanelLayout.panelRects

    val topPinnedHeight =
      pinnedPanelRects.get(PanelPosition.Top).map(_.height).getOrElse(0)
    val bottomPinnedHeight =
      pinnedPanelRects.get(PanelPosition.Bottom).map(_.height).getOrElse(0)
    val leftPinnedWidth =
      pinnedPanelRects.get(PanelPosition.Left).map(_.width).getOrElse(0)
    val rightPinnedWidth =
      pinnedPanelRects.get(PanelPosition.Right).map(_.width).getOrElse(0)

    val leftGap   = if leftPinnedWidth > 0 then uiElementGap else 0
    val rightGap  = if rightPinnedWidth > 0 then uiElementGap else 0
    val topGap    = if topPinnedHeight > 0 then uiElementGap else 0
    val bottomGap = if bottomPinnedHeight > 0 then uiElementGap else 0

    val workspaceX = bounds.x + leftPinnedWidth + leftGap
    val workspaceY = bounds.y + topPinnedHeight + topGap
    val workspaceWidth =
      math.max(1, bounds.width - leftPinnedWidth - rightPinnedWidth - leftGap - rightGap)
    val workspaceHeight =
      math.max(1, contentHeight - topPinnedHeight - bottomPinnedHeight - topGap - bottomGap)

    val editorPanelRect = LayoutRect(workspaceX, workspaceY, workspaceWidth, workspaceHeight)

    val gutterRect = frame.gutterRect
    val tabBarRect = frame.tabBarRect

    val baseLayout = CalculatedLayout(
      editorPanelRect = editorPanelRect,
      textAreaInsets = textAreaInsets,
      pinnedPanelRects = pinnedPanelRects,
      pinnedSurfaceRects = pinnedPanelLayout.surfaceRects,
      gutterRect = gutterRect,
      tabBarRect = tabBarRect
    )

    val panes = laidOutPanes(state, baseLayout)
    val textRect = primaryTextRect(state, panes, editorPanelRect)

    val aboveSurfaces = OverlayStackLayout.orderedAboveCursorSurfaces(state)
    val belowSurfaces = OverlayStackLayout.orderedBelowCursorSurfaces(state)
    val aboveLayout =
      OverlayStackLayout.calculateAboveCursorOverlayStack(aboveSurfaces, state, panes)
    val aboveCursorOverlayStack = aboveLayout.stack
    val belowLayout             = OverlayStackLayout.calculateBelowCursorOverlayStack(belowSurfaces, state, panes)
    val floatingOffsets = OverlayStackLayout.floatingOverlayOffsets(
      aboveSurfaces,
      aboveCursorOverlayStack,
      belowSurfaces,
      belowLayout.stack,
      state,
      panes
    )

    baseLayout.copy(
      expandedPanelRect = state.expandedPanelSurface.map(_ => textRect),
      aboveCursorOverlayRect = aboveCursorOverlayStack.headOption.map(_._2),
      belowCursorOverlayRect = belowLayout.stack.headOption.map(_._2),
      aboveCursorOverlayStack = aboveCursorOverlayStack,
      belowCursorOverlayStack = belowLayout.stack,
      cornerOverlayStack = OverlayStackLayout.calculateCornerOverlays(state, textRect) ++
        OverlayStackLayout.calculateTopEdgeOverlays(state, viewportSize),
      collapsedFloatingSurfaceIds = aboveLayout.collapsedSurfaceIds ++ belowLayout.collapsedSurfaceIds,
      floatingOverlayOffsetRows = floatingOffsets
    )

  /** Center a blocking dialog in the editor workspace without changing workspace allocation. */
  def calculateModalRect(dialog: ModalDialog, state: AppState, layout: CalculatedLayout): LayoutRect =
    val bounds    = layout.editorPanelRect
    val width     = math.max(3, math.min(72, bounds.width))
    val rawHeight = ModalSurfaceComposition.frameHeightFor(dialog.modal, width, state)
    val height    = math.max(3, math.min(bounds.height, rawHeight))
    LayoutRect(
      x = bounds.x + math.max(0, (bounds.width - width) / 2),
      y = bounds.y + math.max(0, (bounds.height - height) / 2),
      width = width,
      height = height
    )

  private[layout] def usesBottomGutter(state: AppState): Boolean =
    state.chrome.statusLine.isPinned

  /** Whether the always-visible tab strip (issue #1074 epic, #1075-1077) reserves its own row. Only once 2+ buffers are
    * open -- with a single buffer there is nothing to switch between, so no strip is reserved or painted (issue #1074
    * decision), and never with `editor.tab_strip = off`.
    */
  private[layout] def showsTabBar(state: AppState): Boolean =
    state.showsTabStrip

  private[layout] def paneHeaderHeight(state: AppState): Int =
    if state.chrome.paneHeaders then EditorPaneHeaderHeight else 0

  /** What a modal or corner overlay centres in or anchors to: the active pane's text across the workspace's height,
    * which is the editor panel itself while one pane fills it.
    */
  private def primaryTextRect(state: AppState, panes: LaidOutPanes, workspace: LayoutRect): LayoutRect =
    state.persisted.layout.activeEditorPaneId
      .flatMap(panes.layouts.get)
      .orElse(panes.layouts.values.headOption)
      .fold(workspace)(pane =>
        LayoutRect(pane.contentRect.x, workspace.y, pane.contentRect.width + pane.scrollbarRect.width, workspace.height)
      )

  /** The gutter counter's width in cells: digits for the largest line number across all buffers, plus one spacer cell,
    * minimum three. Shared by the single-column rail's own reservation and the per-column e-reader rail
    * ([[perColumnGutterWidth]]), so both rails size identically.
    */
  def lineNumberCounterWidth(state: AppState): Int =
    val maxLines =
      if state.persisted.buffers.isEmpty then 10
      else state.persisted.buffers.values.map(_.document.content.lineCount).foldLeft(Int.MinValue)(_ max _)

    math.max(3, maxLines.toString.length + 1) // +1 for spacing, minimum 3 chars

  /** Multi-column e-reader layout (issue #1338, Phase 2 / slice 2): the width in cells each column reserves on its own
    * left edge for its line-number rail -- the shared counter width while line numbers and column mode are both on,
    * zero otherwise. The single pane-level rail's reservation is dropped in this case (see `calculateLayoutWithUI`), so
    * a column's text wraps in `columnWidthCells - perColumnGutterWidth`.
    */
  def perColumnGutterWidth(state: AppState): Int =
    val surfaceConfig    = state.persisted.config.surfaceConfig
    val columnModeActive = surfaceConfig.columnModeEnabled && surfaceConfig.wordWrapEnabled
    if state.chrome.lineNumbers && columnModeActive then lineNumberCounterWidth(state) else 0

  def calculateViewportForCursor(
    cursor: CursorPosition,
    viewport: Viewport,
    bufferLineCount: Int
  ): Viewport =
    val targetCenterLine  = viewport.visibleLines / 2
    val desiredScrollLine = cursor.line - targetCenterLine

    // Clamp scroll position to valid bounds
    val maxScrollLine     = math.max(0, bufferLineCount - viewport.visibleLines)
    val clampedScrollLine = math.max(0, math.min(desiredScrollLine, maxScrollLine))

    viewport.scrolledTo(clampedScrollLine, viewport.leftColumn, 0)

  def updateViewportDimensions(viewport: Viewport, panelRect: LayoutRect): Viewport =
    viewport.copy(
      visibleLines = panelRect.height,
      visibleColumns = panelRect.width
    )

  def updateBufferViewportDimensions(buffer: Buffer, panelRect: LayoutRect, wordWrapEnabled: Boolean): Viewport =
    updateBufferViewportDimensions(
      buffer,
      panelRect,
      wordWrapEnabled,
      columnModeEnabled = false,
      columnTargetWidthCells = SurfaceConfig().columnTargetWidthCells,
      columnGap = SurfaceConfig().columnGap
    )

  /** Column-based document layout (issue #1338, Phase 1): "as many columns as fit" at a configured target width, not a
    * fixed user-picked count -- only takes effect while `wordWrapEnabled` is also on, otherwise this is exactly the
    * three-argument overload above. Slice 4: delegates to the count-driven [[resolvedColumnCount]] seam via a
    * `SurfaceConfig` carrying these legacy width/gap args in Auto mode (`columnCount = None`), so the width-driven
    * result is byte-for-byte the same as before while all the real resolve logic lives in the one seam.
    */
  def updateBufferViewportDimensions(
    buffer: Buffer,
    panelRect: LayoutRect,
    wordWrapEnabled: Boolean,
    columnModeEnabled: Boolean,
    columnTargetWidthCells: Option[Int],
    columnGap: Int
  ): Viewport =
    updateBufferViewportDimensions(
      buffer,
      panelRect,
      wordWrapEnabled,
      columnModeEnabled,
      SurfaceConfig(columnTargetWidthCells = columnTargetWidthCells, columnGap = columnGap)
    )

  /** Multi-column e-reader layout (issue #1338, Phase 2 / slice 4): the count-driven seam. The number of columns comes
    * from [[resolvedColumnCount]] (Auto reproduces the width-driven count; `Some(n)` pins n, clamped to the pane), and
    * a single column's text width from [[columnWidthCellsForCount]]. Only takes effect while `columnModeEnabled &&
    * wordWrapEnabled`, otherwise this is the plain full-width viewport sizing.
    */
  def updateBufferViewportDimensions(
    buffer: Buffer,
    panelRect: LayoutRect,
    wordWrapEnabled: Boolean,
    columnModeEnabled: Boolean,
    surfaceConfig: SurfaceConfig
  ): Viewport =
    val effectivePanelRect =
      if columnModeEnabled && wordWrapEnabled then
        val count = resolvedColumnCount(panelRect.width, surfaceConfig)
        panelRect.copy(width = columnWidthCellsForCount(panelRect.width, count, surfaceConfig.columnGap))
      else panelRect
    val resizedViewport = updateViewportDimensions(buffer.viewport, effectivePanelRect)
    val clampedLeftColumn =
      if wordWrapEnabled then 0
      else clampLeftColumnForBuffer(buffer, resizedViewport)

    resizedViewport.copy(leftColumn = clampedLeftColumn)

  /** How many columns of `columnTargetWidthCells` (plus `columnGap` between them) fit `contentWidthCells` -- e-reader
    * style "as many as fit", never fewer than one.
    */
  def columnCount(contentWidthCells: Int, columnTargetWidthCells: Int, columnGap: Int): Int =
    columnCount(contentWidthCells, Some(columnTargetWidthCells), columnGap)

  /** No target width means unlimited: a single column fills the pane. */
  def columnCount(contentWidthCells: Int, columnTargetWidthCells: Option[Int], columnGap: Int): Int =
    columnTargetWidthCells.fold(1) { targetCells =>
      val gap    = columnGap.max(0)
      val target = targetCells.max(1)
      math.max(1, (contentWidthCells + gap) / (target + gap))
    }

  /** The width of a single column once `columnCount` columns (and the gaps between them) are fitted into
    * `contentWidthCells`.
    */
  def columnWidthCells(contentWidthCells: Int, columnTargetWidthCells: Int, columnGap: Int): Int =
    columnWidthCells(contentWidthCells, Some(columnTargetWidthCells), columnGap)

  def columnWidthCells(contentWidthCells: Int, columnTargetWidthCells: Option[Int], columnGap: Int): Int =
    val gap   = columnGap.max(0)
    val count = columnCount(contentWidthCells, columnTargetWidthCells, columnGap)
    math.max(1, (contentWidthCells - (count - 1) * gap) / count)

  /** Multi-column e-reader layout (issue #1338, Phase 2 / slice 4): the anti-absurdity floor on a column's text width
    * in cells -- NOT a readability target (that is `columnTargetWidthCells`, which drives Auto mode), just the point
    * below which pinning yet another column would leave each one too narrow to hold anything meaningful. It exists only
    * to bound `columnCount = Some(n)` against the pane in `maxColumnsThatFit`/`resolvedColumnCount`.
    */
  val MinColumnTextWidthCells = 8

  /** The most columns `contentWidthCells` can hold before each would fall below the [[MinColumnTextWidthCells]] floor
    * -- how many columns of `(MinColumnTextWidthCells + gap)` fit, mirroring [[columnCount]]'s fit math, never fewer
    * than one. The upper clamp `resolvedColumnCount` applies to an explicit `columnCount = Some(n)`.
    */
  def maxColumnsThatFit(contentWidthCells: Int, gap: Int): Int =
    val clampedGap = gap.max(0)
    math.max(1, (contentWidthCells + clampedGap) / (MinColumnTextWidthCells + clampedGap))

  /** The single seam every column-count caller routes through. Auto (`surfaceConfig.columnCount = None`) reproduces the
    * width-driven [[columnCount]] exactly; an explicit `Some(n)` pins n, clamped to `[1, maxColumnsThatFit]` so an
    * absurd count can never leave columns below the anti-absurdity floor. A zero-width pane (no laid-out window yet)
    * collapses to a single column on either path, since `maxColumnsThatFit` is then one.
    */
  def resolvedColumnCount(contentWidthCells: Int, surfaceConfig: SurfaceConfig): Int =
    val requested =
      surfaceConfig.columnCount.getOrElse(
        columnCount(contentWidthCells, surfaceConfig.columnTargetWidthCells, surfaceConfig.columnGap)
      )
    requested.max(1).min(maxColumnsThatFit(contentWidthCells, surfaceConfig.columnGap))

  /** The width of one column when exactly `count` columns (and the `count - 1` gaps between them) are fitted into
    * `contentWidthCells` -- the count-driven counterpart to [[columnWidthCells]], which derives the count itself.
    */
  def columnWidthCellsForCount(contentWidthCells: Int, count: Int, gap: Int): Int =
    val clampedGap   = gap.max(0)
    val clampedCount = count.max(1)
    math.max(1, (contentWidthCells - (clampedCount - 1) * clampedGap) / clampedCount)

  private def clampLeftColumnForBuffer(buffer: Buffer, viewport: Viewport): Int =
    val visibleColumns = math.max(1, viewport.visibleColumns)
    val cursor         = buffer.editing.cursorPositions.headOption.getOrElse(CursorPosition(viewport.topLine, 0))
    val cursorColumn   = cursor.column.max(0)
    val lineLength     = buffer.document.content.getLine(cursor.line).map(_.length).getOrElse(cursorColumn)
    val maxForCursor   = math.max(0, cursorColumn - visibleColumns + 1)
    val maxForLine     = math.max(0, lineLength - visibleColumns + 1)

    viewport.leftColumn.max(0).min(maxForCursor).min(maxForLine)

  def updateViewportDimensions(viewport: Viewport, panelRect: LayoutRect, metrics: CellMetrics): Viewport =
    viewport.copy(
      visibleLines = panelRect.height / metrics.lineHeight,
      visibleColumns = panelRect.width / metrics.charWidth
    )

  def syncViewportDimensions(state: AppState, viewportSize: ViewportSize): AppState =
    val calculatedLayout = calculateLayout(state, viewportSize)
    val workspaceLayout  = calculateEditorWorkspaceLayout(state, calculatedLayout)
    val (updatedBuffers, updatedPanes) =
      state.persisted.layout.editorPanes.foldLeft((state.persisted.buffers, state.persisted.layout.editorPanes)) {
        case ((buffers, panes), (paneId, pane)) =>
          val paneRect =
            workspaceLayout.paneLayouts.get(paneId).map(_.paneRect).getOrElse(calculatedLayout.editorPanelRect)
          val contentRect   = workspaceLayout.paneLayouts.get(paneId).map(_.contentRect).getOrElse(paneRect)
          val paneViewport  = updateViewportDimensions(pane.viewport, contentRect)
          val nextPanes     = panes + (paneId -> pane.copy(viewport = paneViewport))
          val surfaceConfig = state.persisted.config.surfaceConfig
          val updatedBuffer = pane.bufferId.flatMap(buffers.get).map { buffer =>
            buffer.id -> buffer
              .copy(viewport =
                updateBufferViewportDimensions(
                  buffer,
                  contentRect,
                  surfaceConfig.wordWrapEnabled,
                  columnModeEnabled = surfaceConfig.columnModeEnabled,
                  surfaceConfig
                )
              )
          }
          val nextBuffers = updatedBuffer.fold(buffers)(buffers + _)

          (nextBuffers, nextPanes)
      }

    state.copy(
      persisted = state.persisted.copy(
        buffers = updatedBuffers,
        layout = state.persisted.layout.copy(editorPanes = updatedPanes)
      )
    )
