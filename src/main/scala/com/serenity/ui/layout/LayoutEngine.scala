package com.serenity.ui.layout

import com.serenity.config.{InterfaceDensityMetrics, SurfaceConfig, TextAreaInsets}
import com.serenity.state.models.*

final case class ViewportSize(width: Int, height: Int)

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
    bottomSpacerRect: LayoutRect = LayoutRect(0, 0, 0, 0)
)

final case class EditorWorkspaceLayout(
    editorPanelRect: LayoutRect,
    lineNumberRect: Option[LayoutRect],
    gutterRect: Option[LayoutRect],
    paneLayouts: Map[PaneId, EditorPaneLayout],
    rightLineNumberRect: Option[LayoutRect] = None
):
  def activePaneLayout(state: AppState): Option[EditorPaneLayout] =
    state.persisted.layout.activeEditorPaneId.flatMap(paneLayouts.get)

  def activeHeaderRect(state: AppState): Option[LayoutRect] =
    activePaneLayout(state).map(_.headerRect)

  def activeContentRect(state: AppState): Option[LayoutRect] =
    activePaneLayout(state).map(_.contentRect)

  def lineNumberRowSlots(itemCount: Int): List[SurfaceContentRowSlot] =
    rowSlotsFor(lineNumberRect, itemCount)

  def rightLineNumberRowSlots(itemCount: Int): List[SurfaceContentRowSlot] =
    rowSlotsFor(rightLineNumberRect, itemCount)

  private def rowSlotsFor(rect: Option[LayoutRect], itemCount: Int): List[SurfaceContentRowSlot] =
    rect.toList.flatMap(rect =>
      SurfaceFrameLayout.contentRowSlotsFor(
        content = rect,
        itemCount = itemCount,
        hasHeader = false,
        hasFooter = false
      )
    )

final case class CalculatedLayout(
    editorPanelRect: LayoutRect,
    leftSpacerRect: LayoutRect,
    rightSpacerRect: LayoutRect,
    topSpacerRect: LayoutRect = LayoutRect(0, 0, 0, 0),
    bottomSpacerRect: LayoutRect = LayoutRect(0, 0, 0, 0),
    pinnedPanelRects: Map[PanelPosition, LayoutRect] = Map.empty,
    pinnedSurfaceRects: Map[SurfaceId, LayoutRect] = Map.empty,
    floatingPanelRect: Option[LayoutRect] = None,
    expandedPanelRect: Option[LayoutRect] = None,
    aboveCursorOverlayRect: Option[LayoutRect] = None,
    belowCursorOverlayRect: Option[LayoutRect] = None,
    aboveCursorOverlayStack: List[(SurfaceId, LayoutRect)] = Nil,
    belowCursorOverlayStack: List[(SurfaceId, LayoutRect)] = Nil,
    collapsedFloatingSurfaceIds: Set[SurfaceId] = Set.empty,
    floatingOverlayOffsetRows: Map[SurfaceId, Double] = Map.empty,
    lineNumberRect: Option[LayoutRect] = None,
    rightLineNumberRect: Option[LayoutRect] = None,
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

  def calculateLayoutWithUI(
    state: AppState,
    viewportSize: ViewportSize,
    spacerPercentage: Double = DefaultSpacerPercentage
  ): CalculatedLayout =
    val densityMetrics = InterfaceDensityMetrics.forDensity(state.persisted.config.interfaceDensity)
    val gutterHeight   = if usesBottomGutter(state) then densityMetrics.gutterHeight else 0
    val tabBarHeight   = if showsTabBar(state) then TabBarHeight else 0
    val contentHeight  = math.max(1, viewportSize.height - gutterHeight - tabBarHeight)
    val uiElementGap   = math.ceil(math.max(0.0, state.persisted.config.uiElementGap)).toInt
    val textAreaInsets =
      if spacerPercentage == DefaultSpacerPercentage then state.persisted.config.surfaceConfig.textAreaInsets.normalized
      else TextAreaInsets(spacerPercentage, spacerPercentage).normalized
    val lineNumbersOn    = state.persisted.config.surfaceConfig.showLineNumbers
    val lineNumberLayout = state.persisted.config.surfaceConfig.lineNumberLayout.normalized
    val counterWidth     = if lineNumbersOn then calculateLineNumberWidth(state) else 0
    val hasLeftCounter   = lineNumbersOn && lineNumberLayout.side.showsLeft
    val hasRightCounter  = lineNumbersOn && lineNumberLayout.side.showsRight
    // Margins apply to their side while line numbers are enabled -- even a side with no counter -- so toggling a
    // side's counter does not shift content. Padding sits between a counter and the content, so only on counter sides.
    val leftBlock =
      if lineNumbersOn then
        lineNumberLayout.marginLeft + (if hasLeftCounter then counterWidth + lineNumberLayout.padding else 0)
      else 0
    val rightBlock =
      if lineNumbersOn then
        lineNumberLayout.marginRight + (if hasRightCounter then counterWidth + lineNumberLayout.padding else 0)
      else 0
    val horizontalTextFraction = (1.0 - textAreaInsets.left - textAreaInsets.right).max(0.01)
    val minimumEditorWorkspaceWidth =
      math
        .ceil(
          (state.persisted.config.editorConfig.minimumPaneWidth
            .max(1) + leftBlock + rightBlock) / horizontalTextFraction
        )
        .toInt
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
            LayoutRect(0, tabBarHeight, viewportSize.width, contentHeight),
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

    val workspaceX = leftPinnedWidth + leftGap
    val workspaceY = tabBarHeight + topPinnedHeight + topGap
    val workspaceWidth =
      math.max(1, viewportSize.width - leftPinnedWidth - rightPinnedWidth - leftGap - rightGap)
    val workspaceHeight =
      math.max(1, contentHeight - topPinnedHeight - bottomPinnedHeight - topGap - bottomGap)

    val editorPaneHeaderHeight = paneHeaderHeight(state)
    val leftSpacerWidth        = (workspaceWidth * textAreaInsets.left).toInt
    val rightSpacerWidth       = (workspaceWidth * textAreaInsets.right).toInt
    val contentAreaHeight      = math.max(1, workspaceHeight - editorPaneHeaderHeight)
    val topSpacerHeight        = (contentAreaHeight * textAreaInsets.top).toInt
    val bottomSpacerHeight     = (contentAreaHeight * textAreaInsets.bottom).toInt

    // Adjust editor area to accommodate UI elements
    val availableWidth  = math.max(1, workspaceWidth - leftSpacerWidth - rightSpacerWidth - leftBlock - rightBlock)
    val availableHeight = workspaceHeight

    val lineNumberY      = workspaceY + editorPaneHeaderHeight + topSpacerHeight
    val lineNumberHeight = math.max(1, contentAreaHeight - topSpacerHeight - bottomSpacerHeight)
    val editorPanelX     = workspaceX + leftSpacerWidth + leftBlock

    val leftSpacerRect = LayoutRect(workspaceX, workspaceY, leftSpacerWidth, availableHeight)
    val lineNumberRect =
      if hasLeftCounter then
        Some(
          LayoutRect(
            workspaceX + leftSpacerWidth + lineNumberLayout.marginLeft,
            lineNumberY,
            counterWidth,
            lineNumberHeight
          )
        )
      else None
    val rightLineNumberRect =
      if hasRightCounter then
        Some(
          LayoutRect(
            editorPanelX + availableWidth + lineNumberLayout.padding,
            lineNumberY,
            counterWidth,
            lineNumberHeight
          )
        )
      else None

    val textBandWidth = leftBlock + availableWidth + rightBlock
    val topSpacerRect = LayoutRect(
      workspaceX + leftSpacerWidth,
      workspaceY + editorPaneHeaderHeight,
      textBandWidth,
      topSpacerHeight
    )
    val bottomSpacerRect = LayoutRect(
      workspaceX + leftSpacerWidth,
      workspaceY + editorPaneHeaderHeight + topSpacerHeight + math.max(
        1,
        contentAreaHeight - topSpacerHeight - bottomSpacerHeight
      ),
      textBandWidth,
      bottomSpacerHeight
    )
    val editorPanelRect = LayoutRect(
      x = editorPanelX,
      y = workspaceY,
      width = availableWidth,
      height = availableHeight
    )
    val rightSpacerRect =
      LayoutRect(
        editorPanelX + availableWidth + rightBlock,
        workspaceY,
        rightSpacerWidth,
        availableHeight
      )

    val gutterRect =
      if usesBottomGutter(state) then
        Some(LayoutRect(0, viewportSize.height - gutterHeight, viewportSize.width, gutterHeight))
      else None

    val tabBarRect =
      if tabBarHeight > 0 then Some(LayoutRect(0, 0, viewportSize.width, tabBarHeight)) else None

    val baseLayout = CalculatedLayout(
      editorPanelRect = editorPanelRect,
      leftSpacerRect = leftSpacerRect,
      rightSpacerRect = rightSpacerRect,
      topSpacerRect = topSpacerRect,
      bottomSpacerRect = bottomSpacerRect,
      pinnedPanelRects = pinnedPanelRects,
      pinnedSurfaceRects = pinnedPanelLayout.surfaceRects,
      expandedPanelRect = state.expandedPanelSurface.map(_ => editorPanelRect),
      lineNumberRect = lineNumberRect,
      rightLineNumberRect = rightLineNumberRect,
      gutterRect = gutterRect,
      tabBarRect = tabBarRect
    )

    val paneLayouts = calculateEditorPaneLayouts(state, baseLayout)

    val aboveSurfaces = state.floatingSurfaces.filter {
      _.presentation match
        case SurfacePresentation.Floating(_, SurfacePlacement.AboveCursor) => true
        case _                                                             => false
    }
    val belowSurfaces = OverlayStackLayout.orderedBelowCursorSurfaces(state)
    val aboveCursorOverlayStack =
      aboveSurfaces.flatMap(surface =>
        FloatingSurfaceLayout.calculateFloatingSurfaceRect(surface, state, paneLayouts).map(surface.id -> _)
      )
    val belowLayout = OverlayStackLayout.calculateBelowCursorOverlayStack(belowSurfaces, state, paneLayouts)
    val floatingOffsets = OverlayStackLayout.floatingOverlayOffsets(
      aboveSurfaces,
      aboveCursorOverlayStack,
      belowSurfaces,
      belowLayout.stack,
      state,
      paneLayouts
    )

    baseLayout.copy(
      aboveCursorOverlayRect = aboveCursorOverlayStack.headOption.map(_._2),
      belowCursorOverlayRect = belowLayout.stack.headOption.map(_._2),
      aboveCursorOverlayStack = aboveCursorOverlayStack,
      belowCursorOverlayStack = belowLayout.stack,
      collapsedFloatingSurfaceIds = belowLayout.collapsedSurfaceIds,
      floatingOverlayOffsetRows = floatingOffsets
    )

  /** Center a blocking dialog in the editor workspace without changing workspace allocation. */
  def calculateModalRect(dialog: ModalDialog, state: AppState, layout: CalculatedLayout): LayoutRect =
    val bounds = layout.editorPanelRect
    val width  = math.max(3, math.min(72, bounds.width))
    val rawHeight = ModalSurfaceComposition.frameHeight(
      dialog.modal,
      SurfaceFrameLayout.minimumTargetRows(state.persisted.config.interfaceDensity)
    )
    val height = math.max(3, math.min(bounds.height, rawHeight))
    LayoutRect(
      x = bounds.x + math.max(0, (bounds.width - width) / 2),
      y = bounds.y + math.max(0, (bounds.height - height) / 2),
      width = width,
      height = height
    )

  private[layout] def usesBottomGutter(state: AppState): Boolean =
    state.persisted.config.statusLine.isPinned

  /** Whether the always-visible tab strip (issue #1074 epic, #1075-1077) reserves its own row. Only once 2+ buffers are
    * open -- with a single buffer there is nothing to switch between, so no strip is reserved or painted (issue #1074
    * decision).
    */
  private[layout] def showsTabBar(state: AppState): Boolean =
    state.persisted.bufferOrder.size >= 2

  private[layout] def paneHeaderHeight(state: AppState): Int =
    if state.persisted.config.surfaceConfig.showPaneHeaders then EditorPaneHeaderHeight else 0

  private def calculateLineNumberWidth(state: AppState): Int =
    // Find the maximum line count across all buffers to determine width needed
    val maxLines =
      if state.persisted.buffers.isEmpty then 10
      else state.persisted.buffers.values.map(_.document.content.lineCount).foldLeft(Int.MinValue)(_ max _)

    math.max(3, maxLines.toString.length + 1) // +1 for spacing, minimum 3 chars

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

    viewport.copy(topLine = clampedScrollLine, topVisualLine = 0)

  def updateViewportDimensions(viewport: Viewport, panelRect: LayoutRect): Viewport =
    viewport.copy(
      visibleLines = panelRect.height,
      visibleColumns = panelRect.width,
      topVisualLine = viewport.topVisualLine.min(math.max(0, panelRect.height - 1))
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
    * three-argument overload above.
    */
  def updateBufferViewportDimensions(
    buffer: Buffer,
    panelRect: LayoutRect,
    wordWrapEnabled: Boolean,
    columnModeEnabled: Boolean,
    columnTargetWidthCells: Int,
    columnGap: Int
  ): Viewport =
    val effectivePanelRect =
      if columnModeEnabled && wordWrapEnabled then
        panelRect.copy(width = columnWidthCells(panelRect.width, columnTargetWidthCells, columnGap))
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
    val gap    = columnGap.max(0)
    val target = columnTargetWidthCells.max(1)
    math.max(1, (contentWidthCells + gap) / (target + gap))

  /** The width of a single column once `columnCount` columns (and the gaps between them) are fitted into
    * `contentWidthCells`.
    */
  def columnWidthCells(contentWidthCells: Int, columnTargetWidthCells: Int, columnGap: Int): Int =
    val gap   = columnGap.max(0)
    val count = columnCount(contentWidthCells, columnTargetWidthCells, columnGap)
    math.max(1, (contentWidthCells - (count - 1) * gap) / count)

  private def clampLeftColumnForBuffer(buffer: Buffer, viewport: Viewport): Int =
    val visibleColumns = math.max(1, viewport.visibleColumns)
    val cursor         = buffer.editing.cursors.headOption.getOrElse(CursorPosition(viewport.topLine, 0))
    val cursorColumn   = cursor.column.max(0)
    val lineLength     = buffer.document.content.getLine(cursor.line).map(_.length).getOrElse(cursorColumn)
    val maxForCursor   = math.max(0, cursorColumn - visibleColumns + 1)
    val maxForLine     = math.max(0, lineLength - visibleColumns + 1)

    viewport.leftColumn.max(0).min(maxForCursor).min(maxForLine)

  def updateViewportDimensions(viewport: Viewport, panelRect: LayoutRect, metrics: CellMetrics): Viewport =
    viewport.copy(
      visibleLines = panelRect.height / metrics.lineHeight,
      visibleColumns = panelRect.width / metrics.charWidth,
      topVisualLine = viewport.topVisualLine.min(math.max(0, panelRect.height / metrics.lineHeight - 1))
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
                  columnTargetWidthCells = surfaceConfig.columnTargetWidthCells,
                  columnGap = surfaceConfig.columnGap
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
