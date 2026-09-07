package com.serenity.ui.layout

import com.serenity.config.{InterfaceDensityMetrics, TextAreaInsets}
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
    paneLayouts: Map[PaneId, EditorPaneLayout]
):
  def activePaneLayout(state: AppState): Option[EditorPaneLayout] =
    state.persisted.layout.activeEditorPaneId.flatMap(paneLayouts.get)

  def activeHeaderRect(state: AppState): Option[LayoutRect] =
    activePaneLayout(state).map(_.headerRect)

  def activeContentRect(state: AppState): Option[LayoutRect] =
    activePaneLayout(state).map(_.contentRect)

  def lineNumberRowSlots(itemCount: Int): List[SurfaceContentRowSlot] =
    lineNumberRect.toList.flatMap(rect =>
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
    gutterRect: Option[LayoutRect] = None
)

object LayoutManager:

  def calculateLayout(
    state: AppState,
    viewportSize: ViewportSize,
    spacerPercentage: Double = LayoutEngine.DefaultSpacerPercentage
  ): CalculatedLayout =
    LayoutEngine.calculateLayout(state, viewportSize, spacerPercentage)

object LayoutEngine:

  private[layout] val DefaultSpacerPercentage   = 0.0
  private[layout] val MinimumVerticalPaneHeight = 5
  private[layout] val EditorPaneHeaderHeight    = 1

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
    val contentHeight  = math.max(1, viewportSize.height - gutterHeight)
    val uiElementGap   = math.ceil(math.max(0.0, state.persisted.config.uiElementGap)).toInt
    val textAreaInsets =
      if spacerPercentage == DefaultSpacerPercentage then state.persisted.config.surfaceConfig.textAreaInsets.normalized
      else TextAreaInsets(spacerPercentage, spacerPercentage).normalized
    val lineNumberWidth =
      if state.persisted.config.surfaceConfig.showLineNumbers then calculateLineNumberWidth(state)
      else 0
    val horizontalTextFraction = (1.0 - textAreaInsets.left - textAreaInsets.right).max(0.01)
    val minimumEditorWorkspaceWidth =
      math
        .ceil((state.persisted.config.editorConfig.minimumPaneWidth.max(1) + lineNumberWidth) / horizontalTextFraction)
        .toInt
    val pinnedPanelLayout =
      state.persisted.layout.workspaceTree
        .filter(_.dockedSurfaceIds.nonEmpty)
        .map(
          PinnedPanelLayoutEngine.calculateDockedPanelLayout(
            _,
            state.pinnedSurfaces,
            LayoutRect(0, 0, viewportSize.width, contentHeight),
            minimumEditorWorkspaceWidth,
            MinimumVerticalPaneHeight,
            uiElementGap
          )
        )
        .getOrElse(
          PinnedPanelLayoutEngine.calculatePinnedPanelLayout(
            state.pinnedSurfaces,
            viewportSize.width,
            contentHeight,
            uiElementGap
          )
        )
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
    val workspaceY = topPinnedHeight + topGap
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
    val availableWidth  = math.max(1, workspaceWidth - leftSpacerWidth - rightSpacerWidth - lineNumberWidth)
    val availableHeight = workspaceHeight

    val leftSpacerRect = LayoutRect(workspaceX, workspaceY, leftSpacerWidth, availableHeight)
    val lineNumberRect =
      if state.persisted.config.surfaceConfig.showLineNumbers then
        val lineNumberY      = workspaceY + editorPaneHeaderHeight + topSpacerHeight
        val lineNumberHeight = math.max(1, contentAreaHeight - topSpacerHeight - bottomSpacerHeight)
        Some(
          LayoutRect(workspaceX + leftSpacerWidth, lineNumberY, lineNumberWidth, lineNumberHeight)
        )
      else None

    val topSpacerRect = LayoutRect(
      workspaceX + leftSpacerWidth,
      workspaceY + editorPaneHeaderHeight,
      lineNumberWidth + availableWidth,
      topSpacerHeight
    )
    val bottomSpacerRect = LayoutRect(
      workspaceX + leftSpacerWidth,
      workspaceY + editorPaneHeaderHeight + topSpacerHeight + math.max(
        1,
        contentAreaHeight - topSpacerHeight - bottomSpacerHeight
      ),
      lineNumberWidth + availableWidth,
      bottomSpacerHeight
    )
    val editorPanelRect = LayoutRect(
      x = workspaceX + leftSpacerWidth + lineNumberWidth,
      y = workspaceY,
      width = availableWidth,
      height = availableHeight
    )
    val rightSpacerRect =
      LayoutRect(
        workspaceX + leftSpacerWidth + lineNumberWidth + availableWidth,
        workspaceY,
        rightSpacerWidth,
        availableHeight
      )

    val gutterRect =
      if usesBottomGutter(state) then
        Some(LayoutRect(0, viewportSize.height - gutterHeight, viewportSize.width, gutterHeight))
      else None

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
      gutterRect = gutterRect
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
  def calculateModalRect(surface: UiSurface, state: AppState, layout: CalculatedLayout): LayoutRect =
    val bounds = layout.editorPanelRect
    val width  = math.max(3, math.min(72, bounds.width))
    val height = FloatingSurfaceLayout.calculateFloatingSurfaceHeight(surface.content, width, bounds.height, state)
    LayoutRect(
      x = bounds.x + math.max(0, (bounds.width - width) / 2),
      y = bounds.y + math.max(0, (bounds.height - height) / 2),
      width = width,
      height = height
    )

  private[layout] def usesBottomGutter(state: AppState): Boolean =
    state.persisted.config.surfaceConfig.showGutter ||
      (state.persisted.config.cursorInfoBarSegments.nonEmpty &&
        state.persisted.config.cursorInfoBarPlacement == com.serenity.config.CursorInfoBarPlacement.PinnedBottom)

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
    val resizedViewport = updateViewportDimensions(buffer.viewport, panelRect)
    val clampedLeftColumn =
      if wordWrapEnabled then 0
      else clampLeftColumnForBuffer(buffer, resizedViewport)

    resizedViewport.copy(leftColumn = clampedLeftColumn)

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
          val contentRect  = workspaceLayout.paneLayouts.get(paneId).map(_.contentRect).getOrElse(paneRect)
          val paneViewport = updateViewportDimensions(pane.viewport, contentRect)
          val nextPanes    = panes + (paneId -> pane.copy(viewport = paneViewport))
          val updatedBuffer = pane.bufferId.flatMap(buffers.get).map { buffer =>
            buffer.id -> buffer
              .copy(viewport =
                updateBufferViewportDimensions(
                  buffer,
                  contentRect,
                  state.persisted.config.surfaceConfig.wordWrapEnabled
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
