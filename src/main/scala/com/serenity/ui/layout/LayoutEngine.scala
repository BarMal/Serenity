package com.serenity.ui.layout

import com.serenity.state.models.*

case class ViewportSize(width: Int, height: Int)

case class LayoutRect(x: Int, y: Int, width: Int, height: Int):
  def right: Int   = x + width
  def bottom: Int  = y + height
  def centerX: Int = x + width / 2
  def centerY: Int = y + height / 2

case class CalculatedLayout(
    editorPanelRect: LayoutRect,
    leftSpacerRect: LayoutRect,
    rightSpacerRect: LayoutRect,
    pinnedPanelRects: Map[PanelPosition, LayoutRect] = Map.empty,
    floatingPanelRect: Option[LayoutRect] = None,
    aboveCursorOverlayRect: Option[LayoutRect] = None,
    belowCursorOverlayRect: Option[LayoutRect] = None,
    lineNumberRect: Option[LayoutRect] = None,
    gutterRect: Option[LayoutRect] = None
)

object LayoutManager:

  def calculateLayout(
    state: AppState,
    viewportSize: ViewportSize
  ): Unit = ()

object LayoutEngine:

  // Default spacer width as percentage of terminal width (15% each side = 30% total)
  private val DefaultSpacerPercentage = 0.15

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
    val gutterHeight = if state.config.showGutter then 1 else 0
    val contentHeight = math.max(1, viewportSize.height - gutterHeight)
    val pinnedPanelRects = calculatePinnedPanelRects(
      state.pinnedSurfaces,
      viewportSize.width,
      contentHeight
    )

    val topPinnedHeight =
      pinnedPanelRects.get(PanelPosition.Top).map(_.height).getOrElse(0)
    val bottomPinnedHeight =
      pinnedPanelRects.get(PanelPosition.Bottom).map(_.height).getOrElse(0)
    val leftPinnedWidth =
      pinnedPanelRects.get(PanelPosition.Left).map(_.width).getOrElse(0)
    val rightPinnedWidth =
      pinnedPanelRects.get(PanelPosition.Right).map(_.width).getOrElse(0)

    val workspaceX = leftPinnedWidth
    val workspaceY = topPinnedHeight
    val workspaceWidth =
      math.max(1, viewportSize.width - leftPinnedWidth - rightPinnedWidth)
    val workspaceHeight =
      math.max(1, contentHeight - topPinnedHeight - bottomPinnedHeight)

    val spacerWidth = (workspaceWidth * spacerPercentage).toInt

    // Calculate space needed for UI elements
    val lineNumberWidth =
      if state.config.showLineNumbers then calculateLineNumberWidth(state)
      else 0

    // Adjust editor area to accommodate UI elements
    val availableWidth  = math.max(1, workspaceWidth - (2 * spacerWidth) - lineNumberWidth)
    val availableHeight = workspaceHeight

    val leftSpacerRect = LayoutRect(workspaceX, workspaceY, spacerWidth, workspaceHeight)
    val lineNumberRect =
      if state.config.showLineNumbers then Some(LayoutRect(workspaceX + spacerWidth, workspaceY + 1, lineNumberWidth, availableHeight))
      else None

    val editorPanelRect = LayoutRect(
      x = workspaceX + spacerWidth + lineNumberWidth,
      y = workspaceY,
      width = availableWidth,
      height = availableHeight
    )
    val rightSpacerRect =
      LayoutRect(workspaceX + spacerWidth + lineNumberWidth + availableWidth, workspaceY, spacerWidth, workspaceHeight)

    val gutterRect =
      if state.config.showGutter then Some(LayoutRect(0, viewportSize.height - 1, viewportSize.width, 1))
      else None

    val baseLayout = CalculatedLayout(
      editorPanelRect = editorPanelRect,
      leftSpacerRect = leftSpacerRect,
      rightSpacerRect = rightSpacerRect,
      pinnedPanelRects = pinnedPanelRects,
      lineNumberRect = lineNumberRect,
      gutterRect = gutterRect
    )

    val paneLayouts = calculatePaneLayouts(state, baseLayout)

    val aboveCursorOverlayRect = state.floatingSurfaces
      .find {
        _.presentation match
          case SurfacePresentation.Floating(_, SurfacePlacement.AboveCursor) => true
          case _                                                             => false
      }
      .flatMap(surface => calculateFloatingSurfaceRect(surface, state, paneLayouts))
    val belowCursorOverlayRect = state.floatingSurfaces
      .find {
        _.presentation match
          case SurfacePresentation.Floating(_, SurfacePlacement.BelowCursor) => true
          case _                                                             => false
      }
      .flatMap(surface => calculateFloatingSurfaceRect(surface, state, paneLayouts))

    baseLayout.copy(
      aboveCursorOverlayRect = aboveCursorOverlayRect,
      belowCursorOverlayRect = belowCursorOverlayRect
    )

  private def calculatePinnedPanelRects(
    panels: List[UiSurface],
    terminalWidth: Int,
    contentHeight: Int
  ): Map[PanelPosition, LayoutRect] =
    val panelsByPosition = panels.flatMap {
      _.presentation match
        case SurfacePresentation.Pinned(position, size) => Some(position -> size)
        case _                                          => None
    }.toMap
    val topHeight = panelsByPosition.get(PanelPosition.Top).map(size => math.min(size, contentHeight)).getOrElse(0)
    val remainingAfterTop = math.max(1, contentHeight - topHeight)
    val bottomHeight = panelsByPosition.get(PanelPosition.Bottom).map(size => math.min(size, remainingAfterTop)).getOrElse(0)
    val verticalZoneY = topHeight
    val verticalZoneHeight = math.max(1, contentHeight - topHeight - bottomHeight)

    val leftWidth = panelsByPosition.get(PanelPosition.Left).map(size => math.min(size, terminalWidth)).getOrElse(0)
    val remainingAfterLeft = math.max(1, terminalWidth - leftWidth)
    val rightWidth = panelsByPosition.get(PanelPosition.Right).map(size => math.min(size, remainingAfterLeft)).getOrElse(0)

    val rects = List.newBuilder[(PanelPosition, LayoutRect)]

    if topHeight > 0 then
      rects += PanelPosition.Top -> LayoutRect(0, 0, terminalWidth, topHeight)
    if bottomHeight > 0 then
      rects += PanelPosition.Bottom -> LayoutRect(0, contentHeight - bottomHeight, terminalWidth, bottomHeight)
    if leftWidth > 0 then
      rects += PanelPosition.Left -> LayoutRect(0, verticalZoneY, leftWidth, verticalZoneHeight)
    if rightWidth > 0 then
      rects += PanelPosition.Right -> LayoutRect(terminalWidth - rightWidth, verticalZoneY, rightWidth, verticalZoneHeight)

    rects.result().toMap

  private def calculateLineNumberWidth(state: AppState): Int =
    // Find the maximum line count across all buffers to determine width needed
    val maxLines =
      if state.buffers.isEmpty then 10
      else state.buffers.values.map(_.content.lineCount).max

    math.max(3, maxLines.toString.length + 1) // +1 for spacing, minimum 3 chars

  private def calculateFloatingSurfaceRect(
    surface: UiSurface,
    state: AppState,
    paneLayouts: Map[PaneId, LayoutRect]
  ): Option[LayoutRect] =
    for
      paneId  <- state.layout.activeEditorPaneId
      pane     <- state.layout.editorPanes.get(paneId)
      paneRect <- paneLayouts.get(paneId)
      bufferId <- pane.bufferId
      buffer   <- state.buffers.get(bufferId)
      anchor   <- surfaceAnchor(surface).orElse(state.activeCursorPosition)
      screenPosition <- CursorLayout.calculateScreenPosition(
        anchor,
        buffer.content,
        paneRect,
        buffer.viewport
      )
    yield
      val contentRect     = CursorLayout.contentRectForPane(paneRect)
      val preferredWidth  = calculateFloatingSurfaceWidth(surface.content, contentRect.width)
      val preferredHeight = calculateFloatingSurfaceHeight(surface.content, contentRect.height)
      val overlayX = math.max(
        contentRect.x,
        math.min(screenPosition.x - (preferredWidth / 2), contentRect.right - preferredWidth)
      )
      val overlayY = surface.presentation match
        case SurfacePresentation.Floating(_, SurfacePlacement.AboveCursor) =>
          math.max(contentRect.y, screenPosition.y - preferredHeight)
        case SurfacePresentation.Floating(_, SurfacePlacement.BelowCursor) =>
          val preferredBelowY = screenPosition.y + 1
          if preferredBelowY + preferredHeight <= contentRect.bottom then preferredBelowY
          else math.max(contentRect.y, screenPosition.y - preferredHeight)
        case _ =>
          contentRect.y

      LayoutRect(
        x = overlayX,
        y = overlayY,
        width = preferredWidth,
        height = preferredHeight
      )

  private def calculateFloatingSurfaceWidth(content: SurfaceContent, maxWidth: Int): Int =
    maxWidth

  private def calculateFloatingSurfaceHeight(content: SurfaceContent, maxHeight: Int): Int =
    val preferredHeight = content match
      case SurfaceContent.StartPage(_)                 => maxHeight
      case SurfaceContent.QuickInfo(text)              => math.max(3, text.linesIterator.size + 2)
      case SurfaceContent.FilePreview(_, content)      => math.max(4, math.min(6, content.linesIterator.take(4).size + 2))
      case SurfaceContent.SymbolDefinition(_, _)       => 4
      case SurfaceContent.DirectoryListing(_, entries, _) => math.max(4, math.min(6, entries.take(4).size + 2))
      case SurfaceContent.CommandPalette(_) | SurfaceContent.ThemePicker(_) | SurfaceContent.FileSearch(_) =>
        math.min(8, math.max(4, maxHeight - 1))
      case SurfaceContent.ModalWorkflow(modal) =>
        modal match
          case Modal.FileWorkflow(workflow) =>
            math.max(8, math.min(12, workflow.suggestions.take(4).size + 6))
          case Modal.ReplaceWorkflow(_) => 5
          case Modal.CloseWorkflow(_)   => 4
          case Modal.Custom(_, _) => 4
          case _                  => 3
      case SurfaceContent.Terminal(_, _) | SurfaceContent.Outline(_) | SurfaceContent.Diagnostics(_) =>
        math.min(8, math.max(4, maxHeight - 1))
      case SurfaceContent.GhostOverlay(_, cachedRect) =>
        cachedRect.height

    math.max(3, math.min(maxHeight, preferredHeight))

  private def surfaceAnchor(surface: UiSurface): Option[CursorPosition] =
    surface.presentation match
      case SurfacePresentation.Floating(anchor, _) => anchor
      case _                                       => None

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

    viewport.copy(topLine = clampedScrollLine)

  def updateViewportDimensions(viewport: Viewport, panelRect: LayoutRect): Viewport =
    viewport.copy(
      visibleLines = panelRect.height,
      visibleColumns = panelRect.width
    )

  def updateViewportDimensions(viewport: Viewport, panelRect: LayoutRect, metrics: CellMetrics): Viewport =
    viewport.copy(
      visibleLines = panelRect.height / metrics.lineHeight,
      visibleColumns = panelRect.width / metrics.charWidth
    )

  /** Calculate individual pane layouts within the editor area */
  def calculatePaneLayouts(state: AppState, calculatedLayout: CalculatedLayout): Map[PaneId, LayoutRect] =
    calculatePaneLayoutsWithMinWidth(state, calculatedLayout, state.config.minimumPaneWidth)

  /** Calculate individual pane layouts with minimum width constraint */
  def calculatePaneLayoutsWithMinWidth(
    state: AppState,
    calculatedLayout: CalculatedLayout,
    minWidth: Int
  ): Map[PaneId, LayoutRect] =
    val editorRect = calculatedLayout.editorPanelRect
    val paneIds    = state.layout.orderedPaneIds
    val paneCount  = paneIds.size

    if paneCount == 0 then Map.empty
    else if paneCount == 1 then
      // Single pane uses full editor area
      val paneId = paneIds.head
      Map(paneId -> editorRect)
    else
      // Multiple panes: split horizontally with minimum width constraints
      val maxVisiblePanes  = math.max(1, editorRect.width / minWidth)
      val visiblePaneCount = math.min(paneCount, maxVisiblePanes)
      val paneWidth        = math.max(minWidth, editorRect.width / visiblePaneCount)

      // Find focused pane to ensure it's visible
      val focusedPaneId = state.focus match
        case Focus.EditorPane(paneId) if state.layout.editorPanes.contains(paneId) => Some(paneId)
        case _                                                                     => None

      // Calculate which panes should be visible
      val (visibleStartIndex, visiblePaneIds) = calculateVisiblePaneWindow(
        paneIds,
        focusedPaneId,
        visiblePaneCount
      )

      // Create layouts for all panes
      val allPaneLayouts = for ((paneId, globalIndex) <- paneIds.zipWithIndex) yield
        val visibleIndex = globalIndex - visibleStartIndex
        val isVisible    = visibleIndex >= 0 && visibleIndex < visiblePaneCount

        val paneRect =
          if isVisible then
            // Visible pane: positioned within editor area
            LayoutRect(
              x = editorRect.x + (visibleIndex * paneWidth),
              y = editorRect.y,
              width = paneWidth,
              height = editorRect.height
            )
          else
            // Hidden pane: positioned off-screen
            val offScreenX =
              if visibleIndex < 0 then editorRect.x - 100 // Off-screen to the left
              else editorRect.x + editorRect.width + 100  // Off-screen to the right

            LayoutRect(
              x = offScreenX,
              y = editorRect.y,
              width = paneWidth,
              height = editorRect.height
            )

        paneId -> paneRect

      allPaneLayouts.toMap

  /** Calculate which panes should be visible based on focus and capacity */
  private def calculateVisiblePaneWindow(
    allPaneIds: List[PaneId],
    focusedPaneId: Option[PaneId],
    maxVisible: Int
  ): (Int, List[PaneId]) =
    if allPaneIds.size <= maxVisible then
      // All panes fit
      (0, allPaneIds)
    else
      // Find focused pane index
      val focusedIndex = focusedPaneId.flatMap(id => allPaneIds.zipWithIndex.find(_._1 == id).map(_._2))

      val startIndex = focusedIndex match
        case Some(index) =>
          // Center the window around the focused pane, but stay within bounds
          val idealStart = index - maxVisible / 2
          val maxStart   = allPaneIds.size - maxVisible
          math.max(0, math.min(idealStart, maxStart))
        case None =>
          // No focused pane, show first N panes
          0

      val endIndex     = startIndex + maxVisible
      val visiblePanes = allPaneIds.slice(startIndex, endIndex)
      (startIndex, visiblePanes)
