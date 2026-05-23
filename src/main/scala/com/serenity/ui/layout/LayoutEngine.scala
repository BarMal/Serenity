package com.serenity.ui.layout

import com.serenity.state.models.*

case class TerminalSize(width: Int, height: Int)

case class LayoutRect(x: Int, y: Int, width: Int, height: Int):
  def right: Int   = x + width
  def bottom: Int  = y + height
  def centerX: Int = x + width / 2
  def centerY: Int = y + height / 2

case class CalculatedLayout(
    editorPanelRect: LayoutRect,
    leftSpacerRect: LayoutRect,
    rightSpacerRect: LayoutRect,
    floatingPanelRect: Option[LayoutRect] = None
)

object LayoutEngine:

  // Default spacer width as percentage of terminal width (15% each side = 30% total)
  private val DefaultSpacerPercentage = 0.15

  def calculateLayout(
    state: AppState,
    terminalSize: TerminalSize,
    spacerPercentage: Double = DefaultSpacerPercentage
  ): CalculatedLayout =

    val spacerWidth  = (terminalSize.width * spacerPercentage).toInt
    val editorWidth  = terminalSize.width - (2 * spacerWidth)
    val editorHeight = terminalSize.height

    val leftSpacerRect  = LayoutRect(0, 0, spacerWidth, editorHeight)
    val editorPanelRect = LayoutRect(spacerWidth, 0, editorWidth, editorHeight)
    val rightSpacerRect = LayoutRect(spacerWidth + editorWidth, 0, spacerWidth, editorHeight)

    // Calculate floating panel position if needed
    val floatingPanelRect = calculateFloatingPanel(state, editorPanelRect)

    CalculatedLayout(
      editorPanelRect = editorPanelRect,
      leftSpacerRect = leftSpacerRect,
      rightSpacerRect = rightSpacerRect,
      floatingPanelRect = floatingPanelRect
    )

  private def calculateFloatingPanel(
    state: AppState,
    editorRect: LayoutRect
  ): Option[LayoutRect] =
    // For now, return None - we'll implement this when we have command runner/modal support
    // When implemented, this should:
    // 1. Get current cursor position from focused editor pane
    // 2. Calculate panel position beneath cursor
    // 3. Ensure panel fits within editor bounds
    None

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
    val paneIds    = state.layout.editorPanes.keys.toList.sortBy(_.value)
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
