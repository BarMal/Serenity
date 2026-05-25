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
    floatingPanelRect: Option[LayoutRect] = None,
    aboveCursorOverlayRect: Option[LayoutRect] = None,
    belowCursorOverlayRect: Option[LayoutRect] = None,
    lineNumberRect: Option[LayoutRect] = None,
    gutterRect: Option[LayoutRect] = None
)

object LayoutManager:

  def calculateLayout(
    state: AppState,
    terminalSize: TerminalSize
  ): Unit = ()

object LayoutEngine:

  // Default spacer width as percentage of terminal width (15% each side = 30% total)
  private val DefaultSpacerPercentage = 0.15

  def calculateLayout(
    state: AppState,
    terminalSize: TerminalSize,
    spacerPercentage: Double = DefaultSpacerPercentage
  ): CalculatedLayout =
    calculateLayoutWithUI(state, terminalSize, spacerPercentage)

  def calculateLayoutWithUI(
    state: AppState,
    terminalSize: TerminalSize,
    spacerPercentage: Double = DefaultSpacerPercentage
  ): CalculatedLayout =

    val spacerWidth = (terminalSize.width * spacerPercentage).toInt

    // Calculate space needed for UI elements
    val lineNumberWidth =
      if state.config.showLineNumbers then calculateLineNumberWidth(state)
      else 0

    val gutterHeight = if state.config.showGutter then 1 else 0

    // Adjust editor area to accommodate UI elements
    val availableWidth  = terminalSize.width - (2 * spacerWidth) - lineNumberWidth
    val availableHeight = terminalSize.height - gutterHeight

    val leftSpacerRect = LayoutRect(0, 0, spacerWidth, terminalSize.height)
    val lineNumberRect =
      if state.config.showLineNumbers then Some(LayoutRect(spacerWidth, 1, lineNumberWidth, availableHeight))
      else None

    val editorPanelRect = LayoutRect(
      x = spacerWidth + lineNumberWidth,
      y = 0,
      width = availableWidth,
      height = availableHeight
    )
    val rightSpacerRect =
      LayoutRect(spacerWidth + lineNumberWidth + availableWidth, 0, spacerWidth, terminalSize.height)

    val gutterRect =
      if state.config.showGutter then Some(LayoutRect(0, terminalSize.height - 1, terminalSize.width, 1))
      else None

    val baseLayout = CalculatedLayout(
      editorPanelRect = editorPanelRect,
      leftSpacerRect = leftSpacerRect,
      rightSpacerRect = rightSpacerRect,
      lineNumberRect = lineNumberRect,
      gutterRect = gutterRect
    )

    val paneLayouts = calculatePaneLayouts(state, baseLayout)

    val aboveCursorOverlayRect = calculatePeekOverlayRect(state, paneLayouts, OverlayPlacement.Above)
    val belowCursorOverlayRect = calculateCommandRunnerRect(state, paneLayouts)

    baseLayout.copy(
      aboveCursorOverlayRect = aboveCursorOverlayRect,
      belowCursorOverlayRect = belowCursorOverlayRect
    )

  private def calculateLineNumberWidth(state: AppState): Int =
    // Find the maximum line count across all buffers to determine width needed
    val maxLines =
      if state.buffers.isEmpty then 10
      else state.buffers.values.map(_.content.lineCount).max

    math.max(3, maxLines.toString.length + 1) // +1 for spacing, minimum 3 chars

  private enum OverlayPlacement:
    case Above, Below

  private def calculatePeekOverlayRect(
    state: AppState,
    paneLayouts: Map[PaneId, LayoutRect],
    placement: OverlayPlacement
  ): Option[LayoutRect] =
    for
      overlay <- state.peekOverlay
      paneId  <- state.layout.activeEditorPaneId
      pane    <- state.layout.editorPanes.get(paneId)
      paneRect <- paneLayouts.get(paneId)
      bufferId <- pane.bufferId
      buffer  <- state.buffers.get(bufferId)
      screenPosition <- CursorLayout.calculateScreenPosition(
        overlay.position,
        buffer.content,
        paneRect,
        buffer.viewport
      )
    yield
      val contentRect     = CursorLayout.contentRectForPane(paneRect)
      val preferredWidth  = calculatePeekOverlayWidth(overlay.content, contentRect.width)
      val preferredHeight = calculatePeekOverlayHeight(overlay.content, contentRect.height)
      val overlayX = math.max(
        contentRect.x,
        math.min(screenPosition.x - (preferredWidth / 2), contentRect.right - preferredWidth)
      )
      val overlayY = placement match
        case OverlayPlacement.Above =>
          math.max(contentRect.y, screenPosition.y - preferredHeight)
        case OverlayPlacement.Below =>
          math.min(math.max(contentRect.y, screenPosition.y + 1), contentRect.bottom - preferredHeight)

      LayoutRect(
        x = overlayX,
        y = overlayY,
        width = preferredWidth,
        height = preferredHeight
      )

  private def calculateCommandRunnerRect(
    state: AppState,
    paneLayouts: Map[PaneId, LayoutRect]
  ): Option[LayoutRect] =
    if !state.commandRunner.isActive then None
    else
      for
        paneId   <- state.layout.activeEditorPaneId
        pane     <- state.layout.editorPanes.get(paneId)
        paneRect <- paneLayouts.get(paneId)
        bufferId <- pane.bufferId
        buffer   <- state.buffers.get(bufferId)
        cursor   <- buffer.cursors.headOption
        screenPosition <- CursorLayout.calculateScreenPosition(cursor, buffer.content, paneRect, buffer.viewport)
      yield
        val contentRect     = CursorLayout.contentRectForPane(paneRect)
        val preferredWidth  = math.min(60, math.max(24, contentRect.width - 2))
        val preferredHeight = math.min(8, math.max(4, contentRect.height - 1))
        val overlayX = math.max(
          contentRect.x,
          math.min(screenPosition.x - (preferredWidth / 2), contentRect.right - preferredWidth)
        )
        val preferredBelowY = screenPosition.y + 1
        val overlayY =
          if preferredBelowY + preferredHeight <= contentRect.bottom then preferredBelowY
          else math.max(contentRect.y, screenPosition.y - preferredHeight)

        LayoutRect(
          x = overlayX,
          y = overlayY,
          width = preferredWidth,
          height = preferredHeight
        )

  private def calculatePeekOverlayWidth(content: PeekContent, maxWidth: Int): Int =
    val preferredWidth = content match
      case PeekContent.QuickInfo(text) =>
        text.linesIterator.map(_.length).maxOption.getOrElse(0) + 4
      case PeekContent.FilePreview(path, content) =>
        math.max(path.getFileName.toString.length + 4, content.linesIterator.take(4).map(_.length).maxOption.getOrElse(0) + 4)
      case PeekContent.SymbolDefinition(symbol, _) =>
        symbol.length + 12
      case PeekContent.DirectoryListing(path, entries) =>
        math.max(
          path.getFileName.toString.length + 12,
          entries.take(4).map(_.name.length).maxOption.getOrElse(0) + 4
        )

    math.max(16, math.min(maxWidth, preferredWidth))

  private def calculatePeekOverlayHeight(content: PeekContent, maxHeight: Int): Int =
    val preferredHeight = content match
      case PeekContent.QuickInfo(text)           => math.max(3, text.linesIterator.size + 2)
      case PeekContent.FilePreview(_, content)   => math.max(4, math.min(6, content.linesIterator.take(4).size + 2))
      case PeekContent.SymbolDefinition(_, _)    => 4
      case PeekContent.DirectoryListing(_, entries) => math.max(4, math.min(6, entries.take(4).size + 2))

    math.max(3, math.min(maxHeight, preferredHeight))

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
