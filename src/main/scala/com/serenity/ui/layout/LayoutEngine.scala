package com.serenity.ui.layout

import com.serenity.state.models.{AppState, CursorPosition, Viewport}

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
