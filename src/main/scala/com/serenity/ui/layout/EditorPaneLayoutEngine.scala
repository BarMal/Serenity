package com.serenity.ui.layout

import com.serenity.keystroke.events.Direction
import com.serenity.state.models.*

/** Splitting the editor workspace area into individual pane rectangles via the `WorkspaceTree`, plus the
  * header/content/spacer sub-rects within one pane. Split out of `LayoutEngine` (600-line architecture ratchet); the
  * surrounding docked/pinned-panel and floating-surface layout stays there.
  */
object EditorPaneLayoutEngine:

  /** Calculate individual pane layouts within the editor area */
  def calculatePaneLayouts(state: AppState, calculatedLayout: CalculatedLayout): Map[PaneId, LayoutRect] =
    calculateEditorPaneLayouts(state, calculatedLayout).view.mapValues(_.paneRect).toMap

  /** Calculate individual pane layouts with minimum width constraint */
  def calculatePaneLayoutsWithMinWidth(
    state: AppState,
    calculatedLayout: CalculatedLayout,
    minWidth: Int
  ): Map[PaneId, LayoutRect] =
    calculateEditorPaneLayoutsWithMinWidth(state, calculatedLayout, minWidth).view.mapValues(_.paneRect).toMap

  def calculateEditorPaneLayouts(state: AppState, calculatedLayout: CalculatedLayout): Map[PaneId, EditorPaneLayout] =
    calculateEditorPaneLayoutsWithMinWidth(
      state,
      calculatedLayout,
      state.persisted.config.editorConfig.minimumPaneWidth
    )

  /** Finds the nearest usable pane in a cardinal direction using authoritative pane rectangles. */
  def directionalPaneNeighbor(
    state: AppState,
    calculatedLayout: CalculatedLayout,
    paneId: PaneId,
    direction: Direction
  ): Option[PaneId] =
    val paneRects = calculatePaneLayouts(state, calculatedLayout)
    val candidates = state.persisted.layout.orderedPaneIds
      .filter(_ != paneId)
      .flatMap(id => paneRects.get(id).filter(rect => rect.width > 0 && rect.height > 0).map(FocusTarget.Pane(id) -> _))
    paneRects.get(paneId).flatMap(DirectionalFocusLayout.neighbour(_, candidates, direction)).collect {
      case FocusTarget.Pane(id) => id
    }

  def calculateEditorWorkspaceLayout(state: AppState, calculatedLayout: CalculatedLayout): EditorWorkspaceLayout =
    EditorWorkspaceLayout(
      editorPanelRect = calculatedLayout.editorPanelRect,
      gutterRect = calculatedLayout.gutterRect,
      paneLayouts = calculateEditorPaneLayouts(state, calculatedLayout)
    )

  def calculateEditorPaneLayoutsWithMinWidth(
    state: AppState,
    calculatedLayout: CalculatedLayout,
    minWidth: Int
  ): Map[PaneId, EditorPaneLayout] =
    calculatePaneRectsWithMinWidth(state, calculatedLayout, minWidth).view
      .map((paneId, paneRect) => paneId -> editorPaneLayoutFor(paneId, paneRect, state, calculatedLayout))
      .toMap

  private def calculatePaneRectsWithMinWidth(
    state: AppState,
    calculatedLayout: CalculatedLayout,
    minWidth: Int
  ): Map[PaneId, LayoutRect] =
    val editorPaneIds = state.persisted.layout.editorPanes.keySet
    state.persisted.layout.workspaceTree match
      case Some(tree) =>
        calculateWorkspaceTreePaneRects(
          tree,
          calculatedLayout.editorPanelRect,
          paneId => PaneTextArea.minimumWidth(state, paneId, calculatedLayout.textAreaInsets, minWidth)
        )
          .filter { case (paneId, _) => editorPaneIds.contains(paneId) }
      case None =>
        Map.empty

  private def calculateWorkspaceTreePaneRects(
    tree: WorkspaceTree,
    editorRect: LayoutRect,
    minimumPaneWidth: PaneId => Int
  ): Map[PaneId, LayoutRect] =
    def minimumWidth(node: WorkspaceNode): Int =
      node match
        case WorkspaceNode.Leaf(_, paneId)        => minimumPaneWidth(paneId).max(1)
        case WorkspaceNode.DockedSurface(_, _, _) => 1
        case WorkspaceNode.Split(_, SplitAxis.Horizontal, _, first, second) =>
          minimumWidth(first) + minimumWidth(second)
        case WorkspaceNode.Split(_, SplitAxis.Vertical, _, first, second) =>
          minimumWidth(first).max(minimumWidth(second))

    def minimumHeight(node: WorkspaceNode): Int =
      node match
        case WorkspaceNode.Leaf(_, _)             => LayoutEngine.MinimumVerticalPaneHeight
        case WorkspaceNode.DockedSurface(_, _, _) => 1
        case WorkspaceNode.Split(_, SplitAxis.Horizontal, _, first, second) =>
          minimumHeight(first).max(minimumHeight(second))
        case WorkspaceNode.Split(_, SplitAxis.Vertical, _, first, second) =>
          minimumHeight(first) + minimumHeight(second)

    def splitExtent(total: Int, ratio: Double, minimumFirst: Int, minimumSecond: Int): Int =
      if total <= 1 then total
      else
        val canRespectMinimums = minimumFirst + minimumSecond <= total
        val lower              = if canRespectMinimums then minimumFirst else 1
        val upper              = if canRespectMinimums then total - minimumSecond else total - 1
        math.max(lower, math.min(upper, (total * ratio.max(0.0).min(1.0)).toInt))

    def recurse(node: WorkspaceNode, rect: LayoutRect): Map[PaneId, LayoutRect] =
      node match
        case WorkspaceNode.Leaf(_, paneId)        => Map(paneId -> rect)
        case WorkspaceNode.DockedSurface(_, _, _) => Map.empty
        case WorkspaceNode.Split(_, SplitAxis.Horizontal, ratio, first, second) =>
          val firstWidth = splitExtent(rect.width, ratio, minimumWidth(first), minimumWidth(second))
          recurse(first, rect.copy(width = firstWidth)) ++
            recurse(second, LayoutRect(rect.x + firstWidth, rect.y, rect.width - firstWidth, rect.height))
        case WorkspaceNode.Split(_, SplitAxis.Vertical, ratio, first, second) =>
          val firstHeight = splitExtent(rect.height, ratio, minimumHeight(first), minimumHeight(second))
          recurse(first, rect.copy(height = firstHeight)) ++
            recurse(second, LayoutRect(rect.x, rect.y + firstHeight, rect.width, rect.height - firstHeight))

    tree.editorRoot.map(recurse(_, editorRect)).getOrElse(Map.empty)

  private def editorPaneLayoutFor(
    paneId: PaneId,
    paneRect: LayoutRect,
    state: AppState,
    calculatedLayout: CalculatedLayout
  ): EditorPaneLayout =
    val headerHeight                          = LayoutEngine.paneHeaderHeight(state)
    val insets                                = calculatedLayout.textAreaInsets
    val bands                                 = PaneTextArea.bands(state, paneId, paneRect.x, paneRect.width, insets)
    val paneHeaderRect                        = paneRect.copy(height = headerHeight)
    val baseContent                           = baseContentRectForPane(paneRect, headerHeight)
    val (topSpacerHeight, bottomSpacerHeight) = TextArea.spacerHeights(baseContent.height, insets)
    val textHeight                            = math.max(1, baseContent.height - topSpacerHeight - bottomSpacerHeight)
    val textY                                 = baseContent.y + topSpacerHeight
    val bandRect                              = LayoutRect(bands.text.x, textY, bands.text.width, textHeight)
    val scrollbarColumns                      = ScrollbarLayout.terminalColumns(state, bandRect.width)
    val bandX                                 = bands.leftSpacer.right
    val bandWidth                             = bands.rightSpacer.x - bandX
    def rail(span: PaneTextArea.Span)         = LayoutRect(span.x, textY, span.width, textHeight)
    EditorPaneLayout(
      paneRect = paneRect,
      headerRect = paneHeaderRect,
      titleRect = paneHeaderRect,
      contentRect = bandRect.copy(width = bandRect.width - scrollbarColumns),
      topSpacerRect = LayoutRect(bandX, baseContent.y, bandWidth, topSpacerHeight),
      bottomSpacerRect = LayoutRect(bandX, textY + textHeight, bandWidth, bottomSpacerHeight),
      leftSpacerRect = LayoutRect(bands.leftSpacer.x, paneRect.y, bands.leftSpacer.width, paneRect.height),
      rightSpacerRect = LayoutRect(bands.rightSpacer.x, paneRect.y, bands.rightSpacer.width, paneRect.height),
      lineNumberRect = bands.leftRail.map(rail),
      rightLineNumberRect = bands.rightRail.map(rail),
      scrollbarRect = LayoutRect(bandRect.right - scrollbarColumns, bandRect.y, scrollbarColumns, bandRect.height)
    )

  private[layout] def contentRectForPane(paneRect: LayoutRect): LayoutRect =
    baseContentRectForPane(paneRect, LayoutEngine.EditorPaneHeaderHeight)

  private def baseContentRectForPane(paneRect: LayoutRect, headerHeight: Int): LayoutRect =
    LayoutRect(
      paneRect.x,
      paneRect.y + headerHeight,
      paneRect.width,
      math.max(1, paneRect.height - headerHeight)
    )
