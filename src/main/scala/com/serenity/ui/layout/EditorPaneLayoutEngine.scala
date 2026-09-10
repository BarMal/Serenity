package com.serenity.ui.layout

import com.serenity.config.TextAreaInsets
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
    paneRects.get(paneId).flatMap { current =>
      val order = state.persisted.layout.orderedPaneIds.zipWithIndex.toMap
      paneRects.iterator
        .filter { case (candidateId, rect) => candidateId != paneId && rect.width > 0 && rect.height > 0 }
        .flatMap { (candidateId, candidate) =>
          val rank =
            direction match
              case Direction.Left if candidate.right <= current.x =>
                Some((current.x - candidate.right, math.abs(candidate.centerY - current.centerY)))
              case Direction.Right if candidate.x >= current.right =>
                Some((candidate.x - current.right, math.abs(candidate.centerY - current.centerY)))
              case Direction.Up if candidate.bottom <= current.y =>
                Some((current.y - candidate.bottom, math.abs(candidate.centerX - current.centerX)))
              case Direction.Down if candidate.y >= current.bottom =>
                Some((candidate.y - current.bottom, math.abs(candidate.centerX - current.centerX)))
              case _ =>
                None
          rank.map { (primaryDistance, perpendicularDistance) =>
            (candidateId, primaryDistance, perpendicularDistance, order.getOrElse(candidateId, Int.MaxValue))
          }
        }
        .toList
        .sortBy { case (_, primary, perpendicular, orderIndex) => (primary, perpendicular, orderIndex) }
        .headOption
        .map(_._1)
    }

  def calculateEditorWorkspaceLayout(state: AppState, calculatedLayout: CalculatedLayout): EditorWorkspaceLayout =
    EditorWorkspaceLayout(
      editorPanelRect = calculatedLayout.editorPanelRect,
      lineNumberRect = calculatedLayout.lineNumberRect,
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
        calculateWorkspaceTreePaneRects(tree, calculatedLayout.editorPanelRect, minWidth)
          .filter { case (paneId, _) => editorPaneIds.contains(paneId) }
      case None =>
        Map.empty

  private def calculateWorkspaceTreePaneRects(
    tree: WorkspaceTree,
    editorRect: LayoutRect,
    minWidth: Int
  ): Map[PaneId, LayoutRect] =
    def minimumWidth(node: WorkspaceNode): Int =
      node match
        case WorkspaceNode.Leaf(_, _)             => minWidth.max(1)
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
    val headerHeight   = LayoutEngine.paneHeaderHeight(state)
    val insets         = state.persisted.config.surfaceConfig.textAreaInsets.normalized
    val paneHeaderRect = paneRect.copy(height = headerHeight)
    val headerRect =
      if state.persisted.layout.activeEditorPaneId.contains(paneId) then
        activeWorkspaceHeaderRect(paneRect.y, calculatedLayout, headerHeight)
      else paneHeaderRect
    EditorPaneLayout(
      paneRect = paneRect,
      headerRect = headerRect,
      titleRect = headerRect,
      contentRect = contentRectForPane(paneRect, insets, headerHeight),
      topSpacerRect = topSpacerRectForPane(paneRect, insets, headerHeight),
      bottomSpacerRect = bottomSpacerRectForPane(paneRect, insets, headerHeight)
    )

  private[layout] def contentRectForPane(paneRect: LayoutRect): LayoutRect =
    contentRectForPane(paneRect, TextAreaInsets(), LayoutEngine.EditorPaneHeaderHeight)

  private def contentRectForPane(paneRect: LayoutRect, insets: TextAreaInsets, headerHeight: Int): LayoutRect =
    val baseContent        = baseContentRectForPane(paneRect, headerHeight)
    val topSpacerHeight    = (baseContent.height * insets.top).toInt
    val bottomSpacerHeight = (baseContent.height * insets.bottom).toInt
    LayoutRect(
      baseContent.x,
      baseContent.y + topSpacerHeight,
      baseContent.width,
      math.max(1, baseContent.height - topSpacerHeight - bottomSpacerHeight)
    )

  private def topSpacerRectForPane(paneRect: LayoutRect, insets: TextAreaInsets, headerHeight: Int): LayoutRect =
    val baseContent = baseContentRectForPane(paneRect, headerHeight)
    baseContent.copy(height = (baseContent.height * insets.top).toInt)

  private def bottomSpacerRectForPane(paneRect: LayoutRect, insets: TextAreaInsets, headerHeight: Int): LayoutRect =
    val baseContent        = baseContentRectForPane(paneRect, headerHeight)
    val topSpacerHeight    = (baseContent.height * insets.top).toInt
    val bottomSpacerHeight = (baseContent.height * insets.bottom).toInt
    LayoutRect(
      baseContent.x,
      baseContent.y + topSpacerHeight + math.max(1, baseContent.height - topSpacerHeight - bottomSpacerHeight),
      baseContent.width,
      bottomSpacerHeight
    )

  private def baseContentRectForPane(paneRect: LayoutRect, headerHeight: Int): LayoutRect =
    LayoutRect(
      paneRect.x,
      paneRect.y + headerHeight,
      paneRect.width,
      math.max(1, paneRect.height - headerHeight)
    )

  private def activeWorkspaceHeaderRect(y: Int, layout: CalculatedLayout, headerHeight: Int): LayoutRect =
    val workspaceRects =
      List(
        Some(layout.leftSpacerRect),
        layout.lineNumberRect,
        Some(layout.editorPanelRect),
        Some(layout.rightSpacerRect)
      ).flatten
    val left  = workspaceRects.map(_.x).minOption.getOrElse(layout.editorPanelRect.x)
    val right = workspaceRects.map(_.right).maxOption.getOrElse(layout.editorPanelRect.right)

    LayoutRect(left, y, math.max(1, right - left), headerHeight)
