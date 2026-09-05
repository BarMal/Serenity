package com.serenity.ui.layout

import com.serenity.config.{AppConfig, InterfaceDensityMetrics, TextAreaInsets}
import com.serenity.keystroke.events.Direction
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

  private[layout] val DefaultSpacerPercentage = 0.0
  private val MinimumVerticalPaneHeight       = 5
  private val EditorPaneHeaderHeight          = 1
  private val PinnedPanelDragWorkspaceReach   = 1

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
          calculateDockedPanelLayout(
            _,
            state.pinnedSurfaces,
            LayoutRect(0, 0, viewportSize.width, contentHeight),
            minimumEditorWorkspaceWidth,
            MinimumVerticalPaneHeight,
            uiElementGap
          )
        )
        .getOrElse(
          calculatePinnedPanelLayout(
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
    val belowSurfaces = orderedBelowCursorSurfaces(state)
    val aboveCursorOverlayStack =
      aboveSurfaces.flatMap(surface => calculateFloatingSurfaceRect(surface, state, paneLayouts).map(surface.id -> _))
    val belowLayout = calculateBelowCursorOverlayStack(belowSurfaces, state, paneLayouts)
    val floatingOffsets = floatingOverlayOffsets(
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
    val height = calculateFloatingSurfaceHeight(surface.content, width, bounds.height, state)
    LayoutRect(
      x = bounds.x + math.max(0, (bounds.width - width) / 2),
      y = bounds.y + math.max(0, (bounds.height - height) / 2),
      width = width,
      height = height
    )

  private def usesBottomGutter(state: AppState): Boolean =
    state.persisted.config.surfaceConfig.showGutter ||
      (state.persisted.config.cursorInfoBarSegments.nonEmpty &&
        state.persisted.config.cursorInfoBarPlacement == com.serenity.config.CursorInfoBarPlacement.PinnedBottom)

  private def paneHeaderHeight(state: AppState): Int =
    if state.persisted.config.surfaceConfig.showPaneHeaders then EditorPaneHeaderHeight else 0

  final private case class PinnedPanelLayout(
      panelRects: Map[PanelPosition, LayoutRect],
      surfaceRects: Map[SurfaceId, LayoutRect]
  )

  final private case class PinnedAxisSizes(start: Int, end: Int)

  final case class PinnedPanelDragResize(position: PanelPosition, size: Int)

  private def calculateDockedPanelLayout(
    tree: WorkspaceTree,
    panels: List[UiSurface],
    workspaceRect: LayoutRect,
    minimumEditorWidth: Int,
    minimumEditorHeight: Int,
    uiElementGap: Int
  ): PinnedPanelLayout =
    val requestedSizes = panels.flatMap { surface =>
      surface.presentation match
        case SurfacePresentation.Pinned(_, size) => Some(surface.id -> size.max(1))
        case _                                   => None
    }.toMap
    val nodeRects = calculateWorkspaceNodeRects(
      tree.root,
      workspaceRect,
      requestedSizes,
      minimumEditorWidth,
      minimumEditorHeight,
      uiElementGap
    )
    val surfaceRects = tree.dockedSurfaceIds.flatMap { surfaceId =>
      tree.nodeIdForSurface(surfaceId).flatMap(nodeRects.get).map(surfaceId -> _)
    }.toMap
    val panelRects = panels
      .flatMap { surface =>
        surface.presentation match
          case SurfacePresentation.Pinned(position, _) => surfaceRects.get(surface.id).map(position -> _)
          case _                                       => None
      }
      .groupMap(_._1)(_._2)
      .view
      // groupMap guarantees every value list is non-empty, so reduceOption always yields Some here;
      // flatMap over that Option keeps the reduce total instead of reaching for the partial `.reduce`.
      .flatMap { case (position, rects) => rects.reduceOption(unionRects).map(position -> _) }
      .toMap
    PinnedPanelLayout(panelRects, surfaceRects)

  private def calculateWorkspaceNodeRects(
    root: WorkspaceNode,
    workspaceRect: LayoutRect,
    requestedSizes: Map[SurfaceId, Int],
    minimumEditorWidth: Int,
    minimumEditorHeight: Int,
    uiElementGap: Int
  ): Map[WorkspaceNodeId, LayoutRect] =
    def requestedDockExtent(node: WorkspaceNode): Option[Int] =
      node.dockedSurfaceIds.flatMap(requestedSizes.get).maxOption

    def separatesDockFromEditor(first: WorkspaceNode, second: WorkspaceNode): Boolean =
      (first.paneIds.isEmpty && second.paneIds.nonEmpty) ||
        (second.paneIds.isEmpty && first.paneIds.nonEmpty)

    def minimumWidth(node: WorkspaceNode): Int =
      node match
        case _: WorkspaceNode.Leaf          => minimumEditorWidth
        case _: WorkspaceNode.DockedSurface => 1
        case split: WorkspaceNode.Split =>
          split.splitAxis match
            case SplitAxis.Horizontal =>
              minimumWidth(split.first) + minimumWidth(split.second) +
                (if separatesDockFromEditor(split.first, split.second) then uiElementGap else 0)
            case SplitAxis.Vertical =>
              minimumWidth(split.first).max(minimumWidth(split.second))

    def minimumHeight(node: WorkspaceNode): Int =
      node match
        case _: WorkspaceNode.Leaf          => minimumEditorHeight
        case _: WorkspaceNode.DockedSurface => 1
        case split: WorkspaceNode.Split =>
          split.splitAxis match
            case SplitAxis.Horizontal =>
              minimumHeight(split.first).max(minimumHeight(split.second))
            case SplitAxis.Vertical =>
              minimumHeight(split.first) + minimumHeight(split.second) +
                (if separatesDockFromEditor(split.first, split.second) then uiElementGap else 0)

    def childMinimums(split: WorkspaceNode.Split): (Int, Int) =
      val (firstMinimum, secondMinimum) =
        split.splitAxis match
          case SplitAxis.Horizontal => minimumWidth(split.first)  -> minimumWidth(split.second)
          case SplitAxis.Vertical   => minimumHeight(split.first) -> minimumHeight(split.second)
      if !separatesDockFromEditor(split.first, split.second) then firstMinimum -> secondMinimum
      else if split.first.paneIds.nonEmpty then (firstMinimum + uiElementGap) -> secondMinimum
      else firstMinimum                                                       -> (secondMinimum + uiElementGap)

    def recurse(node: WorkspaceNode, rect: LayoutRect): Map[WorkspaceNodeId, LayoutRect] =
      node match
        case leaf: WorkspaceNode.Leaf =>
          Map(leaf.id -> rect)
        case docked: WorkspaceNode.DockedSurface =>
          Map(docked.id -> rect)
        case split: WorkspaceNode.Split =>
          val total =
            split.splitAxis match
              case SplitAxis.Horizontal => rect.width
              case SplitAxis.Vertical   => rect.height
          val requestedExtent =
            if split.first.paneIds.isEmpty && split.second.paneIds.nonEmpty then
              requestedDockExtent(split.first).getOrElse(splitWorkspaceExtent(total, split.ratio))
            else if split.second.paneIds.isEmpty && split.first.paneIds.nonEmpty then
              requestedDockExtent(split.second)
                .map(size => total - size)
                .getOrElse(splitWorkspaceExtent(total, split.ratio))
            else splitWorkspaceExtent(total, split.ratio)
          val (minimumFirst, minimumSecond) = childMinimums(split)
          val extent                        = clampWorkspaceExtent(total, requestedExtent, minimumFirst, minimumSecond)
          val (firstRect, secondRect) =
            split.splitAxis match
              case SplitAxis.Horizontal =>
                (
                  rect.copy(width = extent),
                  LayoutRect(rect.x + extent, rect.y, rect.width - extent, rect.height)
                )
              case SplitAxis.Vertical =>
                (
                  rect.copy(height = extent),
                  LayoutRect(rect.x, rect.y + extent, rect.width, rect.height - extent)
                )
          Map(split.id -> rect) ++ recurse(split.first, firstRect) ++ recurse(split.second, secondRect)
    recurse(root, workspaceRect)

  private def splitWorkspaceExtent(total: Int, ratio: Double): Int =
    if total <= 1 then total
    else math.max(1, math.min(total - 1, (total * ratio).toInt))

  private def clampWorkspaceExtent(
    total: Int,
    requested: Int,
    minimumFirst: Int,
    minimumSecond: Int
  ): Int =
    if total <= 1 then total
    else
      val canRespectMinimums = minimumFirst + minimumSecond <= total
      val lower              = if canRespectMinimums then minimumFirst else 1
      val upper              = if canRespectMinimums then total - minimumSecond else total - 1
      requested.max(lower).min(upper)

  private def unionRects(first: LayoutRect, second: LayoutRect): LayoutRect =
    val x      = first.x.min(second.x)
    val y      = first.y.min(second.y)
    val right  = first.right.max(second.right)
    val bottom = first.bottom.max(second.bottom)
    LayoutRect(x, y, right - x, bottom - y)

  def pinnedPanelResizeFromDrag(
    state: AppState,
    viewportSize: ViewportSize,
    cellX: Int,
    cellY: Int
  ): Option[PinnedPanelDragResize] =
    val layout         = calculateLayoutWithUI(state, viewportSize)
    val contentHeight  = calculateContentHeight(state, viewportSize)
    val uiElementGap   = math.ceil(math.max(0.0, state.persisted.config.uiElementGap)).toInt
    val pinnedSurfaces = state.pinnedSurfaces
    val panelSizes = pinnedSurfaces.foldLeft(Map.empty[PanelPosition, Int]) {
      case (acc, UiSurface(_, _, SurfacePresentation.Pinned(position, size), _)) =>
        acc.updated(position, acc.get(position).fold(size)(_.max(size)))
      case (acc, _) =>
        acc
    }

    resizeFromDragRegion(layout, cellX, cellY).flatMap { position =>
      val requestedSize =
        position match
          case PanelPosition.Left   => cellX + 1
          case PanelPosition.Right  => viewportSize.width - cellX
          case PanelPosition.Top    => cellY + 1
          case PanelPosition.Bottom => contentHeight - cellY

      clampedPinnedPanelSize(position, requestedSize, panelSizes, viewportSize.width, contentHeight, uiElementGap)
        .map(PinnedPanelDragResize(position, _))
    }

  private def calculatePinnedPanelLayout(
    panels: List[UiSurface],
    terminalWidth: Int,
    contentHeight: Int,
    uiElementGap: Int
  ): PinnedPanelLayout =
    val panelsByPosition = panels.foldLeft(Map.empty[PanelPosition, List[(UiSurface, Int)]]) {
      case (acc, surface) =>
        surface.presentation match
          case SurfacePresentation.Pinned(position, size) =>
            acc.updated(position, acc.getOrElse(position, Nil) :+ (surface -> size))
          case _ =>
            acc
    }
    // Each entry in panelsByPosition is built by appending, so a key is only ever present with a
    // non-empty list -- Int.MinValue is never the reported result.
    val panelSizes = panelsByPosition.view.mapValues(_.map(_._2).foldLeft(Int.MinValue)(_ max _)).toMap
    val verticalSizes = calculatePinnedAxisSizes(
      panelSizes.get(PanelPosition.Top),
      panelSizes.get(PanelPosition.Bottom),
      contentHeight,
      uiElementGap
    )
    val topHeight          = verticalSizes.start
    val bottomHeight       = verticalSizes.end
    val verticalZoneY      = topHeight
    val verticalZoneHeight = math.max(1, contentHeight - topHeight - bottomHeight)

    val horizontalSizes = calculatePinnedAxisSizes(
      panelSizes.get(PanelPosition.Left),
      panelSizes.get(PanelPosition.Right),
      terminalWidth,
      uiElementGap
    )
    val leftWidth  = horizontalSizes.start
    val rightWidth = horizontalSizes.end

    val rects = List.newBuilder[(PanelPosition, LayoutRect)]

    if topHeight > 0 then rects += PanelPosition.Top -> LayoutRect(0, 0, terminalWidth, topHeight)
    if bottomHeight > 0 then
      rects += PanelPosition.Bottom -> LayoutRect(0, contentHeight - bottomHeight, terminalWidth, bottomHeight)
    if leftWidth > 0 then rects += PanelPosition.Left -> LayoutRect(0, verticalZoneY, leftWidth, verticalZoneHeight)
    if rightWidth > 0 then
      rects += PanelPosition.Right -> LayoutRect(
        terminalWidth - rightWidth,
        verticalZoneY,
        rightWidth,
        verticalZoneHeight
      )

    val panelRects   = rects.result().toMap
    val surfaceRects = calculatePinnedSurfaceRects(panelsByPosition, panelRects)

    PinnedPanelLayout(panelRects, surfaceRects)

  private def resizeFromDragRegion(
    layout: CalculatedLayout,
    cellX: Int,
    cellY: Int
  ): Option[PanelPosition] =
    layout.pinnedPanelRects.collectFirst {
      case (PanelPosition.Left, rect)
          if LayoutRect(
            rect.x,
            rect.y,
            (layout.leftSpacerRect.x - rect.x + PinnedPanelDragWorkspaceReach).max(rect.width),
            rect.height
          ).contains(cellX, cellY) =>
        PanelPosition.Left
      case (PanelPosition.Right, rect)
          if LayoutRect(
            (layout.rightSpacerRect.right - PinnedPanelDragWorkspaceReach).min(rect.x),
            rect.y,
            (rect.right - (layout.rightSpacerRect.right - PinnedPanelDragWorkspaceReach)).max(rect.width),
            rect.height
          ).contains(cellX, cellY) =>
        PanelPosition.Right
      case (PanelPosition.Top, rect)
          if LayoutRect(
            rect.x,
            rect.y,
            rect.width,
            (layout.editorPanelRect.y - rect.y + PinnedPanelDragWorkspaceReach).max(rect.height)
          ).contains(cellX, cellY) =>
        PanelPosition.Top
      case (PanelPosition.Bottom, rect)
          if LayoutRect(
            rect.x,
            (layout.editorPanelRect.bottom - PinnedPanelDragWorkspaceReach).min(rect.y),
            rect.width,
            (rect.bottom - (layout.editorPanelRect.bottom - PinnedPanelDragWorkspaceReach)).max(rect.height)
          ).contains(cellX, cellY) =>
        PanelPosition.Bottom
    }

  private def clampedPinnedPanelSize(
    position: PanelPosition,
    requestedSize: Int,
    panelSizes: Map[PanelPosition, Int],
    terminalWidth: Int,
    contentHeight: Int,
    uiElementGap: Int
  ): Option[Int] =
    position match
      case PanelPosition.Left =>
        panelSizes.get(PanelPosition.Left).map { _ =>
          calculatePinnedAxisSizes(
            Some(requestedSize),
            panelSizes.get(PanelPosition.Right),
            terminalWidth,
            uiElementGap
          ).start
        }
      case PanelPosition.Right =>
        panelSizes.get(PanelPosition.Right).map { _ =>
          calculatePinnedAxisSizes(
            panelSizes.get(PanelPosition.Left),
            Some(requestedSize),
            terminalWidth,
            uiElementGap
          ).end
        }
      case PanelPosition.Top =>
        panelSizes.get(PanelPosition.Top).map { _ =>
          calculatePinnedAxisSizes(
            Some(requestedSize),
            panelSizes.get(PanelPosition.Bottom),
            contentHeight,
            uiElementGap
          ).start
        }
      case PanelPosition.Bottom =>
        panelSizes.get(PanelPosition.Bottom).map { _ =>
          calculatePinnedAxisSizes(
            panelSizes.get(PanelPosition.Top),
            Some(requestedSize),
            contentHeight,
            uiElementGap
          ).end
        }

  private def calculateContentHeight(state: AppState, viewportSize: ViewportSize): Int =
    val densityMetrics = InterfaceDensityMetrics.forDensity(state.persisted.config.interfaceDensity)
    val gutterHeight   = if usesBottomGutter(state) then densityMetrics.gutterHeight else 0
    math.max(1, viewportSize.height - gutterHeight)

  private def calculatePinnedAxisSizes(
    startSize: Option[Int],
    endSize: Option[Int],
    total: Int,
    uiElementGap: Int
  ): PinnedAxisSizes =
    val requestedStart = math.max(0, startSize.getOrElse(0))
    val requestedEnd   = math.max(0, endSize.getOrElse(0))
    val hasStart       = requestedStart > 0
    val hasEnd         = requestedEnd > 0
    val reservedGap =
      (if hasStart then uiElementGap else 0) +
        (if hasEnd then uiElementGap else 0)
    val panelBudget = math.max(0, total - reservedGap - 1)
    val endMinimum  = if hasEnd && panelBudget > 1 then 1 else 0
    val startBudget = math.max(0, panelBudget - endMinimum)
    val start       = if hasStart then math.min(requestedStart, startBudget) else 0
    val endBudget   = math.max(0, panelBudget - start)
    val end         = if hasEnd then math.min(requestedEnd, endBudget) else 0

    PinnedAxisSizes(start, end)

  private def calculatePinnedSurfaceRects(
    panelsByPosition: Map[PanelPosition, List[(UiSurface, Int)]],
    panelRects: Map[PanelPosition, LayoutRect]
  ): Map[SurfaceId, LayoutRect] =
    panelsByPosition.toList.flatMap {
      case (position, panelsAtPosition) =>
        panelRects.get(position).toList.flatMap { panelRect =>
          splitPanelRect(position, panelRect, panelsAtPosition.size).zip(panelsAtPosition).map {
            case (rect, (surface, _)) => surface.id -> rect
          }
        }
    }.toMap

  private def splitPanelRect(position: PanelPosition, rect: LayoutRect, panelCount: Int): List[LayoutRect] =
    if panelCount <= 0 then Nil
    else
      position match
        case PanelPosition.Left | PanelPosition.Right =>
          splitSegments(rect.y, rect.height, panelCount).map {
            case (y, height) =>
              rect.copy(y = y, height = height)
          }
        case PanelPosition.Top | PanelPosition.Bottom =>
          splitSegments(rect.x, rect.width, panelCount).map {
            case (x, width) =>
              rect.copy(x = x, width = width)
          }

  private def splitSegments(start: Int, total: Int, count: Int): List[(Int, Int)] =
    val base      = total / count
    val remainder = total % count
    (0 until count).toList
      .foldLeft((start, List.empty[(Int, Int)])) {
        case ((currentStart, acc), index) =>
          val size = base + (if index < remainder then 1 else 0)
          (currentStart + size, acc :+ (currentStart -> size))
      }
      ._2

  private def calculateLineNumberWidth(state: AppState): Int =
    // Find the maximum line count across all buffers to determine width needed
    val maxLines =
      if state.persisted.buffers.isEmpty then 10
      else state.persisted.buffers.values.map(_.document.content.lineCount).foldLeft(Int.MinValue)(_ max _)

    math.max(3, maxLines.toString.length + 1) // +1 for spacing, minimum 3 chars

  private def calculateFloatingSurfaceRect(
    surface: UiSurface,
    state: AppState,
    paneLayouts: Map[PaneId, EditorPaneLayout],
    topYOverride: Option[Int] = None,
    forcedHeight: Option[Int] = None
  ): Option[LayoutRect] =
    for
      paneId     <- state.persisted.layout.activeEditorPaneId
      pane       <- state.persisted.layout.editorPanes.get(paneId)
      paneLayout <- paneLayouts.get(paneId)
      bufferId   <- pane.bufferId
      buffer     <- state.persisted.buffers.get(bufferId)
      rect <- calculateFloatingSurfaceRect(surface, buffer, paneLayout.contentRect, state, topYOverride, forcedHeight)
    yield rect

  private def calculateFloatingSurfaceRect(
    surface: UiSurface,
    buffer: Buffer,
    contentRect: LayoutRect,
    state: AppState,
    topYOverride: Option[Int],
    forcedHeight: Option[Int]
  ): Option[LayoutRect] =
    surface.content match
      case SurfaceContent.CommandRunnerPeek(_) =>
        calculateFrozenCursorPeekRect(surface, contentRect, state)
      case _ =>
        calculateLiveFloatingSurfaceRect(surface, buffer, contentRect, state, topYOverride, forcedHeight)

  /** The cursor-peek prototype's own rect resolution -- deliberately never calls [[floatingAnchor]] or
    * `CursorLayout.calculateScreenPositionInContent`: `state.runtime.cursorPeekResolvedAnchor` was already resolved
    * once, at render time, by `CursorPeekAnchorResolution` (state.manager), and is reused verbatim here on every
    * subsequent paint rather than re-derived, so a reformat underneath an open peek cannot move it. Sizing
    * (`calculateFloatingSurfaceWidth`/`calculateFloatingSurfaceHeight`) is still shared with the live path for a
    * consistent look.
    */
  private def calculateFrozenCursorPeekRect(
    surface: UiSurface,
    contentRect: LayoutRect,
    state: AppState
  ): Option[LayoutRect] =
    for
      anchorScreenPosition <- state.runtime.cursorPeekResolvedAnchor
      placement <- surface.presentation match
        case SurfacePresentation.Floating(_, p) => Some(p)
        case _                                  => None
      preferredWidth  = calculateFloatingSurfaceWidth(contentRect.width)
      preferredHeight = calculateFloatingSurfaceHeight(surface.content, preferredWidth, contentRect.height, state)
      gapRows         = wholeRowOrigin(floatingCursorGapRows(state, surface.content))
      slot            = FrozenPeekSlot(surface.id, preferredWidth, preferredHeight)
      placed <- resolveFrozenCursorPeekStack(
        List(slot),
        anchorScreenPosition,
        contentRect,
        placement,
        gapRows
      ).headOption
    yield placed.rect

  private def calculateLiveFloatingSurfaceRect(
    surface: UiSurface,
    buffer: Buffer,
    contentRect: LayoutRect,
    state: AppState,
    topYOverride: Option[Int],
    forcedHeight: Option[Int]
  ): Option[LayoutRect] =
    val borderCells = SurfaceFrameLayout.borderCellsFor(surface.content)
    val preferredWidth = surface.content match
      case SurfaceContent.ContextualToolbar(toolbarState) =>
        ContextualToolbarLayout.compactContentWidth(
          toolbarState,
          state,
          contentRect.width - (borderCells * 2)
        ) + (borderCells * 2)
      case SurfaceContent.CommandPalette(_) | SurfaceContent.ShortcutsHelp(_) |
          SurfaceContent.ModalWorkflow(Modal.FileWorkflow(_)) =>
        calculateFloatingSurfaceWidth(contentRect.width)
      case _ =>
        contentRect.width
    val preferredHeight = calculateFloatingSurfaceHeight(surface.content, preferredWidth, contentRect.height, state)
    val finalHeight     = forcedHeight.getOrElse(preferredHeight)
    val gapRows         = wholeRowOrigin(floatingCursorGapRows(state, surface.content))

    for
      anchor <- floatingAnchor(surface, state, buffer)
      screenPosition <- CursorLayout.calculateScreenPositionInContent(
        anchor,
        buffer.document.content,
        contentRect,
        buffer.viewport,
        state.persisted.config.surfaceConfig.wordWrapEnabled
      )
      if surface.content match
        case SurfaceContent.ContextualToolbar(_) => contentRect.contains(screenPosition.x, screenPosition.y)
        case _                                   => true
    yield
      val horizontalAnchorX = toolbarSelectionEndScreenPosition(surface, buffer, contentRect, state)
        .filter(_.y == screenPosition.y)
        .map(selectionEnd => (screenPosition.x + selectionEnd.x) / 2)
        .getOrElse(screenPosition.x)
      val overlayX = surface.content match
        // The command palette/settings surface -- and the shortcuts-help reference (issue #1247), for the same
        // reason -- is horizontally centered on screen, not cursor-anchored: unlike the contextual toolbar (anchored
        // to a text selection) or other floating content, its width and content bear no relationship to the cursor's
        // horizontal position, and cursor-anchoring left it pinned near whichever column the caret happened to be in
        // -- often far from center, sometimes hard against an edge. Vertical placement (above/below the cursor,
        // `overlayY` below) is unaffected.
        case SurfaceContent.CommandPalette(_) | SurfaceContent.ShortcutsHelp(_) |
            SurfaceContent.ModalWorkflow(Modal.FileWorkflow(_)) =>
          contentRect.x + math.max(0, (contentRect.width - preferredWidth) / 2)
        case _ =>
          math.max(
            contentRect.x,
            math.min(horizontalAnchorX - (preferredWidth / 2), contentRect.right - preferredWidth)
          )
      val preferredAboveY = screenPosition.y - finalHeight - gapRows
      val preferredBelowY = toolbarSelectionEndScreenPosition(surface, buffer, contentRect, state)
        .map(_.y + 1 + gapRows)
        .getOrElse(screenPosition.y + 1 + gapRows)
      val overlayY = topYOverride.getOrElse(surface.presentation match
        case SurfacePresentation.Floating(_, SurfacePlacement.AboveCursor) =>
          surface.content match
            case SurfaceContent.ContextualToolbar(_)
                if preferredAboveY < contentRect.y &&
                  preferredBelowY + finalHeight <= contentRect.bottom =>
              preferredBelowY
            case _ =>
              math.max(contentRect.y, preferredAboveY)
        case SurfacePresentation.Floating(_, SurfacePlacement.BelowCursor) =>
          surface.content match
            case SurfaceContent.ContextualToolbar(_) if preferredAboveY >= contentRect.y =>
              preferredAboveY
            case _ if preferredBelowY + finalHeight <= contentRect.bottom =>
              preferredBelowY
            case _ =>
              math.max(contentRect.y, screenPosition.y - finalHeight - gapRows)
        case _ =>
          contentRect.y)

      LayoutRect(
        x = overlayX,
        y = overlayY,
        width = preferredWidth,
        height = finalHeight
      )

  private def toolbarSelectionEndScreenPosition(
    surface: UiSurface,
    buffer: Buffer,
    contentRect: LayoutRect,
    state: AppState
  ): Option[ScreenPosition] =
    surface.content match
      case SurfaceContent.ContextualToolbar(_) =>
        buffer.primarySelection.flatMap(selection =>
          CursorLayout.calculateScreenPositionInContent(
            selection.end,
            buffer.document.content,
            contentRect,
            buffer.viewport,
            state.persisted.config.surfaceConfig.wordWrapEnabled
          )
        )
      case _ =>
        None

  final private case class FloatingAnchorFrame(contentRect: LayoutRect, screenPosition: ScreenPosition)

  private def calculateFloatingAnchorFrame(
    surface: UiSurface,
    state: AppState,
    paneLayouts: Map[PaneId, EditorPaneLayout]
  ): Option[FloatingAnchorFrame] =
    for
      paneId     <- state.persisted.layout.activeEditorPaneId
      pane       <- state.persisted.layout.editorPanes.get(paneId)
      paneLayout <- paneLayouts.get(paneId)
      bufferId   <- pane.bufferId
      buffer     <- state.persisted.buffers.get(bufferId)
      anchor     <- floatingAnchor(surface, state, buffer)
      screenPosition <- CursorLayout.calculateScreenPositionInContent(
        anchor,
        buffer.document.content,
        paneLayout.contentRect,
        buffer.viewport,
        state.persisted.config.surfaceConfig.wordWrapEnabled
      )
    yield FloatingAnchorFrame(paneLayout.contentRect, screenPosition)

  final private case class BelowOverlayLayout(
      stack: List[(SurfaceId, LayoutRect)],
      collapsedSurfaceIds: Set[SurfaceId]
  )

  private def orderedBelowCursorSurfaces(state: AppState): List[UiSurface] =
    val maybeToolbar = state.contextualToolbarSurface.filter(isBelowCursorSurface).toList
    val maybeRunner  = state.commandRunnerSurface.filter(isBelowCursorSurface).toList
    if maybeToolbar.nonEmpty && maybeRunner.nonEmpty then maybeToolbar ++ maybeRunner
    else
      val belowSurfaces = state.floatingSurfaces.filter {
        _.presentation match
          case SurfacePresentation.Floating(_, SurfacePlacement.BelowCursor) => true
          case _                                                             => false
      }
      state.persisted.focus match
        case Focus.Surface(surfaceId) =>
          belowSurfaces.find(_.id == surfaceId) match
            case Some(focused) => List(focused)
            case None          => belowSurfaces.headOption.toList
        case _ =>
          if belowSurfaces.nonEmpty then belowSurfaces.headOption.toList
          else
            state.cursorInfoBarSurface.filter {
              _.presentation match
                case SurfacePresentation.Floating(_, SurfacePlacement.BelowCursor) => true
                case _                                                             => false
            }.toList

  private def isBelowCursorSurface(surface: UiSurface): Boolean =
    surface.presentation match
      case SurfacePresentation.Floating(_, SurfacePlacement.BelowCursor) => true
      case _                                                             => false

  private def calculateBelowCursorOverlayStack(
    surfaces: List[UiSurface],
    state: AppState,
    paneLayouts: Map[PaneId, EditorPaneLayout]
  ): BelowOverlayLayout =
    if surfaces.isEmpty then BelowOverlayLayout(Nil, Set.empty)
    else if surfaces.length == 1 then
      BelowOverlayLayout(
        surfaces.flatMap(surface => calculateFloatingSurfaceRect(surface, state, paneLayouts).map(surface.id -> _)),
        Set.empty
      )
    else
      // The command-runner two-surface stack case (main palette + its settings-group submenu) is gone -- a settings
      // group now renders on the one `CommandPalette` surface instead of a second floating one (issue #1059), so
      // every below-cursor multi-surface case now goes through the same generic stacking.
      stackBelowCursorSurfaces(surfaces, state, paneLayouts)

  private def calculateFloatingSurfaceWidth(maxWidth: Int): Int =
    math.min(math.max(0, maxWidth), 72)

  private def floatingCursorGapRows(state: AppState, content: SurfaceContent): Double =
    content match
      case SurfaceContent.CommandPalette(_) =>
        math.max(
          0.0,
          state.persisted.config.surfaceConfig.commandRunnerCursorGapRows.getOrElse(floatingStackGapRows(state))
        )
      case _ => floatingStackGapRows(state)

  private def floatingStackGapRows(state: AppState): Double =
    Option
      .when(state.persisted.config.uiElementGap > 0.0)(state.persisted.config.uiElementGap)
      .getOrElse(InterfaceDensityMetrics.forDensity(state.persisted.config.interfaceDensity).overlayGapRows.toDouble)

  private def wholeRowOrigin(rows: Double): Int =
    math.floor(math.max(0.0, rows)).toInt

  private def floatingOverlayOffsets(
    aboveSurfaces: List[UiSurface],
    aboveRects: List[(SurfaceId, LayoutRect)],
    belowSurfaces: List[UiSurface],
    belowRects: List[(SurfaceId, LayoutRect)],
    state: AppState,
    paneLayouts: Map[PaneId, EditorPaneLayout]
  ): Map[SurfaceId, Double] =
    val aboveById = aboveSurfaces.map(surface => surface.id -> surface).toMap
    val aboveOffsets = aboveRects.flatMap { (surfaceId, rect) =>
      aboveById.get(surfaceId).flatMap { surface =>
        val gap = floatingCursorGapRows(state, surface.content)
        calculateFloatingAnchorFrame(surface, state, paneLayouts).map { anchorFrame =>
          surfaceId -> clampedFloatingOffset(
            rect,
            anchorFrame.contentRect,
            wholeRowOrigin(gap).toDouble - gap
          )
        }
      }
    }
    val firstBelowDirection = for
      surface   <- belowSurfaces.headOption
      (_, rect) <- belowRects.headOption
      anchorY   <- calculateFloatingAnchorFrame(surface, state, paneLayouts).map(_.screenPosition.y)
    yield if rect.y <= anchorY then -1.0 else 1.0
    val cursorRemainder = belowSurfaces.headOption
      .map(surface =>
        val gap = floatingCursorGapRows(state, surface.content)
        gap - wholeRowOrigin(gap)
      )
      .getOrElse(0.0)
    val stackGap        = floatingStackGapRows(state)
    val stackRemainder  = stackGap - wholeRowOrigin(stackGap)
    val direction       = firstBelowDirection.getOrElse(1.0)
    val belowSurfaceIds = belowSurfaces.map(_.id).toSet
    val belowContentRect = belowSurfaces.headOption
      .flatMap(surface => calculateFloatingAnchorFrame(surface, state, paneLayouts))
      .map(_.contentRect)
    val belowOffsets = belowRects.zipWithIndex.collect {
      case ((surfaceId, rect), index) if belowSurfaceIds.contains(surfaceId) =>
        val desiredOffset = direction * (cursorRemainder + index * stackRemainder)
        surfaceId -> belowContentRect
          .map(contentRect => clampedFloatingOffset(rect, contentRect, desiredOffset))
          .getOrElse(desiredOffset)
    }
    (aboveOffsets ++ belowOffsets).toMap

  private def clampedFloatingOffset(rect: LayoutRect, contentRect: LayoutRect, desiredOffset: Double): Double =
    math.max(contentRect.y - rect.y, math.min(desiredOffset, contentRect.bottom - rect.bottom))

  private def calculateFloatingSurfaceHeight(
    content: SurfaceContent,
    maxWidth: Int,
    maxHeight: Int,
    state: AppState
  ): Int =
    val densityMetrics = InterfaceDensityMetrics.forDensity(state.persisted.config.interfaceDensity)
    val commandMaxHeight =
      state.persisted.config.surfaceConfig.commandRunnerVisibleRows
        .map(rows =>
          SurfaceFrameLayout.frameHeightForItemRows(
            AppConfig.clampCommandRunnerVisibleRows(rows),
            hasHeader = true,
            hasFooter = true,
            borderCells = SurfaceFrameLayout.CommandSurfaceBorderCells,
            itemGapRows = state.persisted.config.surfaceConfig.commandRunnerItemGapRows,
            itemTargetRows = SurfaceFrameLayout.minimumTargetRows(state.persisted.config.interfaceDensity)
          )
        )
        .getOrElse(densityMetrics.commandSurfaceMaxHeight)
    val preferredHeight = content match
      case SurfaceContent.StartPage(_)            => maxHeight
      case SurfaceContent.QuickInfo(text)         => math.max(3, text.linesIterator.size + 2)
      case SurfaceContent.FilePreview(_, content) => math.max(4, math.min(6, content.linesIterator.take(4).size + 2))
      case SurfaceContent.SymbolDefinition(_, _)  => 4
      case SurfaceContent.CursorInfoBar(_)        => 3
      case SurfaceContent.DirectoryListing(_, entries, _) => math.max(4, math.min(6, entries.take(4).size + 2))
      case SurfaceContent.DirectoryTree(tree, _) =>
        math.max(4, math.min(8, DirectoryTreeData.visibleRows(tree).size + 2))
      case SurfaceContent.CommandPalette(runner) if runner.isSettingsSurface =>
        math.min(commandMaxHeight, math.max(densityMetrics.commandSurfaceMinHeight, maxHeight - 1))
      case SurfaceContent.CommandPalette(_) =>
        math.min(
          commandMaxHeight,
          math.max(densityMetrics.commandSurfaceMinHeight, maxHeight - 1)
        )
      case SurfaceContent.CommandRunnerPeek(_) =>
        math.min(
          commandMaxHeight,
          math.max(densityMetrics.commandSurfaceMinHeight, maxHeight - 1)
        )
      case SurfaceContent.ThemePicker(_) | SurfaceContent.ThemeCreator(_) | SurfaceContent.FileSearch(_) =>
        math.min(
          densityMetrics.commandSurfaceMaxHeight,
          math.max(densityMetrics.commandSurfaceMinHeight, maxHeight - 1)
        )
      case SurfaceContent.ContextualToolbar(toolbarState) =>
        val borderCells  = SurfaceFrameLayout.borderCellsFor(content)
        val contentWidth = (maxWidth - (borderCells * 2)).max(1)
        val toolbarRows  = ContextualToolbarLayout.rowCount(toolbarState, state, contentWidth)
        SurfaceFrameLayout.frameHeightForItemRows(
          toolbarRows,
          hasHeader = false,
          hasFooter = false,
          borderCells = borderCells,
          itemGapRows = state.persisted.config.uiElementGap,
          itemTargetRows = SurfaceFrameLayout.itemTargetRowsFor(content, state.persisted.config.interfaceDensity)
        )
      case SurfaceContent.ContextMenu(menu) =>
        SurfaceFrameLayout.frameHeightForItemRows(
          itemRows = menu.items.length,
          hasHeader = true,
          hasFooter = menu.items.nonEmpty,
          borderCells = SurfaceFrameLayout.borderCellsFor(content),
          itemGapRows = state.persisted.config.surfaceConfig.commandRunnerItemGapRows,
          itemTargetRows = SurfaceFrameLayout.itemTargetRowsFor(content, state.persisted.config.interfaceDensity)
        )
      case SurfaceContent.CommentLens(lens) =>
        math.max(4, math.min(8, lens.draft.split("\n", -1).length + 3))
      case SurfaceContent.ModalWorkflow(modal) =>
        ModalSurfaceComposition.frameHeight(
          modal,
          SurfaceFrameLayout.minimumTargetRows(state.persisted.config.interfaceDensity)
        )
      case SurfaceContent.Terminal(_, _) | SurfaceContent.Outline(_, _) | SurfaceContent.Comments(_, _) |
          SurfaceContent.Diagnostics(_, _) | SurfaceContent.MarkdownPreview(_, _) =>
        math.min(8, math.max(4, maxHeight - 1))
      case SurfaceContent.ShortcutsHelp(groups) =>
        // Wants enough rows for every group heading plus its entries, but never more than the viewport allows --
        // `resolveShortcutsHelp` clips to whatever height it is actually given.
        math.min(maxHeight - 1, math.max(4, groups.map(g => g.entries.size + 1).sum + 2))
      case SurfaceContent.TabList(entries, _) =>
        // Scales with the number of open tabs rather than the fixed `commandSurfaceMaxHeight` cap the command
        // palette itself is still capped by (issue #1045) -- a many-tab session should see all of its tabs, not
        // just the ~3 that cap would allow.
        math.min(maxHeight - 1, math.max(4, entries.size + 2))
      case SurfaceContent.RecentFilesInMode(_, paths) =>
        math.min(maxHeight - 1, math.max(4, paths.size + 2))
      case SurfaceContent.GhostOverlay(_, cachedRect) =>
        cachedRect.height

    math.max(3, math.min(maxHeight, preferredHeight))

  private def surfaceAnchor(surface: UiSurface): Option[CursorPosition] =
    surface.presentation match
      case SurfacePresentation.Floating(anchor, _) => anchor
      case _                                       => None

  private def floatingAnchor(
    surface: UiSurface,
    state: AppState,
    activeBuffer: Buffer
  ): Option[CursorPosition] =
    surface.content match
      case SurfaceContent.ContextualToolbar(_) =>
        activeBuffer.primarySelection.map(_.start).orElse(state.activeCursorPosition).orElse(surfaceAnchor(surface))
      case SurfaceContent.CommandPalette(_) =>
        state.activeCursorPosition.orElse(surfaceAnchor(surface))
      case _ =>
        surfaceAnchor(surface).orElse(state.activeCursorPosition)

  /** A panel's slot in a frozen cursor-peek stack: an id plus preferred size. Ordered-list-with-insertion shape
    * (`List[FrozenPeekSlot]`, not a single surface) so [[resolveFrozenCursorPeekStack]] is already the general
    * multi-panel case even though the cursor-peek prototype only ever passes one slot today.
    */
  final case class FrozenPeekSlot(id: SurfaceId, preferredWidth: Int, preferredHeight: Int)

  final case class FrozenPeekPlacement(id: SurfaceId, rect: LayoutRect)

  /** Resolves a frozen-anchor cursor-peek stack, box-layout style: each slot in `slots` is stacked in order starting
    * from `anchorScreenPosition`, on the side `placement` prefers, falling back to the other side and then clamping
    * within `contentRect` when neither side has room -- the same height-budget clamp [[stackBelowCursorSurfaces]]
    * already uses for its own (live) stack, reused rather than inventing a second overflow mechanism. A slot with no
    * height budget left is dropped from the result entirely rather than rendered at zero height.
    *
    * Deliberately distinct from [[floatingAnchor]]/[[calculateFloatingSurfaceRect]]: `anchorScreenPosition` is supplied
    * once by the caller (captured from the cursor's *line* at summon time) rather than derived here from `AppState`/the
    * active buffer, so this function never re-reads live cursor state -- callers that want the cursor-peek prototype's
    * "frozen for the whole session, even across a reformat" behaviour resolve the anchor once and hold onto the result;
    * callers of the existing floating surfaces keep re-deriving it every layout pass, unchanged.
    */
  def resolveFrozenCursorPeekStack(
    slots: List[FrozenPeekSlot],
    anchorScreenPosition: ScreenPosition,
    contentRect: LayoutRect,
    placement: SurfacePlacement,
    gapRows: Int
  ): List[FrozenPeekPlacement] =
    if slots.isEmpty then Nil
    else
      val totalHeight     = slots.map(_.preferredHeight).sum + (gapRows * (slots.length - 1).max(0))
      val preferredBelowY = anchorScreenPosition.y + 1 + gapRows
      val preferredAboveY = anchorScreenPosition.y - gapRows - totalHeight
      val fitsBelow       = preferredBelowY + totalHeight <= contentRect.bottom
      val fitsAbove       = preferredAboveY >= contentRect.y

      def clamped: Int =
        math.max(
          contentRect.y,
          math.min(preferredBelowY, contentRect.bottom - math.min(totalHeight, contentRect.height))
        )

      val stackY = placement match
        case SurfacePlacement.BelowCursor =>
          if fitsBelow then preferredBelowY else if fitsAbove then preferredAboveY else clamped
        case SurfacePlacement.AboveCursor =>
          if fitsAbove then preferredAboveY else if fitsBelow then preferredBelowY else clamped

      val (_, placed) = slots.foldLeft((stackY, List.empty[FrozenPeekPlacement])) {
        case ((currentY, acc), slot) =>
          val heightBudget   = math.max(0, contentRect.bottom - currentY)
          val adjustedHeight = math.min(slot.preferredHeight, heightBudget)
          val adjustedY      = if adjustedHeight == 0 then contentRect.bottom else currentY
          val width          = math.min(slot.preferredWidth, contentRect.width)
          val x = math.max(
            contentRect.x,
            math.min(anchorScreenPosition.x - (width / 2), contentRect.right - width)
          )
          val rect = LayoutRect(x = x, y = adjustedY, width = width, height = adjustedHeight)
          (adjustedY + adjustedHeight + gapRows, acc :+ FrozenPeekPlacement(slot.id, rect))
      }
      placed.filter(_.rect.height > 0)

  private def stackBelowCursorSurfaces(
    surfaces: List[UiSurface],
    state: AppState,
    paneLayouts: Map[PaneId, EditorPaneLayout]
  ): BelowOverlayLayout =
    val baseRects =
      surfaces.flatMap(surface => calculateFloatingSurfaceRect(surface, state, paneLayouts).map(surface -> _))
    val anchorFrameOpt =
      surfaces.headOption.flatMap(surface => calculateFloatingAnchorFrame(surface, state, paneLayouts))
    anchorFrameOpt match
      case None =>
        BelowOverlayLayout(Nil, Set.empty)
      case Some(_) if baseRects.isEmpty =>
        BelowOverlayLayout(Nil, Set.empty)
      case Some(anchorFrame) =>
        val gapRows = surfaces.headOption
          .map(surface => wholeRowOrigin(floatingCursorGapRows(state, surface.content)))
          .getOrElse(0)
        val stackGapRows    = wholeRowOrigin(floatingStackGapRows(state))
        val availableBottom = anchorFrame.contentRect.bottom
        val totalHeight     = baseRects.map(_._2.height).sum + (stackGapRows * (baseRects.length - 1).max(0))
        val preferredBelowY = anchorFrame.screenPosition.y + 1 + gapRows
        val preferredAboveY = anchorFrame.screenPosition.y - gapRows - totalHeight
        val stackY =
          if preferredBelowY + totalHeight <= availableBottom then preferredBelowY
          else if preferredAboveY >= anchorFrame.contentRect.y then preferredAboveY
          else
            math.max(
              anchorFrame.contentRect.y,
              math.min(preferredBelowY, availableBottom - math.min(totalHeight, anchorFrame.contentRect.height))
            )
        val (_, stacked) = baseRects.foldLeft((stackY, List.empty[(SurfaceId, LayoutRect)])) {
          case ((currentY, acc), (surface, rect)) =>
            val heightBudget   = math.max(0, availableBottom - currentY)
            val adjustedHeight = math.min(rect.height, heightBudget)
            val adjustedY      = if adjustedHeight == 0 then availableBottom else currentY
            (
              adjustedY + adjustedHeight + stackGapRows,
              acc :+ (surface.id -> rect.copy(y = adjustedY, height = adjustedHeight))
            )
        }
        BelowOverlayLayout(stacked.filter(_._2.height > 0), Set.empty)

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
    // A stored tree that predates a direct state update can omit panes the update just added; trust it
    // only when it still accounts for every current editor pane, matching Layout.effectiveWorkspaceTree.
    val editorPaneIds = state.persisted.layout.editorPanes.keySet
    state.persisted.layout.workspaceTree.filter(tree => editorPaneIds.subsetOf(tree.paneIds.toSet)) match
      case Some(tree) =>
        calculateWorkspaceTreePaneRects(tree, calculatedLayout.editorPanelRect, minWidth)
          .filter { case (paneId, _) => editorPaneIds.contains(paneId) }
      case None =>
        val editorRect = calculatedLayout.editorPanelRect
        val paneIds    = state.persisted.layout.orderedPaneIds
        val paneCount  = paneIds.size

        if paneCount == 0 then Map.empty
        else if paneCount == 1 then
          // Single pane uses full editor area. paneCount == 1 guarantees headOption is Some here.
          paneIds.headOption.fold(Map.empty[PaneId, LayoutRect])(paneId => Map(paneId -> editorRect))
        else
          state.persisted.layout.splitDirection match
            case PaneSplitDirection.Horizontal =>
              calculateHorizontalPaneLayouts(state, editorRect, paneIds, minWidth)
            case PaneSplitDirection.Vertical =>
              calculateVerticalPaneLayouts(state, editorRect, paneIds)

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
        case WorkspaceNode.Leaf(_, _)             => MinimumVerticalPaneHeight
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
    val headerHeight   = paneHeaderHeight(state)
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
    contentRectForPane(paneRect, TextAreaInsets(), EditorPaneHeaderHeight)

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

  private def calculateHorizontalPaneLayouts(
    state: AppState,
    editorRect: LayoutRect,
    paneIds: List[PaneId],
    minWidth: Int
  ): Map[PaneId, LayoutRect] =
    val paneCount        = paneIds.size
    val maxVisiblePanes  = math.max(1, editorRect.width / minWidth)
    val visiblePaneCount = math.min(paneCount, maxVisiblePanes)
    val paneWidth        = math.max(minWidth, editorRect.width / visiblePaneCount)
    val focusedPaneId    = focusedPane(state)
    val (visibleStartIndex, _) = calculateVisiblePaneWindow(
      paneIds,
      focusedPaneId,
      visiblePaneCount
    )

    paneIds.zipWithIndex.map {
      case (paneId, globalIndex) =>
        val visibleIndex = globalIndex - visibleStartIndex
        val isVisible    = visibleIndex >= 0 && visibleIndex < visiblePaneCount
        val paneRect =
          if isVisible then
            LayoutRect(
              x = editorRect.x + (visibleIndex * paneWidth),
              y = editorRect.y,
              width = paneWidth,
              height = editorRect.height
            )
          else
            val offScreenX =
              if visibleIndex < 0 then editorRect.x - 100
              else editorRect.x + editorRect.width + 100

            LayoutRect(
              x = offScreenX,
              y = editorRect.y,
              width = paneWidth,
              height = editorRect.height
            )

        paneId -> paneRect
    }.toMap

  private def calculateVerticalPaneLayouts(
    state: AppState,
    editorRect: LayoutRect,
    paneIds: List[PaneId]
  ): Map[PaneId, LayoutRect] =
    val paneCount        = paneIds.size
    val maxVisiblePanes  = math.max(1, editorRect.height / MinimumVerticalPaneHeight)
    val visiblePaneCount = math.min(paneCount, maxVisiblePanes)
    val focusedPaneId    = focusedPane(state)
    val (visibleStartIndex, _) = calculateVisiblePaneWindow(
      paneIds,
      focusedPaneId,
      visiblePaneCount
    )
    val visibleHeights = splitLengths(editorRect.height, visiblePaneCount)
    val visibleOffsets = visibleHeights.scanLeft(editorRect.y)(_ + _).dropRight(1)

    paneIds.zipWithIndex.map {
      case (paneId, globalIndex) =>
        val visibleIndex = globalIndex - visibleStartIndex
        val isVisible    = visibleIndex >= 0 && visibleIndex < visiblePaneCount
        val paneRect =
          if isVisible then
            LayoutRect(
              x = editorRect.x,
              y = visibleOffsets(visibleIndex),
              width = editorRect.width,
              height = visibleHeights(visibleIndex)
            )
          else
            val offScreenY =
              if visibleIndex < 0 then editorRect.y - 100
              else editorRect.y + editorRect.height + 100

            LayoutRect(
              x = editorRect.x,
              y = offScreenY,
              width = editorRect.width,
              height = editorRect.height
            )

        paneId -> paneRect
    }.toMap

  private def focusedPane(state: AppState): Option[PaneId] =
    state.persisted.focus match
      case Focus.EditorPane(paneId) if state.persisted.layout.editorPanes.contains(paneId) => Some(paneId)
      case _                                                                               => None

  private def splitLengths(total: Int, count: Int): List[Int] =
    val base      = total / count
    val remainder = total % count
    (0 until count).toList.map(index => base + (if index < remainder then 1 else 0))

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
