package com.serenity.ui.layout

import com.serenity.config.InterfaceDensityMetrics
import com.serenity.state.models.*

/** Geometry for panels pinned to a screen edge or docked into the workspace tree, plus drag-to-resize handling for
  * them. Split out of `LayoutEngine` (issue tracked by the 600-line architecture ratchet) -- this is the self-contained
  * "where do pinned/docked panels go, and how do they resize" concern; the surrounding editor/floating-surface layout
  * stays in `LayoutEngine` and calls back into here.
  */
object PinnedPanelLayoutEngine:

  private val PinnedPanelDragWorkspaceReach = 1

  final private[layout] case class PinnedPanelLayout(
      panelRects: Map[PanelPosition, LayoutRect],
      surfaceRects: Map[SurfaceId, LayoutRect]
  )

  final private case class PinnedAxisSizes(start: Int, end: Int)

  final case class PinnedPanelDragResize(position: PanelPosition, size: Int)

  private[layout] def calculateDockedPanelLayout(
    tree: WorkspaceTree,
    panels: List[UiSurface],
    workspaceRect: LayoutRect,
    minimumEditorWidth: Int,
    minimumEditorHeight: Int,
    uiElementGap: Int
  ): PinnedPanelLayout =
    val nodeRects =
      calculateWorkspaceNodeRects(tree.root, workspaceRect, minimumEditorWidth, minimumEditorHeight, uiElementGap)
    val surfaceRects = tree.dockedSurfaceIds.flatMap { surfaceId =>
      tree.nodeIdForSurface(surfaceId).flatMap(nodeRects.get).map(surfaceId -> _)
    }.toMap
    val panelRects = panels
      .flatMap { surface =>
        tree.positionForSurface(surface.id).flatMap(position => surfaceRects.get(surface.id).map(position -> _))
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
    minimumEditorWidth: Int,
    minimumEditorHeight: Int,
    uiElementGap: Int
  ): Map[WorkspaceNodeId, LayoutRect] =
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
          val requestedExtent               = splitWorkspaceExtent(total, split.ratio)
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
    val layout        = LayoutEngine.calculateLayoutWithUI(state, viewportSize)
    val contentHeight = calculateContentHeight(state, viewportSize)
    val uiElementGap  = math.ceil(math.max(0.0, state.persisted.config.uiElementGap)).toInt
    // The panel's already-rendered extent at this edge -- the workspace tree's own ratio (issue #817) is the sole
    // size record for a docked panel, so this reads the geometry that ratio just produced rather than any size still
    // carried on a surface.
    val panelSizes = layout.pinnedPanelRects.map {
      case (position, rect) =>
        val extent = position match
          case PanelPosition.Left | PanelPosition.Right => rect.width
          case PanelPosition.Top | PanelPosition.Bottom => rect.height
        position -> extent
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
    val gutterHeight   = if LayoutEngine.usesBottomGutter(state) then densityMetrics.gutterHeight else 0
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
