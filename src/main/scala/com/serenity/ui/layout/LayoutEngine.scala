package com.serenity.ui.layout

import com.serenity.config.{AppConfig, InterfaceDensityMetrics, TextAreaInsets}
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
    pinnedSurfaceRects: Map[SurfaceId, LayoutRect] = Map.empty,
    floatingPanelRect: Option[LayoutRect] = None,
    expandedPanelRect: Option[LayoutRect] = None,
    aboveCursorOverlayRect: Option[LayoutRect] = None,
    belowCursorOverlayRect: Option[LayoutRect] = None,
    aboveCursorOverlayStack: List[(SurfaceId, LayoutRect)] = Nil,
    belowCursorOverlayStack: List[(SurfaceId, LayoutRect)] = Nil,
    collapsedFloatingSurfaceIds: Set[SurfaceId] = Set.empty,
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

  // Default spacer width as percentage of terminal width (15% each side = 30% total)
  private[layout] val DefaultSpacerPercentage = 0.15
  private val MinimumVerticalPaneHeight       = 5
  private val EditorPaneHeaderHeight          = 1
  private val CommandSurfaceChromeRows        = 4

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
    val densityMetrics = InterfaceDensityMetrics.forDensity(state.config.interfaceDensity)
    val gutterHeight   = if usesBottomGutter(state) then densityMetrics.gutterHeight else 0
    val contentHeight  = math.max(1, viewportSize.height - gutterHeight)
    val pinnedPanelLayout = calculatePinnedPanelLayout(
      state.pinnedSurfaces,
      viewportSize.width,
      contentHeight
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

    val uiElementGap = state.config.uiElementGap
    val leftGap      = if leftPinnedWidth > 0 then uiElementGap else 0
    val rightGap     = if rightPinnedWidth > 0 then uiElementGap else 0
    val topGap       = if topPinnedHeight > 0 then uiElementGap else 0
    val bottomGap    = if bottomPinnedHeight > 0 then uiElementGap else 0

    val workspaceX = leftPinnedWidth + leftGap
    val workspaceY = topPinnedHeight + topGap
    val workspaceWidth =
      math.max(1, viewportSize.width - leftPinnedWidth - rightPinnedWidth - leftGap - rightGap)
    val workspaceHeight =
      math.max(1, contentHeight - topPinnedHeight - bottomPinnedHeight - topGap - bottomGap)

    val textAreaInsets =
      if spacerPercentage == DefaultSpacerPercentage then
        val configuredInsets = state.config.textAreaInsets.normalized
        if configuredInsets == TextAreaInsets() then
          TextAreaInsets(
            densityAwareSpacerPercentage(spacerPercentage, densityMetrics),
            densityAwareSpacerPercentage(spacerPercentage, densityMetrics)
          ).normalized
        else configuredInsets
      else TextAreaInsets(spacerPercentage, spacerPercentage).normalized
    val leftSpacerWidth  = (workspaceWidth * textAreaInsets.left).toInt
    val rightSpacerWidth = (workspaceWidth * textAreaInsets.right).toInt

    // Calculate space needed for UI elements
    val lineNumberWidth =
      if state.config.showLineNumbers then calculateLineNumberWidth(state)
      else 0

    // Adjust editor area to accommodate UI elements
    val availableWidth  = math.max(1, workspaceWidth - leftSpacerWidth - rightSpacerWidth - lineNumberWidth)
    val availableHeight = workspaceHeight

    val leftSpacerRect = LayoutRect(workspaceX, workspaceY, leftSpacerWidth, workspaceHeight)
    val lineNumberRect =
      if state.config.showLineNumbers then
        val topInset = EditorPaneHeaderHeight.min(math.max(0, availableHeight - 1))
        Some(
          LayoutRect(workspaceX + leftSpacerWidth, workspaceY + topInset, lineNumberWidth, availableHeight - topInset)
        )
      else None

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
        workspaceHeight
      )

    val gutterRect =
      if usesBottomGutter(state) then
        Some(LayoutRect(0, viewportSize.height - gutterHeight, viewportSize.width, gutterHeight))
      else None

    val baseLayout = CalculatedLayout(
      editorPanelRect = editorPanelRect,
      leftSpacerRect = leftSpacerRect,
      rightSpacerRect = rightSpacerRect,
      pinnedPanelRects = pinnedPanelRects,
      pinnedSurfaceRects = pinnedPanelLayout.surfaceRects,
      expandedPanelRect = state.expandedPanelSurface.map(_ => editorPanelRect),
      lineNumberRect = lineNumberRect,
      gutterRect = gutterRect
    )

    val paneLayouts = calculatePaneLayouts(state, baseLayout)

    val aboveSurfaces = state.floatingSurfaces.filter {
      _.presentation match
        case SurfacePresentation.Floating(_, SurfacePlacement.AboveCursor) => true
        case _                                                             => false
    }
    val belowSurfaces = orderedBelowCursorSurfaces(state)
    val aboveCursorOverlayStack =
      aboveSurfaces.flatMap(surface => calculateFloatingSurfaceRect(surface, state, paneLayouts).map(surface.id -> _))
    val belowLayout = calculateBelowCursorOverlayStack(belowSurfaces, state, paneLayouts)

    baseLayout.copy(
      aboveCursorOverlayRect = aboveCursorOverlayStack.headOption.map(_._2),
      belowCursorOverlayRect = belowLayout.stack.headOption.map(_._2),
      aboveCursorOverlayStack = aboveCursorOverlayStack,
      belowCursorOverlayStack = belowLayout.stack,
      collapsedFloatingSurfaceIds = belowLayout.collapsedSurfaceIds
    )

  private def usesBottomGutter(state: AppState): Boolean =
    state.config.showGutter ||
      (state.config.cursorInfoBarMode != com.serenity.config.CursorInfoBarMode.Off &&
        state.config.cursorInfoBarPlacement == com.serenity.config.CursorInfoBarPlacement.PinnedBottom)

  private case class PinnedPanelLayout(
      panelRects: Map[PanelPosition, LayoutRect],
      surfaceRects: Map[SurfaceId, LayoutRect]
  )

  private def calculatePinnedPanelLayout(
    panels: List[UiSurface],
    terminalWidth: Int,
    contentHeight: Int
  ): PinnedPanelLayout =
    val panelsByPosition = panels.foldLeft(Map.empty[PanelPosition, List[(UiSurface, Int)]]) {
      case (acc, surface) =>
        surface.presentation match
          case SurfacePresentation.Pinned(position, size) =>
            acc.updated(position, acc.getOrElse(position, Nil) :+ (surface -> size))
          case _ =>
            acc
    }
    val panelSizes        = panelsByPosition.view.mapValues(_.map(_._2).max).toMap
    val topHeight         = panelSizes.get(PanelPosition.Top).map(size => math.min(size, contentHeight)).getOrElse(0)
    val remainingAfterTop = math.max(1, contentHeight - topHeight)
    val bottomHeight =
      panelSizes.get(PanelPosition.Bottom).map(size => math.min(size, remainingAfterTop)).getOrElse(0)
    val verticalZoneY      = topHeight
    val verticalZoneHeight = math.max(1, contentHeight - topHeight - bottomHeight)

    val leftWidth          = panelSizes.get(PanelPosition.Left).map(size => math.min(size, terminalWidth)).getOrElse(0)
    val remainingAfterLeft = math.max(1, terminalWidth - leftWidth)
    val rightWidth =
      panelSizes.get(PanelPosition.Right).map(size => math.min(size, remainingAfterLeft)).getOrElse(0)

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
      if state.buffers.isEmpty then 10
      else state.buffers.values.map(_.content.lineCount).max

    math.max(3, maxLines.toString.length + 1) // +1 for spacing, minimum 3 chars

  private def calculateFloatingSurfaceRect(
    surface: UiSurface,
    state: AppState,
    paneLayouts: Map[PaneId, LayoutRect],
    topYOverride: Option[Int] = None,
    forcedHeight: Option[Int] = None
  ): Option[LayoutRect] =
    for
      paneId   <- state.layout.activeEditorPaneId
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
      val preferredHeight = calculateFloatingSurfaceHeight(surface.content, contentRect.height, state)
      val overlayX = math.max(
        contentRect.x,
        math.min(screenPosition.x - (preferredWidth / 2), contentRect.right - preferredWidth)
      )
      val overlayY = topYOverride.getOrElse(surface.presentation match
        case SurfacePresentation.Floating(_, SurfacePlacement.AboveCursor) =>
          math.max(contentRect.y, screenPosition.y - preferredHeight)
        case SurfacePresentation.Floating(_, SurfacePlacement.BelowCursor) =>
          val preferredBelowY = screenPosition.y + 1
          if preferredBelowY + preferredHeight <= contentRect.bottom then preferredBelowY
          else math.max(contentRect.y, screenPosition.y - preferredHeight)
        case _ =>
          contentRect.y)
      val finalHeight = forcedHeight.getOrElse(preferredHeight)

      LayoutRect(
        x = overlayX,
        y = overlayY,
        width = preferredWidth,
        height = finalHeight
      )

  private case class BelowOverlayLayout(
      stack: List[(SurfaceId, LayoutRect)],
      collapsedSurfaceIds: Set[SurfaceId]
  )

  private def orderedBelowCursorSurfaces(state: AppState): List[UiSurface] =
    val maybeRunner  = state.commandRunnerSurface.toList
    val maybeSubmenu = state.commandRunnerSubmenuSurface.toList
    if maybeRunner.nonEmpty && maybeSubmenu.nonEmpty then maybeRunner ++ maybeSubmenu
    else
      val belowSurfaces = state.floatingSurfaces.filter {
        _.presentation match
          case SurfacePresentation.Floating(_, SurfacePlacement.BelowCursor) => true
          case _                                                             => false
      }
      state.focus match
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

  private def calculateBelowCursorOverlayStack(
    surfaces: List[UiSurface],
    state: AppState,
    paneLayouts: Map[PaneId, LayoutRect]
  ): BelowOverlayLayout =
    if surfaces.isEmpty then BelowOverlayLayout(Nil, Set.empty)
    else if surfaces.length == 1 then
      BelowOverlayLayout(
        surfaces.flatMap(surface => calculateFloatingSurfaceRect(surface, state, paneLayouts).map(surface.id -> _)),
        Set.empty
      )
    else
      surfaces match
        case main :: submenu :: _ =>
          val mainRectOpt        = calculateFloatingSurfaceRect(main, state, paneLayouts)
          val submenuBaseRectOpt = calculateFloatingSurfaceRect(submenu, state, paneLayouts)
          (mainRectOpt, submenuBaseRectOpt) match
            case (Some(mainRect), Some(submenuRect)) =>
              val collapsedHeight = 3
              val gapRows         = InterfaceDensityMetrics.forDensity(state.config.interfaceDensity).overlayGapRows
              val availableBottom = state.layout.activeEditorPaneId
                .flatMap(paneLayouts.get)
                .map(CursorLayout.contentRectForPane)
                .map(_.bottom)
                .getOrElse(mainRect.bottom + submenuRect.height + gapRows)
              val totalHeight           = mainRect.height + gapRows + submenuRect.height
              val shouldCollapse        = mainRect.y + totalHeight > availableBottom
              val adjustedMainHeight    = if shouldCollapse then collapsedHeight else mainRect.height
              val adjustedMainRect      = mainRect.copy(height = adjustedMainHeight)
              val remainingHeight       = math.max(3, availableBottom - adjustedMainRect.bottom - gapRows)
              val adjustedSubmenuHeight = math.min(submenuRect.height, remainingHeight)
              val adjustedSubmenuRect = submenuRect.copy(
                y = adjustedMainRect.bottom + gapRows,
                height = adjustedSubmenuHeight
              )
              BelowOverlayLayout(
                List(main.id -> adjustedMainRect, submenu.id -> adjustedSubmenuRect),
                if shouldCollapse then Set(main.id) else Set.empty
              )
            case _ =>
              BelowOverlayLayout(Nil, Set.empty)
        case _ =>
          BelowOverlayLayout(Nil, Set.empty)

  private def calculateFloatingSurfaceWidth(content: SurfaceContent, maxWidth: Int): Int =
    maxWidth

  private def calculateFloatingSurfaceHeight(content: SurfaceContent, maxHeight: Int, state: AppState): Int =
    val densityMetrics = InterfaceDensityMetrics.forDensity(state.config.interfaceDensity)
    val commandMaxHeight =
      state.config.commandRunnerVisibleRows
        .map(rows => AppConfig.clampCommandRunnerVisibleRows(rows) + CommandSurfaceChromeRows)
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
      case SurfaceContent.CommandPalette(_) =>
        math.min(
          commandMaxHeight,
          math.max(densityMetrics.commandSurfaceMinHeight, maxHeight - 1)
        )
      case SurfaceContent.ThemePicker(_) | SurfaceContent.FileSearch(_) =>
        math.min(
          densityMetrics.commandSurfaceMaxHeight,
          math.max(densityMetrics.commandSurfaceMinHeight, maxHeight - 1)
        )
      case SurfaceContent.ContextMenu(menu) =>
        math.min(
          densityMetrics.commandSurfaceMaxHeight,
          math.max(4, menu.items.length + densityMetrics.commandSurfaceVerticalPadding)
        )
      case SurfaceContent.CommentLens(lens) =>
        math.max(4, math.min(8, lens.draft.split("\n", -1).length + 3))
      case SurfaceContent.CommandPaletteSubmenu(runner, groupId, _) =>
        val allItems = runner.submenuItems(groupId)
        val itemCount = runner.activeSubmenu
          .filter(_.groupId == groupId)
          .map(_.filteredItems(allItems).size)
          .getOrElse(allItems.size)
        math.min(
          commandMaxHeight,
          math.max(densityMetrics.commandSurfaceMinHeight, itemCount + densityMetrics.commandSurfaceVerticalPadding)
        )
      case SurfaceContent.ModalWorkflow(modal) =>
        modal match
          case Modal.FileWorkflow(workflow) =>
            math.max(8, math.min(12, workflow.suggestions.take(4).size + 6))
          case Modal.ReplaceWorkflow(workflow) =>
            if workflow.statusMessage.nonEmpty then 8 else 7
          case Modal.Find(_, results, _) =>
            if results.nonEmpty then 5 else 4
          case Modal.CloseWorkflow(_) => 4
          case Modal.Custom(_, _)     => 4
          case _                      => 3
      case SurfaceContent.Terminal(_, _) | SurfaceContent.Outline(_, _) | SurfaceContent.Diagnostics(_) |
          SurfaceContent.MarkdownPreview(_, _) =>
        math.min(8, math.max(4, maxHeight - 1))
      case SurfaceContent.GhostOverlay(_, cachedRect) =>
        cachedRect.height

    math.max(3, math.min(maxHeight, preferredHeight))

  private def densityAwareSpacerPercentage(
    spacerPercentage: Double,
    densityMetrics: InterfaceDensityMetrics
  ): Double =
    if spacerPercentage == DefaultSpacerPercentage then densityMetrics.editorSpacerPercentage
    else spacerPercentage

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

    viewport.copy(topLine = clampedScrollLine, topVisualLine = 0)

  def updateViewportDimensions(viewport: Viewport, panelRect: LayoutRect): Viewport =
    viewport.copy(
      visibleLines = panelRect.height,
      visibleColumns = panelRect.width,
      topVisualLine = viewport.topVisualLine.min(math.max(0, panelRect.height - 1))
    )

  def updateViewportDimensions(
    viewport: Viewport,
    panelRect: LayoutRect,
    viewportSizing: com.serenity.config.ViewportSizing
  ): Viewport =
    val normalizedSizing = viewportSizing.normalized
    val visibleLines     = normalizedSizing.height.resolve(panelRect.height)
    viewport.copy(
      visibleLines = visibleLines,
      visibleColumns = normalizedSizing.width.resolve(panelRect.width),
      topVisualLine = viewport.topVisualLine.min(math.max(0, visibleLines - 1))
    )

  def updateViewportDimensions(viewport: Viewport, panelRect: LayoutRect, metrics: CellMetrics): Viewport =
    viewport.copy(
      visibleLines = panelRect.height / metrics.lineHeight,
      visibleColumns = panelRect.width / metrics.charWidth,
      topVisualLine = viewport.topVisualLine.min(math.max(0, panelRect.height / metrics.lineHeight - 1))
    )

  def syncViewportDimensions(state: AppState, viewportSize: ViewportSize): AppState =
    val calculatedLayout = calculateLayout(state, viewportSize)
    val paneLayouts      = calculatePaneLayouts(state, calculatedLayout)
    val (updatedBuffers, updatedPanes) =
      state.layout.editorPanes.foldLeft((state.buffers, state.layout.editorPanes)) {
        case ((buffers, panes), (paneId, pane)) =>
          val paneRect     = paneLayouts.getOrElse(paneId, calculatedLayout.editorPanelRect)
          val paneViewport = updateViewportDimensions(pane.viewport, paneRect, state.config.viewportSizing)
          val nextPanes    = panes + (paneId -> pane.copy(viewport = paneViewport))
          val updatedBuffer = pane.bufferId.flatMap(buffers.get).map { buffer =>
            buffer.id -> buffer
              .copy(viewport = updateViewportDimensions(buffer.viewport, paneRect, state.config.viewportSizing))
          }
          val nextBuffers = updatedBuffer.fold(buffers)(buffers + _)

          (nextBuffers, nextPanes)
      }

    state.copy(
      buffers = updatedBuffers,
      layout = state.layout.copy(editorPanes = updatedPanes)
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
      state.layout.splitDirection match
        case PaneSplitDirection.Horizontal =>
          calculateHorizontalPaneLayouts(state, editorRect, paneIds, minWidth)
        case PaneSplitDirection.Vertical =>
          calculateVerticalPaneLayouts(state, editorRect, paneIds)

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
    state.focus match
      case Focus.EditorPane(paneId) if state.layout.editorPanes.contains(paneId) => Some(paneId)
      case _                                                                     => None

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
