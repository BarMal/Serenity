package com.serenity.ui.layout

import com.serenity.config.InterfaceDensityMetrics
import com.serenity.state.models.*

/** Named layout ownership violation for editor and surface rectangles. */
case class LayoutContractViolation(
    ownerName: String,
    childName: String,
    ownerRect: LayoutRect,
    childRect: LayoutRect
)

/** Reusable snapshot of the editor layout ownership contract for renderers and tests. */
case class EditorLayoutContract(
    viewportRect: LayoutRect,
    contentAreaRect: LayoutRect,
    leftSpacerRect: LayoutRect,
    rightSpacerRect: LayoutRect,
    topSpacerRect: LayoutRect,
    bottomSpacerRect: LayoutRect,
    workspace: EditorWorkspaceLayout,
    activePaneId: Option[PaneId],
    minimumFloatingOverlayGapRows: Int,
    pinnedPanelRects: Map[PanelPosition, LayoutRect],
    pinnedSurfaceRects: Map[SurfaceId, LayoutRect],
    pinnedSurfaceTitleRects: Map[SurfaceId, LayoutRect],
    pinnedSurfaceContentRects: Map[SurfaceId, LayoutRect],
    pinnedSurfaceRowSlots: Map[SurfaceId, List[SurfaceContentRowSlot]],
    expandedSurfaceRects: Map[SurfaceId, LayoutRect],
    expandedSurfaceTitleRects: Map[SurfaceId, LayoutRect],
    expandedSurfaceContentRects: Map[SurfaceId, LayoutRect],
    expandedSurfaceRowSlots: Map[SurfaceId, List[SurfaceContentRowSlot]],
    aboveCursorOverlayRects: List[(SurfaceId, LayoutRect)],
    belowCursorOverlayRects: List[(SurfaceId, LayoutRect)],
    floatingOverlayRects: List[(SurfaceId, LayoutRect)],
    floatingOverlayContentRects: List[(SurfaceId, LayoutRect)],
    floatingOverlayHeaderRects: Map[SurfaceId, LayoutRect],
    floatingOverlayRowSlots: Map[SurfaceId, List[SurfaceContentRowSlot]]
):

  def paneLayout(paneId: PaneId): Option[EditorPaneLayout] =
    workspace.paneLayouts.get(paneId)

  def paneHeaderRect(paneId: PaneId): Option[LayoutRect] =
    paneLayout(paneId).map(_.headerRect)

  def paneTitleRect(paneId: PaneId): Option[LayoutRect] =
    paneLayout(paneId).map(_.titleRect)

  def activePaneLayout: Option[EditorPaneLayout] =
    activePaneId.flatMap(paneLayout)

  def activePaneHeaderRect: Option[LayoutRect] =
    activePaneLayout.map(_.headerRect)

  def activePaneTitleRect: Option[LayoutRect] =
    activePaneLayout.map(_.titleRect)

  def gutterRect: Option[LayoutRect] =
    workspace.gutterRect

  def lineNumberRect: Option[LayoutRect] =
    workspace.lineNumberRect

  def lineNumberRowSlots(itemCount: Int): List[SurfaceContentRowSlot] =
    workspace.lineNumberRowSlots(itemCount)

  def panelRect(surfaceId: SurfaceId): Option[LayoutRect] =
    expandedSurfaceRects.get(surfaceId).orElse(pinnedSurfaceRects.get(surfaceId))

  def panelTitleRect(surfaceId: SurfaceId): Option[LayoutRect] =
    expandedSurfaceTitleRects.get(surfaceId).orElse(pinnedSurfaceTitleRects.get(surfaceId))

  def panelContentRect(surfaceId: SurfaceId): Option[LayoutRect] =
    expandedSurfaceContentRects.get(surfaceId).orElse(pinnedSurfaceContentRects.get(surfaceId))

  def panelRowSlots(surfaceId: SurfaceId): List[SurfaceContentRowSlot] =
    expandedSurfaceRowSlots.get(surfaceId).orElse(pinnedSurfaceRowSlots.get(surfaceId)).getOrElse(Nil)

  def overlayRect(surfaceId: SurfaceId): Option[LayoutRect] =
    floatingOverlayRects.collectFirst { case (`surfaceId`, rect) => rect }

  def overlayContentRect(surfaceId: SurfaceId): Option[LayoutRect] =
    floatingOverlayContentRects.collectFirst { case (`surfaceId`, rect) => rect }

  def overlayHeaderRect(surfaceId: SurfaceId): Option[LayoutRect] =
    floatingOverlayHeaderRects.get(surfaceId)

  def overlayRowSlots(surfaceId: SurfaceId): List[SurfaceContentRowSlot] =
    floatingOverlayRowSlots.getOrElse(surfaceId, Nil)

  /** Return all currently detectable contract violations. */
  def violations: List[LayoutContractViolation] =
    workspaceViolations ++
      paneViolations ++
      pinnedPanelViolations ++
      pinnedSurfaceViolations ++
      expandedSurfaceViolations ++
      pinnedSurfaceRowSlotViolations ++
      expandedSurfaceRowSlotViolations ++
      floatingOverlayViolations ++
      floatingOverlayHeaderViolations ++
      floatingOverlayRowSlotViolations ++
      floatingOverlayStackViolations ++
      gutterViolations

  private def workspaceViolations: List[LayoutContractViolation] =
    containedBy(
      "content area",
      contentAreaRect,
      List(
        "editor panel"  -> Some(workspace.editorPanelRect),
        "line numbers"  -> workspace.lineNumberRect,
        "left spacer"   -> Some(leftSpacerRect),
        "right spacer"  -> Some(rightSpacerRect),
        "top spacer"    -> Some(topSpacerRect),
        "bottom spacer" -> Some(bottomSpacerRect)
      )
    )

  private def paneViolations: List[LayoutContractViolation] =
    workspace.paneLayouts.toList.flatMap {
      case (paneId, pane) =>
        val paneName    = if activePaneId.contains(paneId) then "active pane" else s"pane ${paneId.value}"
        val contentName = if activePaneId.contains(paneId) then "active content" else s"pane ${paneId.value} content"
        val topSpacerName =
          if activePaneId.contains(paneId) then "active top spacer" else s"pane ${paneId.value} top spacer"
        val bottomSpacerName =
          if activePaneId.contains(paneId) then "active bottom spacer" else s"pane ${paneId.value} bottom spacer"
        val headerName = if activePaneId.contains(paneId) then "active header" else s"pane ${paneId.value} header"
        containedBy(
          "editor panel",
          workspace.editorPanelRect,
          List(paneName -> Some(pane.paneRect))
        ) ++
          containedBy(
            paneName,
            pane.paneRect,
            List(
              contentName      -> Some(pane.contentRect),
              topSpacerName    -> Some(pane.topSpacerRect),
              bottomSpacerName -> Some(pane.bottomSpacerRect)
            )
          ) ++
          containedBy(
            "content area",
            contentAreaRect,
            List(headerName -> Some(pane.headerRect))
          )
    }

  private def pinnedPanelViolations: List[LayoutContractViolation] =
    containedBy(
      "content area",
      contentAreaRect,
      pinnedPanelRects.toList.map((position, rect) => s"pinned panel $position" -> Some(rect))
    )

  private def pinnedSurfaceViolations: List[LayoutContractViolation] =
    containedBy(
      "content area",
      contentAreaRect,
      pinnedSurfaceRects.toList.map((surfaceId, rect) => s"pinned surface ${surfaceId.value} frame" -> Some(rect))
    ) ++
      pinnedSurfaceRects.toList.flatMap {
        case (surfaceId, frameRect) =>
          containedBy(
            s"pinned surface ${surfaceId.value} frame",
            frameRect,
            List(
              s"pinned surface ${surfaceId.value} title"   -> pinnedSurfaceTitleRects.get(surfaceId),
              s"pinned surface ${surfaceId.value} content" -> pinnedSurfaceContentRects.get(surfaceId)
            )
          ) ++
            titleContentOverlapViolations(
              s"pinned surface ${surfaceId.value}",
              pinnedSurfaceTitleRects.get(surfaceId),
              pinnedSurfaceContentRects.get(surfaceId)
            )
      }

  private def expandedSurfaceViolations: List[LayoutContractViolation] =
    containedBy(
      "content area",
      contentAreaRect,
      expandedSurfaceRects.toList.map((surfaceId, rect) => s"expanded surface ${surfaceId.value} frame" -> Some(rect))
    ) ++
      expandedSurfaceRects.toList.flatMap {
        case (surfaceId, frameRect) =>
          containedBy(
            s"expanded surface ${surfaceId.value} frame",
            frameRect,
            List(
              s"expanded surface ${surfaceId.value} title"   -> expandedSurfaceTitleRects.get(surfaceId),
              s"expanded surface ${surfaceId.value} content" -> expandedSurfaceContentRects.get(surfaceId)
            )
          ) ++
            titleContentOverlapViolations(
              s"expanded surface ${surfaceId.value}",
              expandedSurfaceTitleRects.get(surfaceId),
              expandedSurfaceContentRects.get(surfaceId)
            )
      }

  private def floatingOverlayViolations: List[LayoutContractViolation] =
    activePaneId
      .flatMap(workspace.paneLayouts.get)
      .map(_.contentRect)
      .toList
      .flatMap { activeContent =>
        containedBy(
          "active content",
          activeContent,
          floatingOverlayRects.map((surfaceId, rect) => s"floating overlay ${surfaceId.value} frame" -> Some(rect)) ++
            floatingOverlayContentRects
              .map((surfaceId, rect) => s"floating overlay ${surfaceId.value} content" -> Some(rect))
        )
      }

  private def pinnedSurfaceRowSlotViolations: List[LayoutContractViolation] =
    rowSlotViolations("pinned surface", pinnedSurfaceContentRects, pinnedSurfaceRowSlots)

  private def expandedSurfaceRowSlotViolations: List[LayoutContractViolation] =
    rowSlotViolations("expanded surface", expandedSurfaceContentRects, expandedSurfaceRowSlots)

  private def floatingOverlayRowSlotViolations: List[LayoutContractViolation] =
    rowSlotViolations("floating overlay", floatingOverlayContentRects.toMap, floatingOverlayRowSlots)

  private def floatingOverlayHeaderViolations: List[LayoutContractViolation] =
    floatingOverlayHeaderRects.toList.flatMap {
      case (surfaceId, headerRect) =>
        floatingOverlayContentRects.collectFirst { case (`surfaceId`, contentRect) => contentRect }.toList.flatMap {
          contentRect =>
            Option.when(!contentRect.containsRect(headerRect))(
              LayoutContractViolation(
                s"floating overlay ${surfaceId.value} content",
                s"floating overlay ${surfaceId.value} header",
                contentRect,
                headerRect
              )
            )
        }
    }

  private def floatingOverlayStackViolations: List[LayoutContractViolation] =
    belowCursorOverlayRects.sliding(2).toList.flatMap {
      case List((firstId, firstRect), (secondId, secondRect)) =>
        if secondRect.y < firstRect.bottom then
          List(
            LayoutContractViolation(
              s"floating overlay ${firstId.value} frame",
              s"floating overlay ${secondId.value} frame",
              firstRect,
              secondRect
            )
          )
        else if secondRect.y < firstRect.bottom + minimumFloatingOverlayGapRows then
          List(
            LayoutContractViolation(
              s"floating overlay gap after ${firstId.value}",
              s"floating overlay ${secondId.value} frame",
              LayoutRect(firstRect.x, firstRect.bottom, firstRect.width, minimumFloatingOverlayGapRows),
              secondRect
            )
          )
        else Nil
      case _ => Nil
    }

  private def gutterViolations: List[LayoutContractViolation] =
    workspace.gutterRect.toList
      .filterNot { gutter =>
        gutter.x == viewportRect.x &&
        gutter.width == viewportRect.width &&
        gutter.bottom == viewportRect.bottom &&
        viewportRect.containsRect(gutter)
      }
      .map(gutter => LayoutContractViolation("viewport", "gutter", viewportRect, gutter))

  private def containedBy(
    ownerName: String,
    ownerRect: LayoutRect,
    children: List[(String, Option[LayoutRect])]
  ): List[LayoutContractViolation] =
    children.collect {
      case (childName, Some(childRect)) if !ownerRect.containsRect(childRect) =>
        LayoutContractViolation(ownerName, childName, ownerRect, childRect)
    }

  private def rowSlotViolations(
    ownerPrefix: String,
    ownerRects: Map[SurfaceId, LayoutRect],
    rowSlotsBySurface: Map[SurfaceId, List[SurfaceContentRowSlot]]
  ): List[LayoutContractViolation] =
    rowSlotsBySurface.toList.flatMap {
      case (surfaceId, rowSlots) =>
        ownerRects.get(surfaceId).toList.flatMap { ownerRect =>
          rowSlots.collect {
            case rowSlot
                if !ownerRect.containsRect(
                  LayoutRect(ownerRect.x, rowSlot.y, ownerRect.width.max(0), if ownerRect.height > 0 then 1 else 0)
                ) =>
              LayoutContractViolation(
                s"$ownerPrefix ${surfaceId.value} content",
                s"$ownerPrefix ${surfaceId.value} ${rowSlot.kind} row slot",
                ownerRect,
                LayoutRect(ownerRect.x, rowSlot.y, ownerRect.width.max(0), 1)
              )
          }
        }
    }

  private def titleContentOverlapViolations(
    surfaceName: String,
    titleRect: Option[LayoutRect],
    contentRect: Option[LayoutRect]
  ): List[LayoutContractViolation] =
    (titleRect, contentRect) match
      case (Some(title), Some(content)) if rectanglesOverlap(title, content) =>
        List(LayoutContractViolation(s"$surfaceName title", s"$surfaceName content", title, content))
      case _ =>
        Nil

  private def rectanglesOverlap(first: LayoutRect, second: LayoutRect): Boolean =
    first.x < second.right && second.x < first.right && first.y < second.bottom && second.y < first.bottom

object EditorLayoutContract:

  private case class SurfaceGeometry(
      titleRect: LayoutRect,
      contentRect: LayoutRect,
      rowSlots: List[SurfaceContentRowSlot],
      headerRect: Option[LayoutRect]
  )

  def panelRectFor(surface: UiSurface, calculatedLayout: CalculatedLayout): Option[LayoutRect] =
    surface.presentation match
      case SurfacePresentation.Pinned(position, _) =>
        calculatedLayout.pinnedSurfaceRects.get(surface.id).orElse(calculatedLayout.pinnedPanelRects.get(position))
      case SurfacePresentation.Expanded(_, _) =>
        calculatedLayout.expandedPanelRect
      case _ =>
        None

  def overlayRectFor(surfaceId: SurfaceId, calculatedLayout: CalculatedLayout): Option[LayoutRect] =
    calculatedLayout.aboveCursorOverlayStack
      .find(_._1 == surfaceId)
      .map(_._2)
      .orElse(calculatedLayout.belowCursorOverlayStack.find(_._1 == surfaceId).map(_._2))
      .orElse {
        Option
          .when(
            calculatedLayout.aboveCursorOverlayStack.isEmpty && calculatedLayout.belowCursorOverlayStack.isEmpty
          )(
            calculatedLayout.aboveCursorOverlayRect.orElse(calculatedLayout.belowCursorOverlayRect)
          )
          .flatten
      }

  /** Build the reusable editor layout contract from an already calculated layout. */
  def from(
    state: AppState,
    viewportSize: ViewportSize,
    calculatedLayout: CalculatedLayout
  ): EditorLayoutContract =
    val viewportRect = LayoutRect(0, 0, viewportSize.width, viewportSize.height)
    val contentAreaRect = calculatedLayout.gutterRect match
      case Some(gutter) => LayoutRect(0, 0, viewportSize.width, gutter.y)
      case None         => viewportRect
    val workspace = LayoutEngine.calculateEditorWorkspaceLayout(state, calculatedLayout)
    val minimumFloatingOverlayGapRows = math.max(
      InterfaceDensityMetrics.forDensity(state.config.interfaceDensity).overlayGapRows,
      math.ceil(math.max(0.0, state.config.uiElementGap)).toInt
    )
    val panelGeometryById = (state.pinnedSurfaces ++ state.uiSurfaces.filter {
      _.presentation match
        case SurfacePresentation.Expanded(_, _) => true
        case _                                  => false
    }).flatMap { surface =>
      val panelRect =
        if state.expandedPanelSurface.exists(_.id == surface.id) then calculatedLayout.expandedPanelRect
        else panelRectFor(surface, calculatedLayout)
      panelRect.map(rect => surface.id -> pinnedGeometry(surface, rect, state))
    }.toMap
    val maximizedSurfaceIds = state.expandedPanelSurface.toSet.map(_.id)
    val pinnedSurfaceIds    = calculatedLayout.pinnedSurfaceRects.keySet -- maximizedSurfaceIds
    val pinnedSurfaceTitleRects = pinnedSurfaceIds.toList
      .flatMap(surfaceId => panelGeometryById.get(surfaceId).map(geometry => surfaceId -> geometry.titleRect))
      .toMap
    val pinnedSurfaceContentRects = pinnedSurfaceIds.toList
      .flatMap(surfaceId => panelGeometryById.get(surfaceId).map(geometry => surfaceId -> geometry.contentRect))
      .toMap
    val pinnedSurfaceRowSlots = pinnedSurfaceIds.toList
      .flatMap(surfaceId => panelGeometryById.get(surfaceId).map(geometry => surfaceId -> geometry.rowSlots))
      .toMap
    val expandedSurfaceRects = state.expandedPanelSurface.toList.flatMap { surface =>
      calculatedLayout.expandedPanelRect.map(rect => surface.id -> rect)
    }.toMap
    val expandedSurfaceIds = expandedSurfaceRects.keySet.toList
    val expandedSurfaceTitleRects = expandedSurfaceIds
      .flatMap(surfaceId => panelGeometryById.get(surfaceId).map(geometry => surfaceId -> geometry.titleRect))
      .toMap
    val expandedSurfaceContentRects = expandedSurfaceIds
      .flatMap(surfaceId => panelGeometryById.get(surfaceId).map(geometry => surfaceId -> geometry.contentRect))
      .toMap
    val expandedSurfaceRowSlots = expandedSurfaceIds
      .flatMap(surfaceId => panelGeometryById.get(surfaceId).map(geometry => surfaceId -> geometry.rowSlots))
      .toMap
    val aboveCursorOverlayRects = calculatedLayout.aboveCursorOverlayStack
    val belowCursorOverlayRects = calculatedLayout.belowCursorOverlayStack
    val floatingOverlayRects    = aboveCursorOverlayRects ++ belowCursorOverlayRects
    val floatingGeometryById = floatingOverlayRects.flatMap {
      case (surfaceId, frameRect) =>
        state
          .surfaceById(surfaceId)
          .flatMap(surface => floatingGeometry(surface, frameRect, state, calculatedLayout))
          .map(surfaceId -> _)
    }.toMap
    val floatingOverlayContentRects = floatingOverlayRects.flatMap {
      case (surfaceId, _) =>
        floatingGeometryById.get(surfaceId).map(geometry => surfaceId -> geometry.contentRect)
    }
    val floatingOverlayHeaderRects = floatingGeometryById.flatMap {
      case (surfaceId, geometry) => geometry.headerRect.map(surfaceId -> _)
    }
    val floatingOverlayRowSlots = floatingGeometryById
      .map((surfaceId, geometry) => surfaceId -> geometry.rowSlots)
      .toMap
    EditorLayoutContract(
      viewportRect = viewportRect,
      contentAreaRect = contentAreaRect,
      leftSpacerRect = calculatedLayout.leftSpacerRect,
      rightSpacerRect = calculatedLayout.rightSpacerRect,
      topSpacerRect = calculatedLayout.topSpacerRect,
      bottomSpacerRect = calculatedLayout.bottomSpacerRect,
      workspace = workspace,
      activePaneId = state.layout.activeEditorPaneId,
      minimumFloatingOverlayGapRows = minimumFloatingOverlayGapRows,
      pinnedPanelRects = calculatedLayout.pinnedPanelRects,
      pinnedSurfaceRects = calculatedLayout.pinnedSurfaceRects,
      pinnedSurfaceTitleRects = pinnedSurfaceTitleRects,
      pinnedSurfaceContentRects = pinnedSurfaceContentRects,
      pinnedSurfaceRowSlots = pinnedSurfaceRowSlots,
      expandedSurfaceRects = expandedSurfaceRects,
      expandedSurfaceTitleRects = expandedSurfaceTitleRects,
      expandedSurfaceContentRects = expandedSurfaceContentRects,
      expandedSurfaceRowSlots = expandedSurfaceRowSlots,
      aboveCursorOverlayRects = aboveCursorOverlayRects,
      belowCursorOverlayRects = belowCursorOverlayRects,
      floatingOverlayRects = floatingOverlayRects,
      floatingOverlayContentRects = floatingOverlayContentRects,
      floatingOverlayHeaderRects = floatingOverlayHeaderRects,
      floatingOverlayRowSlots = floatingOverlayRowSlots
    )

  private def pinnedGeometry(surface: UiSurface, frameRect: LayoutRect, state: AppState): SurfaceGeometry =
    val resolved = surface.content match
      case SurfaceContent.MarkdownPreview(bufferId, title) =>
        val content = state.buffers.get(bufferId).map(_.content.collect()).getOrElse("")
        SurfaceContentResolver.resolveMarkdownPreview(title, content, frameRect, SurfaceRenderMode.Pinned)
      case SurfaceContent.Outline(symbols, activeLocation) =>
        val resolvedOutline = SurfaceContent.Outline(
          symbols,
          activeLocation.orElse(
            state.activeCursorPosition.flatMap(cursor =>
              com.serenity.document.DocumentNavigation.currentSymbol(symbols, cursor).map(_.location)
            )
          )
        )
        SurfaceContentResolver.resolve(resolvedOutline, frameRect, SurfaceRenderMode.Pinned)
      case SurfaceContent.Comments(symbols, activeLocation) =>
        val resolvedComments = SurfaceContent.Comments(
          symbols,
          activeLocation.orElse(
            state.activeCursorPosition.flatMap(cursor =>
              com.serenity.document.DocumentNavigation.currentSymbol(symbols, cursor).map(_.location)
            )
          )
        )
        SurfaceContentResolver.resolve(resolvedComments, frameRect, SurfaceRenderMode.Pinned)
      case content =>
        SurfaceContentResolver.resolve(content, frameRect, SurfaceRenderMode.Pinned)
    surfaceGeometry(surface.content, frameRect, resolved, itemGapRows = 0.0, itemTargetRows = 1)

  private def floatingGeometry(
    surface: UiSurface,
    frameRect: LayoutRect,
    state: AppState,
    calculatedLayout: CalculatedLayout
  ): Option[SurfaceGeometry] =
    val collapsed = calculatedLayout.collapsedFloatingSurfaceIds.contains(surface.id)
    val geometryFrame = surface.content match
      case SurfaceContent.GhostOverlay(_, cachedRect) => cachedRect
      case _                                          => frameRect
    val resolved =
      if collapsed then collapsedFloatingContent(surface.content)
      else
        surface.content match
          case SurfaceContent.ContextualToolbar(toolbarState) =>
            SurfaceContentResolver.resolveContextualToolbar(
              toolbarState,
              state,
              geometryFrame,
              SurfaceRenderMode.Floating
            )
          case SurfaceContent.GhostOverlay(originalContent, _) =>
            SurfaceContentResolver.resolve(
              originalContent,
              geometryFrame,
              SurfaceRenderMode.Floating,
              itemGapRowsFor(originalContent, state),
              itemTargetRowsFor(originalContent, state)
            )
          case content =>
            SurfaceContentResolver.resolve(
              content,
              geometryFrame,
              SurfaceRenderMode.Floating,
              itemGapRowsFor(content, state),
              itemTargetRowsFor(content, state)
            )
    Option.when(resolved.header.nonEmpty || resolved.rows.nonEmpty || resolved.footer.nonEmpty)(
      surfaceGeometry(
        surface.content,
        geometryFrame,
        resolved,
        itemGapRowsFor(surface.content, state),
        itemTargetRowsFor(surface.content, state)
      )
    )

  private def surfaceGeometry(
    content: SurfaceContent,
    frameRect: LayoutRect,
    resolved: ResolvedSurfaceContent,
    itemGapRows: Double,
    itemTargetRows: Int
  ): SurfaceGeometry =
    val contentRect = SurfaceFrameLayout.forContent(frameRect, content).contentRect
    SurfaceGeometry(
      titleRect = LayoutRect(contentRect.x, frameRect.y, contentRect.width, 1),
      contentRect = contentRect,
      rowSlots = SurfaceFrameLayout.contentRowSlotsFor(
        contentRect,
        resolved.rows.length,
        resolved.header.nonEmpty,
        resolved.footer.nonEmpty,
        itemGapRows,
        itemTargetRows
      ),
      headerRect = Option.when(resolved.header.nonEmpty && contentRect.height > 0)(
        LayoutRect(contentRect.x, contentRect.y, contentRect.width, 1)
      )
    )

  private def collapsedFloatingContent(content: SurfaceContent): ResolvedSurfaceContent =
    content match
      case SurfaceContent.CommandPalette(runner) =>
        val label = runner.selectedItem match
          case Some(group: com.serenity.command.CommandSurfaceItem.GroupItem) => group.label
          case Some(item)                                                     => item.searchText
          case None                                                           => "commands"
        ResolvedSurfaceContent(rows = List(OverlayRow(label)))
      case other =>
        SurfaceContentResolver.resolve(other, LayoutRect(0, 0, 80, 3), SurfaceRenderMode.Floating)

  private def itemGapRowsFor(content: SurfaceContent, state: AppState): Double =
    content match
      case SurfaceContent.CommandPalette(_) | SurfaceContent.CommandPaletteSubmenu(_, _, _) |
          SurfaceContent.ContextMenu(_) =>
        state.config.commandRunnerItemGapRows
      case SurfaceContent.ContextualToolbar(_) =>
        state.config.uiElementGap
      case SurfaceContent.GhostOverlay(originalContent, _) =>
        itemGapRowsFor(originalContent, state)
      case _ => 0.0

  private def itemTargetRowsFor(content: SurfaceContent, state: AppState): Int =
    content match
      case SurfaceContent.GhostOverlay(originalContent, _) => itemTargetRowsFor(originalContent, state)
      case other => SurfaceFrameLayout.itemTargetRowsFor(other, state.config.interfaceDensity)
