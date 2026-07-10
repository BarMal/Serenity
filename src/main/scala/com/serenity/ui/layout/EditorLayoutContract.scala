package com.serenity.ui.layout

import com.serenity.config.InterfaceDensityMetrics
import com.serenity.state.models.{AppState, PaneId, SurfaceId}
import com.serenity.ui.renderer.{OverlayViewModel, PinnedPanelViewModel}

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

  def panelRect(surfaceId: SurfaceId): Option[LayoutRect] =
    pinnedSurfaceRects.get(surfaceId).orElse(expandedSurfaceRects.get(surfaceId))

  def panelTitleRect(surfaceId: SurfaceId): Option[LayoutRect] =
    pinnedSurfaceTitleRects.get(surfaceId).orElse(expandedSurfaceTitleRects.get(surfaceId))

  def panelContentRect(surfaceId: SurfaceId): Option[LayoutRect] =
    pinnedSurfaceContentRects.get(surfaceId).orElse(expandedSurfaceContentRects.get(surfaceId))

  def panelRowSlots(surfaceId: SurfaceId): List[SurfaceContentRowSlot] =
    pinnedSurfaceRowSlots.get(surfaceId).orElse(expandedSurfaceRowSlots.get(surfaceId)).getOrElse(Nil)

  def overlayRect(surfaceId: SurfaceId): Option[LayoutRect] =
    floatingOverlayRects.collectFirst { case (`surfaceId`, rect) => rect }

  def overlayContentRect(surfaceId: SurfaceId): Option[LayoutRect] =
    floatingOverlayContentRects.collectFirst { case (`surfaceId`, rect) => rect }

  def overlayRowSlots(surfaceId: SurfaceId): List[SurfaceContentRowSlot] =
    floatingOverlayRowSlots.getOrElse(surfaceId, Nil)

  /** Return all currently detectable contract violations. */
  def violations: List[LayoutContractViolation] =
    workspaceViolations ++
      activePaneViolations ++
      pinnedPanelViolations ++
      pinnedSurfaceViolations ++
      expandedSurfaceViolations ++
      pinnedSurfaceRowSlotViolations ++
      expandedSurfaceRowSlotViolations ++
      floatingOverlayViolations ++
      floatingOverlayRowSlotViolations ++
      floatingOverlayStackViolations ++
      gutterViolations

  private def workspaceViolations: List[LayoutContractViolation] =
    containedBy(
      "content area",
      contentAreaRect,
      List(
        "editor panel" -> Some(workspace.editorPanelRect),
        "line numbers" -> workspace.lineNumberRect
      )
    )

  private def activePaneViolations: List[LayoutContractViolation] =
    activePaneId.flatMap(workspace.paneLayouts.get).toList.flatMap { pane =>
      containedBy(
        "editor panel",
        workspace.editorPanelRect,
        List("active pane" -> Some(pane.paneRect))
      ) ++
        containedBy(
          "active pane",
          pane.paneRect,
          List(
            "active content"       -> Some(pane.contentRect),
            "active top spacer"    -> Some(pane.topSpacerRect),
            "active bottom spacer" -> Some(pane.bottomSpacerRect)
          )
        ) ++
        containedBy(
          "content area",
          contentAreaRect,
          List("active header" -> Some(pane.headerRect))
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

object EditorLayoutContract:

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
      math.max(0, state.config.uiElementGap)
    )
    val panelViews = PinnedPanelViewModel.fromState(state, calculatedLayout)
    val pinnedSurfaceContentRects = calculatedLayout.pinnedSurfaceRects.toList.flatMap {
      case (surfaceId, frameRect) =>
        state
          .surfaceById(surfaceId)
          .map(surface => surfaceId -> SurfaceFrameLayout.forContent(frameRect, surface.content).contentRect)
    }.toMap
    val pinnedSurfaceTitleRects = panelViews
      .filter(view => view.surfaceId.exists(calculatedLayout.pinnedSurfaceRects.contains))
      .flatMap(view => view.surfaceId.map(_ -> view.titleRect))
      .toMap
    val pinnedSurfaceRowSlots = panelViews
      .filter(view => view.surfaceId.exists(calculatedLayout.pinnedSurfaceRects.contains))
      .flatMap(view => view.surfaceId.map(_ -> view.contentRowSlots))
      .toMap
    val expandedSurfaceRects = state.expandedPanelSurface.toList.flatMap { surface =>
      calculatedLayout.expandedPanelRect.map(rect => surface.id -> rect)
    }.toMap
    val expandedSurfaceTitleRects = panelViews
      .filter(view => view.surfaceId.exists(expandedSurfaceRects.contains))
      .flatMap(view => view.surfaceId.map(_ -> view.titleRect))
      .toMap
    val expandedSurfaceContentRects = panelViews
      .filter(view => view.surfaceId.exists(expandedSurfaceRects.contains))
      .flatMap(view => view.surfaceId.map(_ -> view.resolvedContentRect))
      .toMap
    val expandedSurfaceRowSlots = panelViews
      .filter(view => view.surfaceId.exists(expandedSurfaceRects.contains))
      .flatMap(view => view.surfaceId.map(_ -> view.contentRowSlots))
      .toMap
    val aboveCursorOverlayRects = calculatedLayout.aboveCursorOverlayStack
    val belowCursorOverlayRects = calculatedLayout.belowCursorOverlayStack
    val floatingOverlayRects    = aboveCursorOverlayRects ++ belowCursorOverlayRects
    val floatingOverlayContentRects = floatingOverlayRects.flatMap {
      case (surfaceId, frameRect) =>
        state
          .surfaceById(surfaceId)
          .map(surface => surfaceId -> SurfaceFrameLayout.forContent(frameRect, surface.content).contentRect)
    }
    val floatingOverlayRowSlots = (OverlayViewModel.fromState(state, calculatedLayout).aboveCursor.toList ++
      OverlayViewModel.fromState(state, calculatedLayout).belowCursorStack)
      .flatMap(view => view.surfaceId.map(_ -> view.contentRowSlots))
      .toMap
    EditorLayoutContract(
      viewportRect = viewportRect,
      contentAreaRect = contentAreaRect,
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
      floatingOverlayRowSlots = floatingOverlayRowSlots
    )
