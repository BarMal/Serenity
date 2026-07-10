package com.serenity.ui.layout

import com.serenity.state.models.{AppState, PaneId, SurfaceId}

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
    pinnedPanelRects: Map[PanelPosition, LayoutRect],
    pinnedSurfaceRects: Map[SurfaceId, LayoutRect],
    pinnedSurfaceContentRects: Map[SurfaceId, LayoutRect],
    floatingOverlayRects: List[(SurfaceId, LayoutRect)],
    floatingOverlayContentRects: List[(SurfaceId, LayoutRect)]
):

  /** Return all currently detectable contract violations. */
  def violations: List[LayoutContractViolation] =
    workspaceViolations ++
      activePaneViolations ++
      pinnedPanelViolations ++
      pinnedSurfaceViolations ++
      floatingOverlayViolations ++
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
      pinnedSurfaceRects.toList.map((surfaceId, rect) => s"pinned surface ${surfaceId.value} frame" -> Some(rect)) ++
        pinnedSurfaceContentRects.toList.map((surfaceId, rect) =>
          s"pinned surface ${surfaceId.value} content" -> Some(rect)
        )
    )

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
    val pinnedSurfaceContentRects = calculatedLayout.pinnedSurfaceRects.toList.flatMap {
      case (surfaceId, frameRect) =>
        state
          .surfaceById(surfaceId)
          .map(surface => surfaceId -> SurfaceFrameLayout.forContent(frameRect, surface.content).contentRect)
    }.toMap
    val floatingOverlayRects = calculatedLayout.aboveCursorOverlayStack ++ calculatedLayout.belowCursorOverlayStack
    val floatingOverlayContentRects = floatingOverlayRects.flatMap {
      case (surfaceId, frameRect) =>
        state
          .surfaceById(surfaceId)
          .map(surface => surfaceId -> SurfaceFrameLayout.forContent(frameRect, surface.content).contentRect)
    }
    EditorLayoutContract(
      viewportRect = viewportRect,
      contentAreaRect = contentAreaRect,
      workspace = workspace,
      activePaneId = state.layout.activeEditorPaneId,
      pinnedPanelRects = calculatedLayout.pinnedPanelRects,
      pinnedSurfaceRects = calculatedLayout.pinnedSurfaceRects,
      pinnedSurfaceContentRects = pinnedSurfaceContentRects,
      floatingOverlayRects = floatingOverlayRects,
      floatingOverlayContentRects = floatingOverlayContentRects
    )
