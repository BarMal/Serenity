package com.serenity.ui.layout

import com.serenity.state.models.*

/** The paint order for one frame of Serenity's user interface. */
enum SceneLayer:
  case Workspace
  case Floating
  case ModalBackdrop
  case Modal

/** Stable identity for a scene node without introducing renderer state into the layout model. */
enum SceneNodeId:
  case EditorPane(paneId: PaneId)
  case EditorPaneHeader(paneId: PaneId)
  case Surface(surfaceId: SurfaceId)
  case ModalBackdrop

/** The interactive role of a rectangle owned by a scene node. */
enum SceneHitKind:
  case Frame
  case Header
  case Content

/** A named interactive rectangle whose owner is the enclosing [[SceneNode]]. */
case class SceneHitRegion(kind: SceneHitKind, rect: LayoutRect)

/** Geometry for one visible pane or surface in a rendered UI frame. */
case class SceneNode(
    id: SceneNodeId,
    layer: SceneLayer,
    frameRect: LayoutRect,
    contentRect: LayoutRect,
    hitRegions: List[SceneHitRegion],
    zIndex: Int
)

/** Pure, authoritative UI geometry for a frame.
  *
  * `calculatedLayout` remains available temporarily for callers that have not yet migrated away from the legacy layout
  * API.
  */
case class UiSceneSnapshot(
    calculatedLayout: CalculatedLayout,
    paneLayouts: Map[PaneId, EditorPaneLayout],
    workspace: List[SceneNode],
    floating: List[SceneNode],
    modalBackdrop: Option[SceneNode],
    modal: List[SceneNode],
    focusOrder: List[SceneNodeId]
):

  def nodesInPaintOrder: List[SceneNode] =
    workspace ++ floating ++ modalBackdrop.toList ++ modal

  def floatingRect(surfaceId: SurfaceId): Option[LayoutRect] =
    floating.collectFirst { case SceneNode(SceneNodeId.Surface(`surfaceId`), _, frame, _, _, _) => frame }

object UiSceneSnapshot:

  def from(state: AppState, viewportSize: ViewportSize): UiSceneSnapshot =
    from(state, LayoutEngine.calculateLayoutWithUI(state, viewportSize))

  def from(state: AppState, calculatedLayout: CalculatedLayout): UiSceneSnapshot =
    val paneLayouts = LayoutEngine.calculateEditorPaneLayouts(state, calculatedLayout)
    val workspacePanes = state.layout.orderedPaneIds.flatMap { paneId =>
      paneLayouts.get(paneId).toList.flatMap { pane =>
        val paneNode = SceneNode(
          id = SceneNodeId.EditorPane(paneId),
          layer = SceneLayer.Workspace,
          frameRect = pane.paneRect,
          contentRect = pane.contentRect,
          hitRegions = List(SceneHitRegion(SceneHitKind.Frame, pane.paneRect)) ++
            Option
              .when(!state.layout.activeEditorPaneId.contains(paneId))(
                SceneHitRegion(SceneHitKind.Header, pane.headerRect)
              )
              .toList ++
            List(SceneHitRegion(SceneHitKind.Content, pane.contentRect)),
          zIndex = 0
        )
        val activeHeader = Option.when(state.layout.activeEditorPaneId.contains(paneId))(
          SceneNode(
            id = SceneNodeId.EditorPaneHeader(paneId),
            layer = SceneLayer.Workspace,
            frameRect = pane.headerRect,
            contentRect = pane.headerRect,
            hitRegions = List(
              SceneHitRegion(SceneHitKind.Frame, pane.headerRect),
              SceneHitRegion(SceneHitKind.Header, pane.headerRect)
            ),
            zIndex = 0
          )
        )
        paneNode :: activeHeader.toList
      }
    }
    val workspaceSurfaces = workspaceSurfaceNodes(state, calculatedLayout, workspacePanes.size)
    val floating          = floatingSurfaceNodes(state, calculatedLayout, workspacePanes.size + workspaceSurfaces.size)
    val modal = modalSurfaceNodes(state, calculatedLayout, workspacePanes.size + workspaceSurfaces.size + floating.size)
    val modalBackdrop = Option.when(modal.nonEmpty)(
      SceneNode(
        id = SceneNodeId.ModalBackdrop,
        layer = SceneLayer.ModalBackdrop,
        frameRect = calculatedLayout.editorPanelRect,
        contentRect = calculatedLayout.editorPanelRect,
        hitRegions = List(SceneHitRegion(SceneHitKind.Frame, calculatedLayout.editorPanelRect)),
        zIndex = workspacePanes.size + workspaceSurfaces.size + floating.size
      )
    )
    val nodes = workspacePanes ++ workspaceSurfaces ++ floating ++ modal
    UiSceneSnapshot(
      calculatedLayout = calculatedLayout,
      paneLayouts = paneLayouts,
      workspace = workspacePanes ++ workspaceSurfaces,
      floating = floating,
      modalBackdrop = modalBackdrop,
      modal = modal,
      focusOrder = orderedForFocus(
        state.focus,
        nodes.filterNot(_.id match
          case SceneNodeId.EditorPaneHeader(_) => true
          case _                               => false)
      )
    )

  private def orderedForFocus(focus: Focus, nodes: List[SceneNode]): List[SceneNodeId] =
    val nodeIds = nodes.map(_.id)
    val focused = focus match
      case Focus.EditorPane(paneId) => SceneNodeId.EditorPane(paneId)
      case Focus.Surface(surfaceId) => SceneNodeId.Surface(surfaceId)
    Option.when(nodeIds.contains(focused))(focused).toList ++ nodeIds.filterNot(_ == focused)

  private def workspaceSurfaceNodes(
    state: AppState,
    calculatedLayout: CalculatedLayout,
    initialZIndex: Int
  ): List[SceneNode] =
    val pinned = state.pinnedSurfaces
    val expanded = state.uiSurfaces.filter {
      _.presentation match
        case SurfacePresentation.Expanded(_, _) => true
        case _                                  => false
    }
    (pinned ++ expanded).zipWithIndex.flatMap {
      case (surface, offset) =>
        panelRect(surface, calculatedLayout).map { frame =>
          surfaceNode(surface.id, SceneLayer.Workspace, frame, initialZIndex + offset)
        }
    }

  private def floatingSurfaceNodes(
    state: AppState,
    calculatedLayout: CalculatedLayout,
    initialZIndex: Int
  ): List[SceneNode] =
    (calculatedLayout.aboveCursorOverlayStack ++ calculatedLayout.belowCursorOverlayStack).zipWithIndex
      .flatMap {
        case ((surfaceId, frame), offset) =>
          state
            .surfaceById(surfaceId)
            .filterNot(surface => state.blockingModalSurfaces.exists(_.id == surface.id))
            .map(_ => surfaceNode(surfaceId, SceneLayer.Floating, frame, initialZIndex + offset))
      }

  private def modalSurfaceNodes(
    state: AppState,
    calculatedLayout: CalculatedLayout,
    initialZIndex: Int
  ): List[SceneNode] =
    state.blockingModalSurfaces.zipWithIndex.map { (surface, offset) =>
      surfaceNode(
        surface.id,
        SceneLayer.Modal,
        LayoutEngine.calculateModalRect(surface, state, calculatedLayout),
        initialZIndex + offset
      )
    }

  private def panelRect(surface: UiSurface, calculatedLayout: CalculatedLayout): Option[LayoutRect] =
    surface.presentation match
      case SurfacePresentation.Pinned(position, _) =>
        calculatedLayout.pinnedSurfaceRects.get(surface.id).orElse(calculatedLayout.pinnedPanelRects.get(position))
      case SurfacePresentation.Expanded(_, _) =>
        calculatedLayout.expandedPanelRect
      case _ => None

  private def surfaceNode(surfaceId: SurfaceId, layer: SceneLayer, frame: LayoutRect, zIndex: Int): SceneNode =
    val content = SurfaceFrameLayout(frame).contentRect
    SceneNode(
      id = SceneNodeId.Surface(surfaceId),
      layer = layer,
      frameRect = frame,
      contentRect = content,
      hitRegions = List(
        SceneHitRegion(SceneHitKind.Frame, frame),
        SceneHitRegion(SceneHitKind.Content, content)
      ),
      zIndex = zIndex
    )
