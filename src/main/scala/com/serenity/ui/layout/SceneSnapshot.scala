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
final case class SceneHitRegion(kind: SceneHitKind, rect: LayoutRect)

/** One e-reader column of a pane's page (issue #1338, Phase 2 / slice 1): its own [[TextLayoutSnapshot]] (a
  * `visibleLines`-row chunk of the buffer, wrapped at the column's own narrower width) plus where to paint it -- its
  * `columnIndex` on the page (column 0 is the page's leftmost), its x-origin in cells relative to the pane's content
  * rect (`contentRect.x + xOffsetCells`), and its own width in cells (so the renderer can clip the column to its own
  * band and never paint into its neighbour). The renderer composes the page by painting each column's snapshot at its
  * own x-origin; a single-column pane is just `columnIndex = 0`, `xOffsetCells = 0`.
  */
final case class ColumnSnapshotPlacement(
    columnIndex: Int,
    xOffsetCells: Int,
    columnWidthCells: Int,
    snapshot: TextLayoutSnapshot
)

/** Geometry for one visible pane or surface in a rendered UI frame. */
final case class SceneNode(
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
final case class UiSceneSnapshot(
    calculatedLayout: CalculatedLayout,
    paneLayouts: Map[PaneId, EditorPaneLayout],
    editorContract: EditorLayoutContract,
    textSnapshots: Map[PaneId, TextLayoutSnapshot],
    // Multi-column e-reader layout (issue #1338, Phase 2 / slice 1): the full ordered list of per-column snapshots the
    // renderer paints side by side. Empty for a pane not in column mode; for a column-mode pane `textSnapshots` still
    // carries the single (active-column) snapshot so every non-column consumer -- gutter, mouse targeting, damage
    // planning -- keeps working unchanged in slice 1.
    columnSnapshots: Map[PaneId, Vector[ColumnSnapshotPlacement]],
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

  def textSnapshot(paneId: PaneId): Option[TextLayoutSnapshot] =
    textSnapshots.get(paneId)

  /** The pane's ordered per-column snapshots, or empty when the pane is not painting in multi-column mode -- the
    * renderer's entry point for painting an e-reader page.
    */
  def columnSnapshotsFor(paneId: PaneId): Vector[ColumnSnapshotPlacement] =
    columnSnapshots.getOrElse(paneId, Vector.empty)

  def withTextSnapshots(snapshots: Map[PaneId, TextLayoutSnapshot]): UiSceneSnapshot =
    copy(textSnapshots = snapshots)

  def withColumnSnapshots(snapshots: Map[PaneId, Vector[ColumnSnapshotPlacement]]): UiSceneSnapshot =
    copy(columnSnapshots = snapshots)

object UiSceneSnapshot:

  def from(state: AppState, viewportSize: ViewportSize): UiSceneSnapshot =
    from(state, LayoutEngine.calculateLayoutWithUI(state, viewportSize), viewportSize)

  def from(state: AppState, calculatedLayout: CalculatedLayout): UiSceneSnapshot =
    from(
      state,
      calculatedLayout,
      ViewportSize(calculatedLayout.editorPanelRect.right, calculatedLayout.editorPanelRect.bottom)
    )

  def from(
    state: AppState,
    calculatedLayout: CalculatedLayout,
    viewportSize: ViewportSize
  ): UiSceneSnapshot =
    val paneLayouts = LayoutEngine.calculateEditorPaneLayouts(state, calculatedLayout)
    val editorContract = EditorLayoutContract.from(
      state,
      viewportSize,
      calculatedLayout
    )
    val workspacePanes = state.persisted.layout.orderedPaneIds.flatMap { paneId =>
      paneLayouts.get(paneId).toList.flatMap { pane =>
        val paneNode = SceneNode(
          id = SceneNodeId.EditorPane(paneId),
          layer = SceneLayer.Workspace,
          frameRect = pane.paneRect,
          contentRect = pane.contentRect,
          hitRegions = List(SceneHitRegion(SceneHitKind.Frame, pane.paneRect)) ++
            Option
              .when(!state.persisted.layout.activeEditorPaneId.contains(paneId))(
                SceneHitRegion(SceneHitKind.Header, pane.headerRect)
              )
              .toList ++
            List(SceneHitRegion(SceneHitKind.Content, pane.contentRect)),
          zIndex = 0
        )
        val activeHeader = Option.when(state.persisted.layout.activeEditorPaneId.contains(paneId))(
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
      editorContract = editorContract,
      textSnapshots = Map.empty,
      columnSnapshots = Map.empty,
      workspace = workspacePanes ++ workspaceSurfaces,
      floating = floating,
      modalBackdrop = modalBackdrop,
      modal = modal,
      focusOrder =
        if modal.nonEmpty then modal.lastOption.toList.map(_.id)
        else
          orderedForFocus(
            state.persisted.focus,
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
      // Only reachable if focus says Modal with no modal actually open (see the `modal.nonEmpty` short-circuit
      // above, which is the normal path whenever a dialog is open) -- ModalBackdrop is never itself a member of
      // `nodes`, so this always falls through to the unfocused ordering below rather than matching anything.
      case Focus.Modal => SceneNodeId.ModalBackdrop
    Option.when(nodeIds.contains(focused))(focused).toList ++ nodeIds.filterNot(_ == focused)

  private def workspaceSurfaceNodes(
    state: AppState,
    calculatedLayout: CalculatedLayout,
    initialZIndex: Int
  ): List[SceneNode] =
    state.pinnedSurfaces.zipWithIndex.flatMap {
      case (surface, offset) =>
        EditorLayoutContract
          .panelRectFor(surface, state, calculatedLayout)
          .map(frame => surfaceNode(surface.id, SceneLayer.Workspace, frame, initialZIndex + offset))
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
            .map(_ => surfaceNode(surfaceId, SceneLayer.Floating, frame, initialZIndex + offset))
      }

  private def modalSurfaceNodes(
    state: AppState,
    calculatedLayout: CalculatedLayout,
    initialZIndex: Int
  ): List[SceneNode] =
    state.runtime.modalStack.zipWithIndex.map { (dialog, offset) =>
      surfaceNode(
        dialog.id,
        SceneLayer.Modal,
        LayoutEngine.calculateModalRect(dialog, state, calculatedLayout),
        initialZIndex + offset
      )
    }

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
