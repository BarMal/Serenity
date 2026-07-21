package com.serenity.state.manager

import com.serenity.rope.Rope
import com.serenity.state.models.*
import com.serenity.ui.fonts.FontLoader.FontConfig
import com.serenity.ui.layout.*

final private[manager] case class RopeIdentity private (value: Rope):

  override def equals(obj: Any): Boolean =
    obj match
      case that: RopeIdentity => value.asInstanceOf[AnyRef] eq that.value.asInstanceOf[AnyRef]
      case _                  => false

  override def hashCode(): Int =
    System.identityHashCode(value.asInstanceOf[AnyRef])

private[manager] object RopeIdentity:
  def apply(value: Rope): RopeIdentity =
    new RopeIdentity(value)

private[manager] case class MouseTargetLayoutKey(
    viewportSize: ViewportSize,
    showGutter: Boolean,
    showLineNumbers: Boolean,
    wordWrapEnabled: Boolean,
    minimumPaneWidth: Int,
    focusPaneId: Option[PaneId],
    orderedPaneIds: List[PaneId],
    paneBuffers: List[(PaneId, Option[BufferId])],
    pinnedPanels: List[(SurfaceId, PanelPosition, Int)],
    lineNumberContent: List[(BufferId, RopeIdentity)]
)

private[manager] object MouseTargetLayoutKey:

  def from(state: AppState, viewportSize: ViewportSize): MouseTargetLayoutKey =
    MouseTargetLayoutKey(
      viewportSize = viewportSize,
      showGutter = state.config.showGutter,
      showLineNumbers = state.config.showLineNumbers,
      wordWrapEnabled = state.config.wordWrapEnabled,
      minimumPaneWidth = state.config.minimumPaneWidth,
      focusPaneId = state.focus match
        case Focus.EditorPane(paneId) if state.layout.editorPanes.contains(paneId) => Some(paneId)
        case _                                                                     => None,
      orderedPaneIds = state.layout.orderedPaneIds,
      paneBuffers =
        state.layout.orderedPaneIds.map(paneId => paneId -> state.layout.editorPanes.get(paneId).flatMap(_.bufferId)),
      pinnedPanels = state.uiSurfaces.collect {
        case UiSurface(id, _, SurfacePresentation.Pinned(position, size), _) => (id, position, size)
      },
      lineNumberContent =
        if state.config.showLineNumbers then
          state.buffers.toList.sortBy(_._1.value).map((bufferId, buffer) => bufferId -> RopeIdentity(buffer.content))
        else Nil
    )

private[manager] case class MouseTargetSnapshotKey(
    bufferId: BufferId,
    content: RopeIdentity,
    viewport: Viewport,
    language: Option[com.serenity.lsp.config.LanguageId],
    fontConfig: FontConfig,
    panelWidthPx: Int,
    wordWrapEnabled: Boolean
)

private[manager] object MouseTargetSnapshotKey:

  def from(
    buffer: Buffer,
    fontConfig: FontConfig,
    panelWidthPx: Int,
    wordWrapEnabled: Boolean = true
  ): MouseTargetSnapshotKey =
    MouseTargetSnapshotKey(
      bufferId = buffer.id,
      content = RopeIdentity(buffer.content),
      viewport = buffer.viewport,
      language = buffer.language,
      fontConfig = fontConfig,
      panelWidthPx = panelWidthPx,
      wordWrapEnabled = wordWrapEnabled
    )

private[manager] case class MouseTargetCache(
    layoutKey: MouseTargetLayoutKey,
    layout: CalculatedLayout,
    scene: UiSceneSnapshot,
    paneLayouts: Map[PaneId, EditorPaneLayout],
    snapshots: Map[MouseTargetSnapshotKey, TextLayoutSnapshot] = Map.empty
)

private[manager] object MouseTargetCache:

  def fromState(state: AppState, viewportSize: ViewportSize): MouseTargetCache =
    val layoutKey   = MouseTargetLayoutKey.from(state, viewportSize)
    val layout      = LayoutEngine.calculateLayoutWithUI(state, viewportSize)
    val scene       = UiSceneSnapshot.from(state, layout)
    MouseTargetCache(layoutKey, layout, scene, scene.paneLayouts)
