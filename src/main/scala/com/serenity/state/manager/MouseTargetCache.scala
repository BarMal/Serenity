package com.serenity.state.manager

import com.serenity.lsp.config.LanguageId
import com.serenity.richtext.RichTextDocument
import com.serenity.rope.Rope
import com.serenity.state.models.*
import com.serenity.ui.fonts.FontLoader
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
    fontConfig: FontConfig,
    showGutter: Boolean,
    showLineNumbers: Boolean,
    wordWrapEnabled: Boolean,
    minimumPaneWidth: Int,
    focusPaneId: Option[PaneId],
    orderedPaneIds: List[PaneId],
    paneBuffers: List[(PaneId, Option[BufferId])],
    paneSnapshotInputs: List[
      (
        PaneId,
        Option[(RopeIdentity, Viewport, TypographyRole, Option[LanguageId], Option[RichTextDocument])]
      )
    ],
    pinnedPanels: List[(SurfaceId, PanelPosition, Int)],
    lineNumberContent: List[(BufferId, RopeIdentity)]
)

private[manager] object MouseTargetLayoutKey:

  def from(state: AppState, viewportSize: ViewportSize): MouseTargetLayoutKey =
    MouseTargetLayoutKey(
      viewportSize = viewportSize,
      fontConfig = state.config.fontConfig,
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
      paneSnapshotInputs = state.layout.orderedPaneIds.map { paneId =>
        paneId -> state.layout.editorPanes
          .get(paneId)
          .flatMap(_.bufferId)
          .flatMap(state.buffers.get)
          .map(buffer =>
            (
              RopeIdentity(buffer.content),
              buffer.viewport,
              buffer.typographyRole,
              buffer.language,
              buffer.richTextDocument
            )
          )
      },
      pinnedPanels = state.uiSurfaces.collect {
        case UiSurface(id, _, SurfacePresentation.Pinned(position, size), _) => (id, position, size)
      },
      lineNumberContent =
        if state.config.showLineNumbers then
          state.buffers.toList.sortBy(_._1.value).map((bufferId, buffer) => bufferId -> RopeIdentity(buffer.content))
        else Nil
    )

private[manager] case class MouseTargetCache(
    layoutKey: MouseTargetLayoutKey,
    scene: UiSceneSnapshot
)

private[manager] object MouseTargetCache:

  def fromState(state: AppState, viewportSize: ViewportSize): MouseTargetCache =
    val layoutKey = MouseTargetLayoutKey.from(state, viewportSize)
    val scene = UiSceneSnapshot.publishedFor(state, viewportSize).getOrElse {
      val layout = LayoutEngine.calculateLayoutWithUI(state, viewportSize)
      val base   = UiSceneSnapshot.from(state, layout, viewportSize)
      val snapshots = base.paneLayouts.flatMap {
        case (paneId, paneLayout) =>
          for
            pane     <- state.layout.editorPanes.get(paneId)
            bufferId <- pane.bufferId
            buffer   <- state.buffers.get(bufferId)
          yield
            val font  = FontLoader.previewFontForRole(state.config.fontConfig, buffer.typographyRole)
            val width = paneLayout.contentRect.width * CellMetrics.fromFont(font).charWidth
            paneId -> TextLayoutSnapshot.fromBuffer(buffer, width, font, wordWrapEnabled = state.config.wordWrapEnabled)
      }
      val prepared = base.withTextSnapshots(snapshots)
      UiSceneSnapshot.publish(state, viewportSize, prepared)
      prepared
    }
    MouseTargetCache(layoutKey, scene)
