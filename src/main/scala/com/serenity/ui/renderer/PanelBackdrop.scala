package com.serenity.ui.renderer

import com.serenity.state.models.SurfaceId
import com.serenity.ui.layout.{LayoutRect, SceneNodeId, UiSceneSnapshot}

/** What a docked panel paints over: decided from the scene's structure rather than by sampling pixels. */
object PanelBackdrop:

  /** Whether everything behind `rect` is the frame's freshly cleared theme background, so blurring it is a no-op. A
    * docked panel owns its workspace slot, so no other workspace node (pane, pane header, another panel), spacer column
    * or status gutter -- everything painted before panels -- overlaps it; an expanded panel laid over the editor does.
    */
  def isClearedBackground(scene: UiSceneSnapshot, surfaceId: SurfaceId, rect: LayoutRect): Boolean =
    !paintedBefore(scene, surfaceId).exists(overlaps(rect, _))

  private def paintedBefore(scene: UiSceneSnapshot, surfaceId: SurfaceId): List[LayoutRect] =
    val otherNodes = scene.workspace.filterNot(_.id == SceneNodeId.Surface(surfaceId)).map(_.frameRect)
    val contract   = scene.editorContract
    otherNodes ++ List(contract.leftSpacerRect, contract.rightSpacerRect) ++ contract.gutterRect.toList

  private[renderer] def overlaps(a: LayoutRect, b: LayoutRect): Boolean =
    a.x < b.right && b.x < a.right && a.y < b.bottom && b.y < a.bottom
