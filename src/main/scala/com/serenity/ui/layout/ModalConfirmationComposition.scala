package com.serenity.ui.layout

import com.serenity.state.models.*

/** Composition plans for the "N choices, pick one" confirmation dialogs: close-confirmation and the external-change
  * reload-conflict prompt (#1623). Split out of `ModalSurfaceComposition.scala` purely to keep both files under this
  * repo's architecture-ratchet file-length limit -- `ModalSurfaceComposition` still dispatches to this object from
  * `forModal`/`frameHeight`, and forwards `close`/`closeFrameHeight`/`closeChoice`/`reloadConflictFrameHeight`/
  * `reloadConflictChoice` unchanged for its existing external callers, so no behavior or public API changed.
  */
private[layout] object ModalConfirmationComposition:

  private val actions: List[(CloseWorkflowChoice, String, SurfaceActionId, SurfaceFocusId)] = List(
    (CloseWorkflowChoice.Save, "Save", SurfaceActionId("close-save"), SurfaceFocusId("close-save")),
    (
      CloseWorkflowChoice.Discard,
      "Close Anyway",
      SurfaceActionId("close-discard"),
      SurfaceFocusId("close-discard")
    ),
    (CloseWorkflowChoice.Cancel, "Cancel", SurfaceActionId("close-cancel"), SurfaceFocusId("close-cancel"))
  )

  /** Resolve close-confirmation paint, focus, and hit geometry in the shared layout grid. */
  def close(
    workflow: CloseWorkflowState,
    frameRect: LayoutRect,
    targetRows: Int
  ): ResolvedSurfaceComposition =
    val content               = SurfaceFrameLayout(frameRect).contentRect
    val actionRows            = math.max(1, targetRows)
    val bounds                = ModalSurfaceComposition.logicalRect(content.x, content.y, content.width, content.height)
    val preferredActionHeight = actions.length * actionRows
    val usePreferredLayout    = content.height >= 2 + preferredActionHeight
    val useHorizontalActions  = !usePreferredLayout && content.height >= 1 && content.width >= actions.length
    val textValues =
      if usePreferredLayout then List("unsaved changes", workflow.currentBufferLabel)
      else if useHorizontalActions then
        List("unsaved changes", workflow.currentBufferLabel).takeRight(math.max(0, content.height - 1))
      else Nil
    val textBoxes = textValues.zipWithIndex.map { (text, index) =>
      SurfacePaintBox(
        SurfacePaintKind.Text,
        ModalSurfaceComposition.logicalRect(content.x, content.y + index, content.width, 1),
        text = Some(text)
      )
    }
    val actionBoxes =
      if useHorizontalActions then horizontalActionBoxes(workflow, content)
      else
        val actionStartY = content.y + textValues.length
        actions.zipWithIndex.map {
          case ((choice, label, actionId, focusId), index) =>
            actionBox(
              workflow,
              choice,
              label,
              actionId,
              focusId,
              ModalSurfaceComposition.logicalRect(content.x, actionStartY + index * actionRows, content.width, actionRows)
            )
        }
    val clippedTextBoxes   = textBoxes.flatMap(ModalSurfaceComposition.clipBox(_, bounds))
    val clippedActionBoxes = actionBoxes.flatMap(ModalSurfaceComposition.clipBox(_, bounds))
    val hitRegions = clippedActionBoxes.flatMap { box =>
      for
        focusId       <- box.focusId
        actionId      <- box.actionId
        semanticLabel <- box.semanticLabel
      yield SurfaceHitRegion(box.rect, focusId, Some(actionId), semanticLabel)
    }

    ResolvedSurfaceComposition(
      bounds = bounds,
      intrinsicSize = SurfaceIntrinsicSize(
        // The two literal strings prepended below guarantee this list is always non-empty.
        width = ("unsaved changes" :: workflow.currentBufferLabel :: actions.map(_._2))
          .map(_.length.toDouble)
          .foldLeft(Double.MinValue)(_ max _),
        height = 2 + actions.length * actionRows
      ),
      paintBoxes = clippedTextBoxes ++ clippedActionBoxes,
      hitRegions = hitRegions,
      focusOrder = clippedActionBoxes.flatMap(_.focusId)
    )

  /** Frame height required by the close-confirmation composition. */
  def closeFrameHeight(targetRows: Int): Int =
    SurfaceFrameLayout.DefaultBorderCells * 2 + 2 + actions.length * math.max(1, targetRows)

  /** Translate a declared close action identity into its reducer choice. */
  def closeChoice(actionId: SurfaceActionId): Option[CloseWorkflowChoice] =
    actions.collectFirst { case (choice, _, `actionId`, _) => choice }

  private val reloadConflictActions: List[(ReloadConflictChoice, String, SurfaceActionId, SurfaceFocusId)] = List(
    (
      ReloadConflictChoice.Reload,
      "Reload from disk",
      SurfaceActionId("reload-conflict-reload"),
      SurfaceFocusId("reload-conflict-reload")
    ),
    (
      ReloadConflictChoice.Overwrite,
      "Overwrite",
      SurfaceActionId("reload-conflict-overwrite"),
      SurfaceFocusId("reload-conflict-overwrite")
    ),
    (
      ReloadConflictChoice.Cancel,
      "Cancel",
      SurfaceActionId("reload-conflict-cancel"),
      SurfaceFocusId("reload-conflict-cancel")
    )
  )

  /** Resolve the external-change-conflict prompt's (#1623) paint, focus, and hit geometry: a header line, the
    * buffer's label, then one action row per choice -- structurally the close-confirmation layout's simpler cousin,
    * since a reload conflict never needs the horizontal-actions fallback (its label text is short and fixed).
    */
  def reloadConflict(
    workflow: ReloadConflictState,
    frameRect: LayoutRect,
    targetRows: Int
  ): ResolvedSurfaceComposition =
    val content    = SurfaceFrameLayout(frameRect).contentRect
    val actionRows = math.max(1, targetRows)
    val bounds     = ModalSurfaceComposition.logicalRect(content.x, content.y, content.width, content.height)
    val textBoxes = List("file changed on disk", workflow.bufferLabel).zipWithIndex.map { (text, index) =>
      ModalSurfaceComposition.textBox(text, ModalSurfaceComposition.rowRect(bounds, index))
    }
    val actionStartY = content.y + textBoxes.length
    val actionBoxes = reloadConflictActions.zipWithIndex.map {
      case ((choice, label, actionId, focusId), index) =>
        ModalSurfaceComposition.actionBox(
          label,
          actionId,
          focusId,
          selected = choice == workflow.selectedChoice,
          ModalSurfaceComposition.logicalRect(content.x, actionStartY + index * actionRows, content.width, actionRows)
        )
    }
    val clippedTextBoxes   = textBoxes.flatMap(ModalSurfaceComposition.clipBox(_, bounds))
    val clippedActionBoxes = actionBoxes.flatMap(ModalSurfaceComposition.clipBox(_, bounds))
    val hitRegions = clippedActionBoxes.flatMap { box =>
      for
        focusId       <- box.focusId
        actionId      <- box.actionId
        semanticLabel <- box.semanticLabel
      yield SurfaceHitRegion(box.rect, focusId, Some(actionId), semanticLabel)
    }

    ResolvedSurfaceComposition(
      bounds = bounds,
      intrinsicSize = SurfaceIntrinsicSize(
        width = ("file changed on disk" :: workflow.bufferLabel :: reloadConflictActions.map(_._2))
          .map(_.length.toDouble)
          .foldLeft(Double.MinValue)(_ max _),
        height = 2 + reloadConflictActions.length * actionRows
      ),
      paintBoxes = clippedTextBoxes ++ clippedActionBoxes,
      hitRegions = hitRegions,
      focusOrder = clippedActionBoxes.flatMap(_.focusId)
    )

  /** Frame height required by the external-change-conflict composition. */
  def reloadConflictFrameHeight(targetRows: Int): Int =
    SurfaceFrameLayout.DefaultBorderCells * 2 + 2 + reloadConflictActions.length * math.max(1, targetRows)

  /** Translate a declared reload-conflict action identity into its reducer choice. */
  def reloadConflictChoice(actionId: SurfaceActionId): Option[ReloadConflictChoice] =
    reloadConflictActions.collectFirst { case (choice, _, `actionId`, _) => choice }

  private def horizontalActionBoxes(
    workflow: CloseWorkflowState,
    content: LayoutRect
  ): List[SurfacePaintBox] =
    val baseWidth  = content.width / actions.length
    val extraCells = content.width % actions.length
    actions
      .foldLeft((content.x, List.empty[SurfacePaintBox])) {
        case ((nextX, boxes), (choice, label, actionId, focusId)) =>
          val index = boxes.length
          val width = baseWidth + (if index < extraCells then 1 else 0)
          val box = actionBox(
            workflow,
            choice,
            label,
            actionId,
            focusId,
            ModalSurfaceComposition.logicalRect(nextX, content.bottom - 1, width, 1)
          )
          (nextX + width, boxes :+ box)
      }
      ._2

  private def actionBox(
    workflow: CloseWorkflowState,
    choice: CloseWorkflowChoice,
    label: String,
    actionId: SurfaceActionId,
    focusId: SurfaceFocusId,
    rect: LogicalPixelRect
  ): SurfacePaintBox =
    SurfacePaintBox(
      kind = SurfacePaintKind.ActionItem,
      rect = rect,
      text = Some(label),
      focusId = Some(focusId),
      actionId = Some(actionId),
      semanticLabel = Some(label),
      selected = workflow.selectedChoice == choice
    )
