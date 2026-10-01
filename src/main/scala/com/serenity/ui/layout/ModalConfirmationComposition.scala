package com.serenity.ui.layout

import com.serenity.state.models.*

/** The close-confirmation dialog's composition. Split out of `ModalSurfaceComposition.scala` to keep both files under
  * this repo's architecture-ratchet file-length limit; `ModalSurfaceComposition` dispatches to it and forwards
  * `close`/`closeFrameHeight`/`closeChoice` for external callers. Other confirmations use [[ConfirmComposition]].
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
              ModalSurfaceComposition.logicalRect(
                content.x,
                actionStartY + index * actionRows,
                content.width,
                actionRows
              )
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
