package com.serenity.ui.layout

import com.serenity.state.models.{CloseWorkflowChoice, CloseWorkflowState}

/** Declarative composition plans for blocking workflow surfaces. */
object ModalSurfaceComposition:

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
    val content    = SurfaceFrameLayout(frameRect).contentRect
    val actionRows = math.max(1, targetRows)
    val textBoxes = List(
      SurfacePaintBox(
        SurfacePaintKind.Text,
        logicalRect(content.x, content.y, content.width, 1),
        text = Some("unsaved changes")
      ),
      SurfacePaintBox(
        SurfacePaintKind.Text,
        logicalRect(content.x, content.y + 1, content.width, 1),
        text = Some(workflow.currentBufferLabel)
      )
    )
    val actionBoxes = actions.zipWithIndex.map {
      case ((choice, label, actionId, focusId), index) =>
        SurfacePaintBox(
          kind = SurfacePaintKind.ActionItem,
          rect = logicalRect(content.x, content.y + 2 + index * actionRows, content.width, actionRows),
          text = Some(label),
          focusId = Some(focusId),
          actionId = Some(actionId),
          semanticLabel = Some(label),
          selected = workflow.selectedChoice == choice
        )
    }
    val hitRegions = actionBoxes.flatMap { box =>
      for
        focusId       <- box.focusId
        actionId      <- box.actionId
        semanticLabel <- box.semanticLabel
      yield SurfaceHitRegion(box.rect, focusId, Some(actionId), semanticLabel)
    }

    ResolvedSurfaceComposition(
      bounds = logicalRect(content.x, content.y, content.width, content.height),
      intrinsicSize = SurfaceIntrinsicSize(
        width = ("unsaved changes" :: workflow.currentBufferLabel :: actions.map(_._2)).map(_.length.toDouble).max,
        height = 2 + actions.length * actionRows
      ),
      paintBoxes = textBoxes ++ actionBoxes,
      hitRegions = hitRegions,
      focusOrder = actions.map(_._4)
    )

  /** Frame height required by the close-confirmation composition. */
  def closeFrameHeight(targetRows: Int): Int =
    SurfaceFrameLayout.DefaultBorderCells * 2 + 2 + actions.length * math.max(1, targetRows)

  /** Translate a declared close action identity into its reducer choice. */
  def closeChoice(actionId: SurfaceActionId): Option[CloseWorkflowChoice] =
    actions.collectFirst { case (choice, _, `actionId`, _) => choice }

  private def logicalRect(x: Int, y: Int, width: Int, height: Int): LogicalPixelRect =
    LogicalPixelRect(x.toDouble, y.toDouble, width.toDouble, height.toDouble)
