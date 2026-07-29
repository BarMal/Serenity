package com.serenity.ui.layout

import com.serenity.state.models.*

/** Declarative composition plans for blocking workflow surfaces. */
object ModalSurfaceComposition:

  /** Resolve any modal workflow into one paint, hit-testing, and focus plan. */
  def forModal(
    modal: Modal,
    frameRect: LayoutRect,
    targetRows: Int
  ): Option[ResolvedSurfaceComposition] =
    modal match
      case Modal.CloseWorkflow(workflow) => Some(close(workflow, frameRect, targetRows))
      case Modal.GotoLine(input)         => Some(inputPlan("Go to line", input, "goto-line", frameRect))
      case Modal.Find(query, results, currentIndex) =>
        Some(findPlan(query, results, currentIndex, frameRect))
      case Modal.Custom(name, input)       => Some(inputPlan(name, input, "custom-input", frameRect))
      case Modal.FileWorkflow(workflow)    => Some(filePlan(workflow, frameRect))
      case Modal.ReplaceWorkflow(workflow) => Some(replacePlan(workflow, frameRect, targetRows))

  /** Return the minimum frame height needed to show a modal workflow at the requested density. */
  def frameHeight(modal: Modal, targetRows: Int): Int =
    val actionRows = math.max(1, targetRows)
    modal match
      case Modal.GotoLine(_)               => 3
      case Modal.Find(_, Nil, _)           => 5
      case Modal.Custom(_, _)              => 4
      case Modal.Find(_, _, _)             => 6
      case Modal.ReplaceWorkflow(workflow) =>
        val contentRows = 3 + actionRows * 2 + workflow.statusMessage.fold(0)(_ => 1)
        SurfaceFrameLayout.DefaultBorderCells * 2 + contentRows
      case Modal.FileWorkflow(workflow)    => math.max(8, math.min(12, workflow.suggestions.take(4).size + 6))
      case Modal.CloseWorkflow(_)          => closeFrameHeight(actionRows)

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
    val bounds                = logicalRect(content.x, content.y, content.width, content.height)
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
        logicalRect(content.x, content.y + index, content.width, 1),
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
              logicalRect(content.x, actionStartY + index * actionRows, content.width, actionRows)
            )
        }
    val clippedTextBoxes   = textBoxes.flatMap(clipBox(_, bounds))
    val clippedActionBoxes = actionBoxes.flatMap(clipBox(_, bounds))
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
        width = ("unsaved changes" :: workflow.currentBufferLabel :: actions.map(_._2)).map(_.length.toDouble).max,
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

  private def inputPlan(
    label: String,
    value: String,
    focusId: String,
    frameRect: LayoutRect
  ): ResolvedSurfaceComposition =
    val content = SurfaceFrameLayout(frameRect).contentRect
    val bounds  = logicalRect(content.x, content.y, content.width, content.height)
    val row = inputBox(
      label,
      value,
      SurfaceFocusId(focusId),
      bounds.copy(height = math.min(1.0, bounds.height))
    )
    plan(bounds, List(row))

  private def findPlan(
    query: String,
    results: List[FindResult],
    currentIndex: Int,
    frameRect: LayoutRect
  ): ResolvedSurfaceComposition =
    val content   = SurfaceFrameLayout(frameRect).contentRect
    val bounds    = logicalRect(content.x, content.y, content.width, content.height)
    val resultSet = FindResultSet.normalized(query, results, currentIndex)
    val headerBox = textBox("find", rowRect(bounds, 0))
    val queryBox  = inputBox("Find", query, SurfaceFocusId("find"), rowRect(bounds, 1))
    val resultBoxes = resultSet.visibleResults(math.max(0, content.height - 3)).zipWithIndex.map {
      case ((result, index), offset) =>
        textBox(
          s"${index + 1}. ${result.line + 1}:${result.column + 1}",
          rowRect(bounds, offset + 2),
          selected = index == resultSet.currentIndex,
          focusId = Some(SurfaceFocusId(s"find-result-$index")),
          actionId = Some(SurfaceActionId(s"find-result-$index"))
        )
    }
    val footer = Option.when(resultSet.query.nonEmpty) {
      textBox(
        if resultSet.results.isEmpty then "0 matches" else resultSet.selectionSummary,
        rowRect(bounds, content.height - 1)
      )
    }
    plan(bounds, headerBox :: queryBox :: resultBoxes ++ footer.toList)

  private def replacePlan(
    workflow: ReplaceWorkflowState,
    frameRect: LayoutRect,
    targetRows: Int
  ): ResolvedSurfaceComposition =
    val content    = SurfaceFrameLayout(frameRect).contentRect
    val bounds     = logicalRect(content.x, content.y, content.width, content.height)
    val actionRows = math.max(1, targetRows)
    val fields = List(
      inputBox(
        "Find",
        workflow.findText,
        SurfaceFocusId("find"),
        rowRect(bounds, 0),
        selected = workflow.activeField == ReplaceWorkflowField.Find
      ),
      inputBox(
        "Replace",
        workflow.replacementText,
        SurfaceFocusId("replace"),
        rowRect(bounds, 1),
        selected = workflow.activeField == ReplaceWorkflowField.ReplaceWith
      )
    )
    val actionY = bounds.y + 2
    val actionBoxes = horizontalBoxes(
      bounds.copy(y = actionY, height = actionRows),
      List(
        ("Replace Next", "replace-next", workflow.selectedAction == ReplaceWorkflowAction.ReplaceNext),
        ("Replace All", "replace-all", workflow.selectedAction == ReplaceWorkflowAction.ReplaceAll)
      )
    )
    val scopeY = actionY + actionRows
    val scopeBoxes = horizontalBoxes(
      bounds.copy(y = scopeY, height = actionRows),
      List(
        ("Current Buffer", "current-buffer", workflow.selectedScope == ReplaceWorkflowScope.CurrentBuffer),
        ("Selection", "replace-selection", workflow.selectedScope == ReplaceWorkflowScope.Selection)
      )
    )
    val status = workflow.statusMessage.toList.map(message => textBox(message, rowRect(bounds, 2 + actionRows * 2)))
    plan(bounds, fields ++ actionBoxes ++ scopeBoxes ++ status)

  private def filePlan(
    workflow: FileWorkflowState,
    frameRect: LayoutRect
  ): ResolvedSurfaceComposition =
    val content   = SurfaceFrameLayout(frameRect).contentRect
    val bounds    = logicalRect(content.x, content.y, content.width, content.height)
    val rowHeight = 1
    val header    = textBox(workflow.operationLabel, rowRect(bounds, 0))
    val filename = inputBox(
      "Filename",
      workflow.filename,
      SurfaceFocusId("filename"),
      rowRect(bounds, 1, rowHeight),
      selected = workflow.activeField == FileWorkflowField.Filename,
      cursorAtEnd = false,
      segments = List(
        OverlaySegment("Filename"),
        OverlaySegment(workflow.filename, selected = workflow.activeField == FileWorkflowField.Filename)
      ),
      layout = SurfacePaintLayout.Split
    )
    val pathSegments =
      if workflow.path.isEmpty then List(OverlaySegment(""))
      else
        workflow.path
          .split("(?<=[/\\\\])", -1)
          .toList
          .filter(_.nonEmpty)
          .map { segment =>
            val missing = workflow.missingPathSegments.exists(segment.contains)
            OverlaySegment(
              segment,
              selected = workflow.activeField == FileWorkflowField.Path && !missing,
              tone = if missing then OverlayTone.Error else OverlayTone.Normal
            )
          }
    val path = inputBox(
      "Path",
      workflow.path,
      SurfaceFocusId("path"),
      rowRect(bounds, 2, rowHeight),
      selected = workflow.activeField == FileWorkflowField.Path,
      cursorAtEnd = false,
      segments = OverlaySegment("Path ") :: pathSegments,
      layout = SurfacePaintLayout.Inline
    )
    val suggestions = workflow.suggestions.take(4).zipWithIndex.map {
      case (suggestion, index) =>
        val suffix = if suggestion.isDirectory then "/" else ""
        actionBox(
          suggestion.value + suffix,
          SurfaceActionId(s"file-suggestion-$index"),
          SurfaceFocusId(s"file-suggestion-$index"),
          selected = index == workflow.selectedSuggestionIndex,
          rowRect(bounds, index + 3, rowHeight)
        )
    }
    val footer = workflow.statusMessage
      .orElse(Option.when(workflow.confirmCreateDirectories && workflow.missingPathSegments.nonEmpty) {
        s"Create directories: ${workflow.missingPathSegments.mkString(" / ")}"
      })
      .toList
      .map(message => textBox(message, rowRect(bounds, workflow.suggestions.take(4).size + 3, rowHeight)))
    plan(bounds, header :: filename :: path :: suggestions ++ footer)

  private def plan(bounds: LogicalPixelRect, boxes: List[SurfacePaintBox]): ResolvedSurfaceComposition =
    val clipped = boxes.flatMap(box => box.rect.intersection(bounds).map(rect => box.copy(rect = rect)))
    val hits = clipped.flatMap { box =>
      for
        focusId <- box.focusId
        label   <- box.semanticLabel
      yield SurfaceHitRegion(box.rect, focusId, box.actionId, label)
    }
    ResolvedSurfaceComposition(
      bounds = bounds,
      intrinsicSize = SurfaceIntrinsicSize(bounds.width, bounds.height),
      paintBoxes = clipped,
      hitRegions = hits,
      focusOrder = hits.map(_.focusId)
    )

  private def textBox(
    text: String,
    rect: LogicalPixelRect,
    selected: Boolean = false,
    segments: List[OverlaySegment] = Nil,
    layout: SurfacePaintLayout = SurfacePaintLayout.Plain,
    focusId: Option[SurfaceFocusId] = None,
    actionId: Option[SurfaceActionId] = None
  ): SurfacePaintBox =
    SurfacePaintBox(
      SurfacePaintKind.Text,
      rect,
      text = Some(text),
      focusId = focusId,
      actionId = actionId,
      semanticLabel = Some(text),
      selected = selected,
      segments = segments,
      layout = layout
    )

  private def inputBox(
    label: String,
    value: String,
    focusId: SurfaceFocusId,
    rect: LogicalPixelRect,
    selected: Boolean = true,
    cursorAtEnd: Boolean = true,
    segments: List[OverlaySegment] = Nil,
    layout: SurfacePaintLayout = SurfacePaintLayout.Plain
  ): SurfacePaintBox =
    SurfacePaintBox(
      kind = SurfacePaintKind.TextInput,
      rect = rect,
      text = Some(s"$label $value"),
      focusId = Some(focusId),
      semanticLabel = Some(label),
      selected = selected,
      cursorOffset = Option.when(selected && cursorAtEnd)(label.length + 1 + value.length),
      segments = if segments.nonEmpty then segments else List(OverlaySegment(label), OverlaySegment(value)),
      layout = if segments.nonEmpty then layout else SurfacePaintLayout.Split
    )

  private def actionBox(
    label: String,
    actionId: SurfaceActionId,
    focusId: SurfaceFocusId,
    selected: Boolean,
    rect: LogicalPixelRect
  ): SurfacePaintBox =
    SurfacePaintBox(
      kind = SurfacePaintKind.ActionItem,
      rect = rect,
      text = Some(label),
      focusId = Some(focusId),
      actionId = Some(actionId),
      semanticLabel = Some(label),
      selected = selected
    )

  private def horizontalBoxes(
    rect: LogicalPixelRect,
    items: List[(String, String, Boolean)]
  ): List[SurfacePaintBox] =
    val width = if items.isEmpty then 0.0 else rect.width / items.length
    items.zipWithIndex.map {
      case ((label, id, selected), index) =>
        actionBox(
          label,
          SurfaceActionId(id),
          SurfaceFocusId(id),
          selected,
          LogicalPixelRect(rect.x + index * width, rect.y, width, rect.height)
        )
    }

  private def rowRect(bounds: LogicalPixelRect, row: Int, height: Int = 1): LogicalPixelRect =
    LogicalPixelRect(
      bounds.x,
      bounds.y + row,
      bounds.width,
      math.min(height.toDouble, math.max(0.0, bounds.bottom - bounds.y - row))
    )

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
            logicalRect(nextX, content.bottom - 1, width, 1)
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

  private def clipBox(box: SurfacePaintBox, bounds: LogicalPixelRect): Option[SurfacePaintBox] =
    box.rect.intersection(bounds).map(clipped => box.copy(rect = clipped))

  private def logicalRect(x: Int, y: Int, width: Int, height: Int): LogicalPixelRect =
    LogicalPixelRect(x.toDouble, y.toDouble, width.toDouble, height.toDouble)
