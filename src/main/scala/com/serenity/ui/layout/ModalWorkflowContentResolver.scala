package com.serenity.ui.layout

import com.serenity.state.models.*

/** Resolves `SurfaceContent.ModalWorkflow` (find, replace, and the various file workflows) into overlay rows. Split out
  * of `SurfaceContentResolver` to keep that file's dispatcher readable -- see the doc comment there.
  */
private[layout] object ModalWorkflowContentResolver:

  def resolve(
    modal: Modal,
    rect: LayoutRect,
    mode: SurfaceRenderMode
  ): ResolvedSurfaceContent =
    modal match
      case Modal.FileWorkflow(workflow) =>
        resolveFileWorkflow(workflow, mode)
      case Modal.ReplaceWorkflow(workflow) =>
        resolveReplaceWorkflow(workflow, mode)
      case Modal.Find(query, results, currentIndex) =>
        resolveFindWorkflow(query, results, currentIndex, rect, mode)
      case Modal.CloseWorkflow(_) =>
        ResolvedSurfaceContent()
      case _ =>
        ResolvedSurfaceContent(rows = modalLines(modal).map(OverlayRow(_)))

  private def modalLines(modal: Modal): List[String] =
    modal match
      case Modal.GotoLine(input)   => List("goto-line", input)
      case Modal.Find(query, _, _) => List("find", query)
      case Modal.FileWorkflow(workflow) =>
        List(workflow.operationLabel, workflow.filename, workflow.path)
      case Modal.ReplaceWorkflow(workflow) =>
        List("replace", workflow.findText, workflow.replacementText)
      case Modal.CloseWorkflow(workflow) =>
        List("unsaved changes", workflow.currentBufferLabel)
      case Modal.Custom(name, input) => List(name, input)

  private def resolveFindWorkflow(
    query: String,
    results: List[FindResult],
    currentIndex: Int,
    rect: LayoutRect,
    mode: SurfaceRenderMode
  ): ResolvedSurfaceContent =
    val resultSet  = FindResultSet.normalized(query, results, currentIndex)
    val queryLabel = "Find"
    val queryText  = s"$queryLabel $query"
    val queryRow = OverlayRow(
      plainText = queryText,
      selected = true,
      cursorColumn = Some(queryText.length),
      segments = List(
        OverlaySegment(queryLabel),
        OverlaySegment(query, selected = true)
      ),
      layout = OverlayRowLayout.Split
    )

    val safeIndex     = resultSet.currentIndex
    val maxResultRows = math.max(0, rect.height - 3)
    val resultRows = resultSet.visibleResults(maxResultRows).map {
      case (result, index) =>
        OverlayRow(
          plainText = s"${index + 1}. ${result.line + 1}:${result.column + 1}",
          selected = index == safeIndex
        )
    }

    val footer = Option
      .when(resultSet.query.nonEmpty && resultSet.results.isEmpty) {
        OverlayRow("0 matches")
      }
      .orElse(Option.when(resultSet.results.nonEmpty) {
        OverlayRow(resultSet.selectionSummary)
      })

    ResolvedSurfaceContent(
      title = mode.titleFor("find"),
      header = Some(OverlayRow("find")),
      rows = queryRow :: resultRows,
      footer = footer
    )

  private def resolveReplaceWorkflow(
    workflow: ReplaceWorkflowState,
    mode: SurfaceRenderMode
  ): ResolvedSurfaceContent =
    val findRow = OverlayRow(
      plainText = s"Find ${workflow.findText}",
      selected = workflow.activeField == ReplaceWorkflowField.Find,
      cursorColumn =
        Option.when(workflow.activeField == ReplaceWorkflowField.Find)(s"Find ${workflow.findText}".length),
      segments = List(
        OverlaySegment("Find"),
        OverlaySegment(workflow.findText, selected = workflow.activeField == ReplaceWorkflowField.Find)
      ),
      layout = OverlayRowLayout.Split
    )

    val replaceRow = OverlayRow(
      plainText = s"Replace ${workflow.replacementText}",
      selected = workflow.activeField == ReplaceWorkflowField.ReplaceWith,
      cursorColumn = Option.when(workflow.activeField == ReplaceWorkflowField.ReplaceWith)(
        s"Replace ${workflow.replacementText}".length
      ),
      segments = List(
        OverlaySegment("Replace"),
        OverlaySegment(workflow.replacementText, selected = workflow.activeField == ReplaceWorkflowField.ReplaceWith)
      ),
      layout = OverlayRowLayout.Split
    )

    val actionRow = OverlayRow(
      plainText = "Replace Next Replace All",
      segments = List(
        OverlaySegment("Replace Next", selected = workflow.selectedAction == ReplaceWorkflowAction.ReplaceNext),
        OverlaySegment("Replace All", selected = workflow.selectedAction == ReplaceWorkflowAction.ReplaceAll)
      ),
      layout = OverlayRowLayout.Distributed
    )

    val scopeRow = OverlayRow(
      plainText = "Current Buffer Selection",
      segments = List(
        OverlaySegment("Current Buffer", selected = workflow.selectedScope == ReplaceWorkflowScope.CurrentBuffer),
        OverlaySegment("Selection", selected = workflow.selectedScope == ReplaceWorkflowScope.Selection)
      ),
      layout = OverlayRowLayout.Distributed
    )

    ResolvedSurfaceContent(
      title = mode.titleFor("replace"),
      header = Some(OverlayRow("replace")),
      rows = List(findRow, replaceRow, actionRow, scopeRow),
      footer = workflow.statusMessage.map(OverlayRow(_))
    )

  private def resolveFileWorkflow(
    workflow: FileWorkflowState,
    mode: SurfaceRenderMode
  ): ResolvedSurfaceContent =
    val operationLabel = workflow.operationLabel

    val filenameRow = OverlayRow(
      plainText = s"Filename ${workflow.filename}",
      selected = workflow.activeField == FileWorkflowField.Filename,
      segments = List(
        OverlaySegment("Filename"),
        OverlaySegment(workflow.filename, selected = workflow.activeField == FileWorkflowField.Filename)
      ),
      layout = OverlayRowLayout.Split
    )

    val pathSegments =
      if workflow.path.isEmpty then List(OverlaySegment(""))
      else
        workflow.path
          .split("[/\\\\]")
          .toList
          .filter(_.nonEmpty)
          .map { segment =>
            val isMissing = workflow.missingPathSegments.contains(segment)
            OverlaySegment(
              text = segment,
              selected = workflow.activeField == FileWorkflowField.Path && !isMissing,
              tone = if isMissing then OverlayTone.Error else OverlayTone.Normal
            )
          }

    val pathRow = OverlayRow(
      plainText = s"Path ${workflow.path}",
      selected = workflow.activeField == FileWorkflowField.Path,
      segments = OverlaySegment("Path") :: pathSegments,
      layout = OverlayRowLayout.Split
    )

    val suggestionRows = workflow.suggestions.zipWithIndex.map {
      case (suggestion, index) =>
        val suffix = if suggestion.isDirectory then "/" else ""
        OverlayRow(
          plainText = suggestion.value + suffix,
          selected = index == workflow.selectedSuggestionIndex
        )
    }

    val footer =
      workflow.statusMessage
        .map(OverlayRow(_))
        .orElse(
          Option.when(workflow.confirmCreateDirectories && workflow.missingPathSegments.nonEmpty) {
            OverlayRow(s"Create directories: ${workflow.missingPathSegments.mkString(" / ")}")
          }
        )

    ResolvedSurfaceContent(
      title = mode.titleFor(operationLabel),
      header = Some(OverlayRow(operationLabel)),
      rows = filenameRow :: pathRow :: suggestionRows,
      footer = footer
    )
