package com.serenity.ui.layout

import com.serenity.config.{HotkeyTrigger, ModalKeyAction}
import com.serenity.state.models.*

/** Declarative composition plans for blocking workflow surfaces. Every [[ConfirmPrompt]] is composed in
  * `ConfirmComposition` and every [[ListPicker]] in `ListPickerComposition`, split out to keep this file under the
  * architecture-ratchet file-length limit.
  */
object ModalSurfaceComposition:

  /** Resolve any modal workflow into one paint, hit-testing, and focus plan. `modalBindings` sources the file
    * workflow's keybinding-hints footer (issue #1253) from the app's actual, currently-configured `Modal` keymap group
    * -- like `ShortcutsHelpContent` (#1250), never a hardcoded list -- and defaults to `ModalKeyAction.defaultBindings`
    * for callers (mostly tests) that construct a plan without a live `AppConfig`.
    */
  def forModal(
    modal: Modal,
    frameRect: LayoutRect,
    targetRows: Int,
    modalBindings: Map[ModalKeyAction, List[HotkeyTrigger]] = ModalKeyAction.defaultBindings,
    measure: TextMeasure = TextMeasure.Cells
  ): Option[ResolvedSurfaceComposition] =
    modal match
      case Modal.Confirm(prompt)           => Some(ConfirmComposition.forPrompt(prompt, frameRect, targetRows, measure))
      case Modal.TextPrompt(prompt)        => Some(textPromptPlan(prompt, frameRect))
      case find: Modal.Find                => Some(findPlan(find, frameRect, modalBindings, measure))
      case Modal.FileWorkflow(workflow)    => Some(filePlan(workflow, frameRect, modalBindings, measure))
      case Modal.ReplaceWorkflow(workflow) => Some(replacePlan(workflow, frameRect, targetRows, measure))
      case Modal.ListPicker(picker)        => Some(ListPickerComposition.forPicker(picker, frameRect, measure))
      case Modal.PanelArrangement(arrangement) =>
        Some(PanelArrangementComposition.forArrangement(arrangement, frameRect, modalBindings, measure))

  /** [[forModal]] with the density, keymap and text measure the app's own state calls for -- how painting, hit-testing
    * and accessibility all read a modal, so they agree on where its wrapped rows and choices are.
    */
  def forState(modal: Modal, frameRect: LayoutRect, state: AppState): Option[ResolvedSurfaceComposition] =
    forModal(
      modal,
      frameRect,
      SurfaceFrameLayout.minimumTargetRows(state.persisted.config.interfaceDensity),
      state.persisted.config.inputConfig.focusedKeymapConfig.modal.bindings,
      TextMeasure.forState(state)
    )

  /** [[frameHeight]] for a frame `width` cells wide, as the app's own state sizes it. */
  def frameHeightFor(modal: Modal, width: Int, state: AppState): Int =
    frameHeight(
      modal,
      SurfaceFrameLayout.minimumTargetRows(state.persisted.config.interfaceDensity),
      width,
      state.persisted.config.inputConfig.focusedKeymapConfig.modal.bindings,
      TextMeasure.forState(state)
    )

  /** The minimum frame height needed to show a modal workflow at the requested density, in a frame `width` cells wide:
    * prose that wraps there takes the rows it wraps to.
    */
  def frameHeight(
    modal: Modal,
    targetRows: Int,
    width: Int,
    modalBindings: Map[ModalKeyAction, List[HotkeyTrigger]] = ModalKeyAction.defaultBindings,
    measure: TextMeasure = TextMeasure.Cells
  ): Int =
    val actionRows = math.max(1, targetRows)
    val wrap       = ModalWrap.ofFrame(width, measure)
    modal match
      case Modal.TextPrompt(_) => 3
      case find: Modal.Find =>
        (if find.results.isEmpty then 5 else 6) + (findErrorLines(find, wrap).size - 1).max(0)
      case Modal.ListPicker(picker) => ListPickerComposition.frameHeight(picker, wrap)
      case Modal.PanelArrangement(arrangement) =>
        PanelArrangementComposition.frameHeight(arrangement, modalBindings, wrap)
      case Modal.ReplaceWorkflow(workflow) =>
        val contentRows = 3 + actionRows * 2 + workflow.statusMessage.fold(0)(wrap.lines(_).size)
        SurfaceFrameLayout.DefaultBorderCells * 2 + contentRows
      case Modal.FileWorkflow(workflow) =>
        // header + filename + path + format rows, plus up to 4 suggestions, plus a status/create-dir footer and the
        // keybinding-hints footer (issue #1253), which take as many rows as they wrap to.
        val footerRows = math.max(1, fileFooterLines(workflow, wrap).size)
        val hintRows   = wrap.lines(fileWorkflowKeyHints(workflow, modalBindings)).size
        math.max(9, math.min(14, workflow.suggestions.take(4).size + 8)) + (footerRows - 1) + (hintRows - 1)
      case Modal.Confirm(prompt) => ConfirmComposition.frameHeight(prompt, actionRows, wrap)

  private def inputPlan(
    label: String,
    value: String,
    focusId: String,
    frameRect: LayoutRect,
    caret: Int
  ): ResolvedSurfaceComposition =
    val content = SurfaceFrameLayout(frameRect).contentRect
    val bounds  = logicalRect(content.x, content.y, content.width, content.height)
    val row = inputBox(
      label,
      value,
      SurfaceFocusId(focusId),
      bounds.copy(height = math.min(1.0, bounds.height)),
      caret = Some(caret)
    )
    plan(bounds, List(row))

  private def textPromptPlan(prompt: TextPrompt, frameRect: LayoutRect): ResolvedSurfaceComposition =
    val focusId = prompt.purpose match
      case TextPromptPurpose.GotoLine        => "goto-line"
      case TextPromptPurpose.SessionName(_)  => "session-name"
      case TextPromptPurpose.RenameSymbol(_) => "rename-symbol"
    inputPlan(prompt.label, prompt.input, focusId, frameRect, prompt.field.caret)

  private def findPlan(
    find: Modal.Find,
    frameRect: LayoutRect,
    modalBindings: Map[ModalKeyAction, List[HotkeyTrigger]],
    measure: TextMeasure
  ): ResolvedSurfaceComposition =
    val content    = SurfaceFrameLayout(frameRect).contentRect
    val bounds     = logicalRect(content.x, content.y, content.width, content.height)
    val errorRows  = findErrorLines(find, ModalWrap.ofContent(content.width, measure))
    val footerRows = math.max(1, errorRows.size)
    val query      = find.query
    val resultSet  = FindResultSet.normalized(query.text, find.results, find.currentIndex, find.capped)
    val headerBox  = textBox(findHeader(find.options, modalBindings), rowRect(bounds, 0))
    val queryBox   = inputBox("Find", query.text, SurfaceFocusId("find"), rowRect(bounds, 1), caret = Some(query.caret))
    val resultBoxes = resultSet.visibleResults(math.max(0, content.height - 2 - footerRows)).zipWithIndex.map {
      case ((result, index), offset) =>
        textBox(
          s"${index + 1}. ${result.line + 1}:${result.column + 1}",
          rowRect(bounds, offset + 2),
          selected = index == resultSet.currentIndex,
          focusId = Some(SurfaceFocusId(s"find-result-$index")),
          action = Some(SurfaceAction.SelectFindResult(index))
        )
    }
    val footer =
      if errorRows.nonEmpty then
        errorRows.zipWithIndex.map { (line, offset) =>
          textBox(line, rowRect(bounds, content.height - footerRows + offset), tone = OverlayTone.Error)
        }
      else if resultSet.query.nonEmpty then
        List(
          textBox(
            if resultSet.results.isEmpty then "0 matches" else resultSet.selectionSummary,
            rowRect(bounds, content.height - 1)
          )
        )
      else Nil
    plan(bounds, headerBox :: queryBox :: resultBoxes ++ footer)

  private def findErrorLines(find: Modal.Find, wrap: ModalWrap): List[String] =
    Option
      .when(find.query.text.nonEmpty)(FindPattern.compile(find.query.text, find.options))
      .flatMap(_.left.toOption)
      .toList
      .flatMap(error => wrap.lines(s"Invalid regex: ${error.message}"))

  /** "find" followed by each option's state and key, e.g. `find  [x] case alt+c  [ ] word alt+w  [ ] regex alt+r`. */
  private def findHeader(options: FindOptions, modalBindings: Map[ModalKeyAction, List[HotkeyTrigger]]): String =
    val toggles = List(
      ("case", FindOption.MatchCase, ModalKeyAction.ToggleMatchCase),
      ("word", FindOption.WholeWord, ModalKeyAction.ToggleWholeWord),
      ("regex", FindOption.Regex, ModalKeyAction.ToggleRegex)
    ).map {
      case (label, option, action) =>
        val mark = if options.isOn(option) then "[x]" else "[ ]"
        val key  = modalBindings.getOrElse(action, Nil).headOption.fold("")(trigger => s" ${trigger.render}")
        s"$mark $label$key"
    }
    ("find" :: toggles).mkString("  ")

  private def replacePlan(
    workflow: ReplaceWorkflowState,
    frameRect: LayoutRect,
    targetRows: Int,
    measure: TextMeasure
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
    val status = workflow.statusMessage.toList
      .flatMap(ModalWrap.ofContent(content.width, measure).lines)
      .zipWithIndex
      .map((line, offset) => textBox(line, rowRect(bounds, 2 + actionRows * 2 + offset)))
    plan(bounds, fields ++ actionBoxes ++ scopeBoxes ++ status)

  /** Keeps the tail of a path's segments on screen when they do not all fit `maxWidth`, eliding the head behind a
    * leading "..." segment instead. The segments nearest the file -- not an OS-specific temp-directory prefix, which
    * can run far longer on macOS (`/var/folders/.../T/`) than on Linux (`/tmp/`) -- are what tells a user where the
    * file actually lives, so dropping the tail (as plain left-to-right truncation does) hides exactly the part that
    * matters. Width is measured the same way [[OverlaySegmentRowRenderer.renderInlineSegments]] spends it: each
    * segment's text plus one gap cell after it (none after the last), so this never overshoots what actually fits.
    */
  private def visiblePathSegments(
    label: OverlaySegment,
    segments: List[OverlaySegment],
    maxWidth: Int
  ): List[OverlaySegment] =
    def widthOf(segs: List[OverlaySegment]): Int =
      segs.map(_.text.length).sum + math.max(0, segs.size - 1)

    if widthOf(label :: segments) <= maxWidth then label :: segments
    else
      val ellipsis = OverlaySegment("...")
      @annotation.tailrec
      def keepTailThatFits(remaining: List[OverlaySegment], kept: List[OverlaySegment]): List[OverlaySegment] =
        remaining match
          case Nil => kept
          case lastSegment :: earlierSegments =>
            val candidate = lastSegment :: kept
            if widthOf(label :: ellipsis :: candidate) <= maxWidth then keepTailThatFits(earlierSegments, candidate)
            else kept
      label :: ellipsis :: keepTailThatFits(segments.reverse, Nil)

  private def filePlan(
    workflow: FileWorkflowState,
    frameRect: LayoutRect,
    modalBindings: Map[ModalKeyAction, List[HotkeyTrigger]],
    measure: TextMeasure
  ): ResolvedSurfaceComposition =
    val content   = SurfaceFrameLayout(frameRect).contentRect
    val bounds    = logicalRect(content.x, content.y, content.width, content.height)
    val wrap      = ModalWrap.ofContent(content.width, measure)
    val rowHeight = 1
    // A dedicated kind (rather than plain Text) so AccessibilityModel.modalControls can find and announce this
    // non-interactive header directly -- it has no focusId, so it can never turn up among `hitRegions` the way an
    // interactive control does (#1527).
    val header = headingBox(workflow.operationLabel, rowRect(bounds, 0))
    // Open Folder picks a folder, so it has no filename to type: the Path sits directly under the title.
    val hasFilenameRow = workflow.mode != FileWorkflowMode.OpenFolder
    val pathRowIndex   = if hasFilenameRow then 2 else 1
    val filename = Option.when(hasFilenameRow)(
      inputBox(
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
    )
    val pathLabelSegment = OverlaySegment("Path ")
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
      rowRect(bounds, pathRowIndex, rowHeight),
      selected = workflow.activeField == FileWorkflowField.Path,
      cursorAtEnd = false,
      segments = visiblePathSegments(pathLabelSegment, pathSegments, bounds.width.toInt),
      layout = SurfacePaintLayout.Inline
    )
    val formatLabel = workflow.detectedFileType.displayName
    val formatNotes =
      Option.when(workflow.wouldLoseFormatting)("will lose rich formatting").toList ++ workflow.fidelityNote
    val formatValue =
      if formatNotes.isEmpty then formatLabel else s"$formatLabel (${formatNotes.mkString("; ")})"
    // Open has no format to choose, so it renders no Format row at all (#1527); only Save As shows it. Dropping the
    // row (rather than a dead "Format:" label) also lets the suggestion list start one row higher on Open.
    val formatRow = workflow match
      case saveAsWorkflow: SaveAsFileWorkflowState =>
        val formatSelected = saveAsWorkflow.activeField == FileWorkflowField.Format
        List(
          inputBox(
            "Format",
            formatValue,
            SurfaceFocusId("format"),
            rowRect(bounds, 3, rowHeight),
            selected = formatSelected,
            cursorAtEnd = false,
            segments = List(OverlaySegment("Format"), OverlaySegment(formatValue, selected = formatSelected)),
            layout = SurfacePaintLayout.Split
          )
        )
      case _: OpenFileWorkflowState | _: OpenFolderFileWorkflowState => Nil
    val suggestionBaseRow = pathRowIndex + 1 + formatRow.size
    // Render a bounded window that follows the selection rather than a frozen top slice, so navigating past the
    // visible cap keeps the highlighted suggestion on screen (#1526). Action/focus ids stay the *global* suggestion
    // index -- `ModalFileWorkflowReducer` maps `file-suggestion-N` straight back into `workflow.suggestions(N)`.
    val visibleCount = 4
    val total        = workflow.suggestions.length
    val windowStart =
      if total <= visibleCount then 0
      else math.min(math.max(0, workflow.selectedSuggestionIndex - visibleCount / 2), total - visibleCount)
    val suggestions = workflow.suggestions.slice(windowStart, windowStart + visibleCount).zipWithIndex.map {
      case (suggestion, displayIndex) =>
        val globalIndex = windowStart + displayIndex
        val suffix      = if suggestion.isDirectory then "/" else ""
        actionBox(
          suggestion.value + suffix,
          SurfaceActionId(s"file-suggestion-$globalIndex"),
          SurfaceFocusId(s"file-suggestion-$globalIndex"),
          selected = globalIndex == workflow.selectedSuggestionIndex,
          rowRect(bounds, displayIndex + suggestionBaseRow, rowHeight)
        )
    }
    val footerLines = fileFooterLines(workflow, wrap)
    val footerStart = suggestions.size + suggestionBaseRow
    val hintsStart  = footerStart + math.max(1, footerLines.size)
    val footer =
      footerLines.zipWithIndex.map((line, offset) => textBox(line, rowRect(bounds, footerStart + offset, rowHeight)))
    val keyHints = wrap.lines(fileWorkflowKeyHints(workflow, modalBindings)).zipWithIndex.map { (line, offset) =>
      textBox(line, rowRect(bounds, hintsStart + offset, rowHeight))
    }
    plan(bounds, header :: (filename.toList ++ (path :: (formatRow ++ suggestions ++ footer ++ keyHints))))

  private def fileFooterLines(workflow: FileWorkflowState, wrap: ModalWrap): List[String] =
    workflow.statusMessage
      .orElse(Option.when(workflow.confirmCreateDirectories && workflow.missingPathSegments.nonEmpty) {
        s"Create directories: ${workflow.missingPathSegments.mkString(" / ")}"
      })
      .toList
      .flatMap(wrap.lines)

  /** Builds the file workflow's own current-action hint, in the same tone as `commandRunnerShowKeyHints` elsewhere in
    * this file: sourced live from `modalBindings` (the app's actual, currently-configured `Modal` keymap group) rather
    * than hardcoded, so it can never drift from what a user has rebound (issue #1253). "Create dir" only appears while
    * it actually does something -- a save-as with missing directories to create.
    */
  private def fileWorkflowKeyHints(
    workflow: FileWorkflowState,
    modalBindings: Map[ModalKeyAction, List[HotkeyTrigger]]
  ): String =
    val showCreateDirectory =
      workflow.mode == FileWorkflowMode.SaveAs && workflow.missingPathSegments.nonEmpty
    val choosesFolder = workflow.mode == FileWorkflowMode.OpenFolder
    val navigateLabel = workflow match
      case saveAsWorkflow: SaveAsFileWorkflowState if saveAsWorkflow.activeField == FileWorkflowField.Format =>
        "Cycle format"
      case _ if choosesFolder => "Folders"
      case _                  => "Suggestions"
    // In Open Folder, Enter and Tab only move around; the one action that picks the folder says so.
    val actions = List(
      (if choosesFolder then "Browse" else "Submit")        -> ModalKeyAction.Submit,
      "Cancel"                                              -> ModalKeyAction.Dismiss,
      (if choosesFolder then "Descend" else "Switch field") -> ModalKeyAction.NextField,
      navigateLabel                                         -> ModalKeyAction.NavigateDown
    ) ++ Option.when(showCreateDirectory)("Create dir" -> ModalKeyAction.CreateDirectory) ++
      Option.when(workflow.canOpenAsProjectRoot)(
        (if choosesFolder then "Open folder" else "Open as root") -> ModalKeyAction.OpenAsProjectRoot
      )
    actions
      .flatMap {
        case (label, action) =>
          modalBindings.getOrElse(action, Nil).headOption.map(trigger => s"$label ${trigger.render}")
      }
      .mkString("  ")

  private[layout] def plan(bounds: LogicalPixelRect, boxes: List[SurfacePaintBox]): ResolvedSurfaceComposition =
    val clipped = boxes.flatMap(box => box.rect.intersection(bounds).map(rect => box.copy(rect = rect)))
    val hits = clipped.flatMap { box =>
      for
        focusId <- box.focusId
        label   <- box.semanticLabel
      yield SurfaceHitRegion(box.rect, focusId, box.actionId, label, box.action)
    }
    ResolvedSurfaceComposition(
      bounds = bounds,
      intrinsicSize = SurfaceIntrinsicSize(bounds.width, bounds.height),
      paintBoxes = clipped,
      hitRegions = hits,
      focusOrder = hits.map(_.focusId)
    )

  private[layout] def textBox(
    text: String,
    rect: LogicalPixelRect,
    selected: Boolean = false,
    segments: List[OverlaySegment] = Nil,
    layout: SurfacePaintLayout = SurfacePaintLayout.Plain,
    focusId: Option[SurfaceFocusId] = None,
    actionId: Option[SurfaceActionId] = None,
    tone: OverlayTone = OverlayTone.Normal,
    action: Option[SurfaceAction] = None
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
      layout = layout,
      tone = tone,
      action = action
    )

  private def headingBox(text: String, rect: LogicalPixelRect): SurfacePaintBox =
    SurfacePaintBox(SurfacePaintKind.Heading, rect, text = Some(text), semanticLabel = Some(text))

  private def inputBox(
    label: String,
    value: String,
    focusId: SurfaceFocusId,
    rect: LogicalPixelRect,
    selected: Boolean = true,
    cursorAtEnd: Boolean = true,
    segments: List[OverlaySegment] = Nil,
    layout: SurfacePaintLayout = SurfacePaintLayout.Plain,
    caret: Option[Int] = None
  ): SurfacePaintBox =
    SurfacePaintBox(
      kind = SurfacePaintKind.TextInput,
      rect = rect,
      text = Some(s"$label $value"),
      focusId = Some(focusId),
      semanticLabel = Some(label),
      selected = selected,
      cursorOffset = Option.when(selected && cursorAtEnd)(label.length + 1 + caret.getOrElse(value.length)),
      segments = if segments.nonEmpty then segments else List(OverlaySegment(label), OverlaySegment(value)),
      layout = if segments.nonEmpty then layout else SurfacePaintLayout.Split
    )

  private[layout] def actionBox(
    label: String,
    actionId: SurfaceActionId,
    focusId: SurfaceFocusId,
    selected: Boolean,
    rect: LogicalPixelRect,
    tone: OverlayTone = OverlayTone.Normal
  ): SurfacePaintBox =
    SurfacePaintBox(
      kind = SurfacePaintKind.ActionItem,
      rect = rect,
      text = Some(label),
      focusId = Some(focusId),
      actionId = Some(actionId),
      semanticLabel = Some(label),
      selected = selected,
      tone = tone
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

  private[layout] def rowRect(bounds: LogicalPixelRect, row: Int, height: Int = 1): LogicalPixelRect =
    LogicalPixelRect(
      bounds.x,
      bounds.y + row,
      bounds.width,
      math.min(height.toDouble, math.max(0.0, bounds.bottom - bounds.y - row))
    )

  private[layout] def clipBox(box: SurfacePaintBox, bounds: LogicalPixelRect): Option[SurfacePaintBox] =
    box.rect.intersection(bounds).map(clipped => box.copy(rect = clipped))

  private[layout] def logicalRect(x: Int, y: Int, width: Int, height: Int): LogicalPixelRect =
    LogicalPixelRect(x.toDouble, y.toDouble, width.toDouble, height.toDouble)
