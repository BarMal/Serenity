package com.serenity.ui.renderer

import java.awt.Color

import com.serenity.command.{CommandCategory, CommandRegistry, CommandSurfaceItem}
import com.serenity.state.models.*
import com.serenity.ui.layout.{LayoutRect, SurfaceLayoutKind}

enum SurfaceRenderMode:
  case Floating
  case Pinned

enum OverlayTone:
  case Normal
  case Muted
  case Error

enum OverlayRowLayout:
  case Plain
  case Distributed
  case Split

case class OverlaySegment(
    text: String,
    selected: Boolean = false,
    tone: OverlayTone = OverlayTone.Normal,
    foregroundColor: Option[Color] = None,
    backgroundColor: Option[Color] = None
)

case class OverlayRow(
    plainText: String,
    selected: Boolean = false,
    cursorColumn: Option[Int] = None,
    foregroundColor: Option[Color] = None,
    backgroundColor: Option[Color] = None,
    segments: List[OverlaySegment] = Nil,
    layout: OverlayRowLayout = OverlayRowLayout.Plain
)

case class ResolvedSurfaceContent(
    title: Option[String] = None,
    header: Option[OverlayRow] = None,
    rows: List[OverlayRow] = Nil,
    footer: Option[OverlayRow] = None
)

object SurfaceContentResolver:

  def resolve(
    content: SurfaceContent,
    rect: LayoutRect,
    mode: SurfaceRenderMode
  ): ResolvedSurfaceContent =
    content match
      case SurfaceContent.StartPage(_) =>
        ResolvedSurfaceContent()
      case SurfaceContent.QuickInfo(text) =>
        ResolvedSurfaceContent(
          title = None,
          rows = text.linesIterator.toList match
            case Nil   => List(OverlayRow(""))
            case lines => lines.map(OverlayRow(_))
        )
      case SurfaceContent.FilePreview(path, content) =>
        ResolvedSurfaceContent(
          titleFor(mode, s"Preview: ${path.getFileName}"),
          rows = content.linesIterator.take(4).toList.map(OverlayRow(_))
        )
      case SurfaceContent.SymbolDefinition(symbol, location) =>
        ResolvedSurfaceContent(
          titleFor(mode, "symbol"),
          rows = List(
            OverlayRow(s"Symbol: $symbol"),
            OverlayRow(s"Line ${location.line + 1}, Col ${location.column + 1}")
          )
        )
      case SurfaceContent.DirectoryListing(path, entries, selectedPath) =>
        resolveDirectoryListing(
          rect,
          mode,
          path.getFileName.toString,
          entries.map(_.name),
          selectedPath.flatMap(p => Option(p.getFileName).map(_.toString))
        )
      case SurfaceContent.DirectoryTree(tree, selectedPath) =>
        resolveDirectoryTree(rect, mode, tree, selectedPath)
      case SurfaceContent.CommandPalette(runner) =>
        resolveCommandPalette(runner, rect, mode)
      case SurfaceContent.CommandPaletteSubmenu(runner, groupId, previewOnly) =>
        resolveCommandPaletteSubmenu(runner, groupId, previewOnly, rect, mode)
      case SurfaceContent.ModalWorkflow(modal) =>
        resolveModalWorkflow(modal, rect, mode)
      case SurfaceContent.Terminal(buffer, cursor) =>
        resolveTerminal(rect, mode, buffer, cursor)
      case SurfaceContent.Outline(symbols) =>
        resolveOutline(rect, mode, symbols.map(symbol => (symbol.kind.toString, symbol.name)))
      case SurfaceContent.Diagnostics(issues) =>
        resolveDiagnostics(rect, mode, issues)
      case SurfaceContent.ThemePicker(state) =>
        resolveThemePicker(state, mode)
      case SurfaceContent.FileSearch(state) =>
        resolveFileSearch(state, rect, mode)
      case SurfaceContent.GhostOverlay(originalContent, cachedRect) =>
        resolve(originalContent, cachedRect, mode)

  private def resolveModalWorkflow(
    modal: Modal,
    rect: LayoutRect,
    mode: SurfaceRenderMode
  ): ResolvedSurfaceContent =
    modal match
      case Modal.FileWorkflow(workflow) =>
        resolveFileWorkflow(workflow, rect, mode)
      case Modal.ReplaceWorkflow(workflow) =>
        resolveReplaceWorkflow(workflow, mode)
      case Modal.CloseWorkflow(workflow) =>
        resolveCloseWorkflow(workflow, mode)
      case _ =>
        ResolvedSurfaceContent(rows = modalLines(modal).map(OverlayRow(_)))

  private def titleFor(mode: SurfaceRenderMode, title: String): Option[String] =
    mode match
      case SurfaceRenderMode.Floating => None
      case SurfaceRenderMode.Pinned   => Some(title)

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

  private def resolveReplaceWorkflow(
    workflow: ReplaceWorkflowState,
    mode: SurfaceRenderMode
  ): ResolvedSurfaceContent =
    val findRow = OverlayRow(
      plainText = s"Find ${workflow.findText}",
      selected = workflow.activeField == ReplaceWorkflowField.Find,
      segments = List(
        OverlaySegment("Find"),
        OverlaySegment(workflow.findText, selected = workflow.activeField == ReplaceWorkflowField.Find)
      ),
      layout = OverlayRowLayout.Split
    )

    val replaceRow = OverlayRow(
      plainText = s"Replace ${workflow.replacementText}",
      selected = workflow.activeField == ReplaceWorkflowField.ReplaceWith,
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
      title = titleFor(mode, "replace"),
      header = Some(OverlayRow("replace")),
      rows = List(findRow, replaceRow, actionRow, scopeRow),
      footer = workflow.statusMessage.map(OverlayRow(_))
    )

  private def resolveCloseWorkflow(
    workflow: CloseWorkflowState,
    mode: SurfaceRenderMode
  ): ResolvedSurfaceContent =
    val choiceSegments = List(
      OverlaySegment("Save", selected = workflow.selectedChoice == CloseWorkflowChoice.Save),
      OverlaySegment("Discard", selected = workflow.selectedChoice == CloseWorkflowChoice.Discard),
      OverlaySegment("Cancel", selected = workflow.selectedChoice == CloseWorkflowChoice.Cancel)
    )

    ResolvedSurfaceContent(
      title = titleFor(mode, "unsaved changes"),
      header = Some(OverlayRow("unsaved changes")),
      rows = List(
        OverlayRow(workflow.currentBufferLabel),
        OverlayRow(
          plainText = choiceSegments.map(_.text).mkString(" "),
          segments = choiceSegments,
          layout = OverlayRowLayout.Distributed
        )
      )
    )

  private def resolveFileWorkflow(
    workflow: FileWorkflowState,
    rect: LayoutRect,
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
      title = titleFor(mode, operationLabel),
      header = Some(OverlayRow(operationLabel)),
      rows = filenameRow :: pathRow :: suggestionRows,
      footer = footer
    )

  private def resolveCommandPalette(
    runner: com.serenity.command.CommandRunner,
    rect: LayoutRect,
    mode: SurfaceRenderMode
  ): ResolvedSurfaceContent =
    if !runner.isActive then ResolvedSurfaceContent(titleFor(mode, "commands"))
    else
      given CommandRegistry = CommandRegistry.withToggleUI
      val header =
        if runner.searchTerm.isEmpty then Some(categoryTabs(runner.activeCategory))
        else
          Some(
            OverlayRow(
              plainText = s"search: ${runner.searchTerm}",
              cursorColumn = Some(s"search: ${runner.searchTerm}".length)
            )
          )

      val allItems    = runner.visibleItems
      val maxItemRows = math.max(1, rect.height - 4)
      val offset =
        if allItems.size <= maxItemRows then 0
        else
          val half = maxItemRows / 2
          math.max(0, math.min(runner.selectedIndex - half, allItems.size - maxItemRows))

      val windowItems           = allItems.slice(offset, offset + maxItemRows)
      val adjustedSelectedIndex = runner.selectedIndex - offset

      val rows = windowItems.zipWithIndex.map {
        case (CommandSurfaceItem.CommandItem(command), index) =>
          val prefix =
            if runner.searchTerm.isEmpty then ""
            else s"[${categoryLabel(command.category)}] "
          OverlayRow(
            plainText = s"$prefix${command.label} - ${command.description}",
            selected = index == adjustedSelectedIndex
          )
        case (option: CommandSurfaceItem.OptionItem, index) =>
          optionRow(option, index == adjustedSelectedIndex)
        case (item: CommandSurfaceItem.InputItem, index) =>
          val editingText = if runner.editingItemId.contains(item.id) then Some(runner.editingText) else None
          inputRow(item, index == adjustedSelectedIndex, editingText)
        case (group: CommandSurfaceItem.GroupItem, index) =>
          OverlayRow(
            plainText = group.label,
            selected = index == adjustedSelectedIndex,
            segments = List(
              OverlaySegment(group.label),
              OverlaySegment(group.hint.getOrElse(""), tone = OverlayTone.Normal)
            ).filterNot(_.text.isEmpty),
            layout = OverlayRowLayout.Split
          )
      }
      val footer =
        if allItems.nonEmpty then Some(OverlayRow(s"${runner.selectedIndex + 1}/${allItems.length}"))
        else None

      ResolvedSurfaceContent(
        title = titleFor(mode, "commands"),
        header = header,
        rows = rows,
        footer = footer
      )

  private def resolveCommandPaletteSubmenu(
    runner: com.serenity.command.CommandRunner,
    groupId: String,
    previewOnly: Boolean,
    rect: LayoutRect,
    mode: SurfaceRenderMode
  ): ResolvedSurfaceContent =
    val group         = runner.settingsGroups.find(_.id == groupId)
    val submenuState  = runner.activeSubmenu.filter(_.groupId == groupId)
    val allItems      = runner.submenuItems(groupId)
    val items         = submenuState.map(_.filteredItems(allItems)).getOrElse(allItems)
    val selectedIndex = submenuState.map(_.selectedIndex).getOrElse(0)
    val maxItemRows   = math.max(1, rect.height - 4)
    val offset =
      if items.size <= maxItemRows then 0
      else
        val half = maxItemRows / 2
        math.max(0, math.min(selectedIndex - half, items.size - maxItemRows))
    val windowItems           = items.slice(offset, offset + maxItemRows)
    val adjustedSelectedIndex = selectedIndex - offset
    val rows = windowItems.zipWithIndex.map {
      case (option: CommandSurfaceItem.OptionItem, index) =>
        optionRow(option, !previewOnly && index == adjustedSelectedIndex)
      case (item: CommandSurfaceItem.InputItem, index) =>
        val editingText =
          if !previewOnly then submenuState.filter(_.editingItemId.contains(item.id)).map(_.editingText)
          else None
        inputRow(item, !previewOnly && index == adjustedSelectedIndex, editingText)
      case (CommandSurfaceItem.CommandItem(command), index) =>
        OverlayRow(
          plainText = s"${command.label} - ${command.description}",
          selected = !previewOnly && index == adjustedSelectedIndex
        )
      case (group: CommandSurfaceItem.GroupItem, index) =>
        OverlayRow(
          plainText = group.label,
          selected = !previewOnly && index == adjustedSelectedIndex
        )
    }
    val footer =
      Option.when(items.nonEmpty)(OverlayRow(s"${selectedIndex + 1}/${items.length}"))

    ResolvedSurfaceContent(
      title = titleFor(mode, group.map(_.label).getOrElse("submenu")),
      header = group.map { g =>
        submenuState.filter(_.searchTerm.nonEmpty) match
          case Some(submenu) =>
            OverlayRow(
              plainText = s"${g.label} search: ${submenu.searchTerm}",
              cursorColumn = Some(s"${g.label} search: ${submenu.searchTerm}".length)
            )
          case None =>
            OverlayRow(g.label)
      },
      rows = rows,
      footer = footer
    )

  private def categoryTabs(activeCategory: CommandCategory): OverlayRow =
    val categories = List(
      CommandCategory.All,
      CommandCategory.File,
      CommandCategory.View,
      CommandCategory.Edit,
      CommandCategory.Settings
    )
    OverlayRow(
      plainText = categories.map(categoryLabel).mkString(" "),
      segments = categories.map(category =>
        OverlaySegment(
          text = categoryLabel(category),
          selected = category == activeCategory
        )
      ),
      layout = OverlayRowLayout.Distributed
    )

  private def categoryLabel(category: CommandCategory): String =
    category match
      case CommandCategory.All      => "All"
      case CommandCategory.File     => "File"
      case CommandCategory.View     => "View"
      case CommandCategory.Edit     => "Edit"
      case CommandCategory.Settings => "Settings"

  private def optionRow(option: CommandSurfaceItem.OptionItem, selected: Boolean): OverlayRow =
    val rightSegments =
      option.hint.toList.map(hint => OverlaySegment(hint, tone = OverlayTone.Normal)) :+
        OverlaySegment(option.selectedOption, selected = true)

    OverlayRow(
      plainText = s"${option.label}: ${option.hint.map(_ + " ").getOrElse("")}${option.selectedOption}",
      selected = selected,
      segments = OverlaySegment(option.label) :: rightSegments,
      layout = OverlayRowLayout.Split
    )

  private def inputRow(
    item: CommandSurfaceItem.InputItem,
    selected: Boolean,
    editingText: Option[String]
  ): OverlayRow =
    val displayText = editingText.getOrElse(item.currentValue)
    val isError     = editingText.exists(item.isOutOfBounds)
    val valueTone   = if isError then OverlayTone.Error else OverlayTone.Normal
    val cursorCol   = editingText.map(_ => s"${item.label}: ${item.hint} ".length + displayText.length)
    OverlayRow(
      plainText = s"${item.label}: ${item.hint} $displayText",
      selected = selected,
      cursorColumn = cursorCol,
      segments = List(
        OverlaySegment(item.label),
        OverlaySegment(item.hint, tone = OverlayTone.Normal),
        OverlaySegment(displayText, tone = valueTone, selected = editingText.isDefined)
      ),
      layout = OverlayRowLayout.Split
    )

  private def resolveDirectoryListing(
    rect: LayoutRect,
    mode: SurfaceRenderMode,
    rootName: String,
    entryNames: List[String],
    selectedName: Option[String]
  ): ResolvedSurfaceContent =
    val layoutKind = SurfaceLayoutKind.classify(rect)
    val lines = (mode, layoutKind) match
      case (SurfaceRenderMode.Floating, SurfaceLayoutKind.Horizontal) =>
        List(s"$rootName  ${entryNames.take(4).mkString(" | ")}")
      case (SurfaceRenderMode.Floating, SurfaceLayoutKind.Vertical) =>
        rootName :: entryNames.take(4)
      case (SurfaceRenderMode.Floating, SurfaceLayoutKind.Square) =>
        s"Directory: $rootName" :: entryNames.take(3)
      case (SurfaceRenderMode.Floating, SurfaceLayoutKind.Compact) =>
        List(s"$rootName (${entryNames.length})")
      case (SurfaceRenderMode.Pinned, SurfaceLayoutKind.Horizontal) =>
        List(entryNames.take(4).mkString(" | ")).filter(_.nonEmpty)
      case (SurfaceRenderMode.Pinned, SurfaceLayoutKind.Vertical) =>
        entryNames.take(4)
      case (SurfaceRenderMode.Pinned, SurfaceLayoutKind.Square) =>
        selectedName.map(name => s"Selected: $name").toList ++ entryNames.take(3)
      case (SurfaceRenderMode.Pinned, SurfaceLayoutKind.Compact) =>
        List(s"${entryNames.length} entries")

    ResolvedSurfaceContent(
      title = titleFor(mode, rootName),
      rows = lines.map(OverlayRow(_))
    )

  private def resolveDirectoryTree(
    rect: LayoutRect,
    mode: SurfaceRenderMode,
    tree: com.serenity.ui.layout.DirectoryTreeData,
    selectedPath: Option[java.nio.file.Path]
  ): ResolvedSurfaceContent =
    val visibleRows = com.serenity.ui.layout.DirectoryTreeData.visibleRows(tree)
    val maxRows     = math.max(1, rect.height - 2)
    val rows = visibleRows.take(maxRows).map { row =>
      val marker =
        if row.isDirectory then
          if row.isExpanded then "▾ "
          else if row.isLoaded then "▸ "
          else "▹ "
        else ""
      val indent = "  " * row.depth
      OverlayRow(
        plainText = s"$indent$marker${row.name}",
        selected = selectedPath.contains(row.path)
      )
    }

    ResolvedSurfaceContent(
      title = titleFor(mode, tree.rootPath.getFileName.toString),
      rows = rows
    )

  private def resolveTerminal(
    rect: LayoutRect,
    mode: SurfaceRenderMode,
    buffer: String,
    cursor: Int
  ): ResolvedSurfaceContent =
    val lines = buffer.linesIterator.toList
    val shaped = SurfaceLayoutKind.classify(rect) match
      case SurfaceLayoutKind.Horizontal =>
        lines.take(math.max(1, rect.height - 2))
      case SurfaceLayoutKind.Vertical =>
        lines.take(math.max(1, rect.height - 2)).zipWithIndex.map { case (line, index) => s"${index + 1}: $line" }
      case SurfaceLayoutKind.Square =>
        s"cursor: $cursor" :: lines.take(math.max(0, rect.height - 3))
      case SurfaceLayoutKind.Compact =>
        List(s"${lines.length} lines", s"cursor $cursor")

    ResolvedSurfaceContent(titleFor(mode, "terminal"), rows = shaped.map(OverlayRow(_)))

  private def resolveOutline(
    rect: LayoutRect,
    mode: SurfaceRenderMode,
    symbols: List[(String, String)]
  ): ResolvedSurfaceContent =
    val shaped = SurfaceLayoutKind.classify(rect) match
      case SurfaceLayoutKind.Horizontal =>
        List(symbols.take(4).map(_._2).mkString(" | ")).filter(_.nonEmpty)
      case SurfaceLayoutKind.Vertical =>
        symbols.take(math.max(1, rect.height - 2)).map { case (kind, name) => s"$kind $name" }
      case SurfaceLayoutKind.Square =>
        symbols.take(math.max(1, rect.height - 2)).map(_._2)
      case SurfaceLayoutKind.Compact =>
        List(s"${symbols.length} symbols")

    ResolvedSurfaceContent(titleFor(mode, "outline"), rows = shaped.map(OverlayRow(_)))

  private def resolveDiagnostics(
    rect: LayoutRect,
    mode: SurfaceRenderMode,
    issues: List[com.serenity.ui.layout.Diagnostic]
  ): ResolvedSurfaceContent =
    val errorCount   = issues.count(_.severity == com.serenity.ui.layout.DiagnosticSeverity.Error)
    val warningCount = issues.count(_.severity == com.serenity.ui.layout.DiagnosticSeverity.Warning)
    val infoCount = issues.count(issue =>
      issue.severity == com.serenity.ui.layout.DiagnosticSeverity.Info ||
        issue.severity == com.serenity.ui.layout.DiagnosticSeverity.Hint
    )
    val shaped = SurfaceLayoutKind.classify(rect) match
      case SurfaceLayoutKind.Horizontal =>
        List(s"$errorCount error | $warningCount warning | $infoCount info")
      case SurfaceLayoutKind.Vertical =>
        issues.take(math.max(1, rect.height - 2)).map(issue => s"${issue.severity}: ${issue.message}")
      case SurfaceLayoutKind.Square =>
        s"$errorCount error, $warningCount warning" :: issues.take(math.max(0, rect.height - 3)).map(_.message)
      case SurfaceLayoutKind.Compact =>
        List(s"${issues.length} issues", s"$errorCount error")

    ResolvedSurfaceContent(titleFor(mode, "diagnostics"), rows = shaped.map(OverlayRow(_)))

  private def resolveThemePicker(state: ThemePickerState, mode: SurfaceRenderMode): ResolvedSurfaceContent =
    val rows =
      state.themes.zipWithIndex.map((name, idx) => OverlayRow(plainText = name, selected = idx == state.selectedIndex))
    ResolvedSurfaceContent(titleFor(mode, "Theme"), rows = rows)

  private def resolveFileSearch(
    state: FileSearchState,
    rect: LayoutRect,
    mode: SurfaceRenderMode
  ): ResolvedSurfaceContent =
    val headerRow = OverlayRow(
      plainText = if state.query.isEmpty then " " else state.query,
      cursorColumn = Some(state.query.length)
    )
    val resultRows = state.results.take(rect.height - 2).zipWithIndex.map { (result, idx) =>
      OverlayRow(
        plainText = s"${result.bufferName}:${result.line + 1}  ${result.lineContent}",
        selected = idx == state.selectedIndex
      )
    }
    ResolvedSurfaceContent(
      title = titleFor(mode, "Search"),
      header = Some(headerRow),
      rows = resultRows
    )
