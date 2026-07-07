package com.serenity.ui.renderer

import java.awt.Color

import com.serenity.command.{CommandCategory, CommandSurfaceItem}
import com.serenity.markdown.MarkdownDocumentPreview
import com.serenity.state.models.*
import com.serenity.ui.layout.*

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
  case Columns

case class OverlaySegment(
    text: String,
    selected: Boolean = false,
    tone: OverlayTone = OverlayTone.Normal,
    foregroundColor: Option[Color] = None,
    backgroundColor: Option[Color] = None,
    fontFamily: Option[String] = None
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
      case SurfaceContent.CursorInfoBar(text) =>
        ResolvedSurfaceContent(rows = List(OverlayRow(text)))
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
      case SurfaceContent.Outline(symbols, activeLocation) =>
        resolveOutline(rect, mode, symbols, activeLocation)
      case SurfaceContent.Diagnostics(issues) =>
        resolveDiagnostics(rect, mode, issues)
      case SurfaceContent.ThemePicker(state) =>
        resolveThemePicker(state, mode)
      case SurfaceContent.ThemeCreator(state) =>
        resolveThemeCreator(state, rect, mode)
      case SurfaceContent.FileSearch(state) =>
        resolveFileSearch(state, rect, mode)
      case SurfaceContent.ContextMenu(menu) =>
        resolveContextMenu(menu, rect, mode)
      case SurfaceContent.CommentLens(lens) =>
        ResolvedSurfaceContent(
          title = titleFor(mode, "comment"),
          header = Some(OverlayRow("comment")),
          rows = commentLensRows(lens)
        )
      case SurfaceContent.MarkdownPreview(_, title) =>
        ResolvedSurfaceContent(title = titleFor(mode, s"Preview: $title"))
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
      case Modal.Find(query, results, currentIndex) =>
        resolveFindWorkflow(query, results, currentIndex, rect, mode)
      case Modal.CloseWorkflow(workflow) =>
        resolveCloseWorkflow(workflow, mode)
      case _ =>
        ResolvedSurfaceContent(rows = modalLines(modal).map(OverlayRow(_)))

  private def titleFor(mode: SurfaceRenderMode, title: String): Option[String] =
    mode match
      case SurfaceRenderMode.Floating => None
      case SurfaceRenderMode.Pinned   => Some(title)

  private def commentLensRows(lens: CommentLensState): List[OverlayRow] =
    val (cursorLine, cursorColumn) = lineAndColumnAt(lens.draft, lens.clampedCursor)
    splitLines(lens.draft).zipWithIndex.map { (line, index) =>
      OverlayRow(
        plainText = line,
        selected = index == cursorLine,
        cursorColumn = Option.when(index == cursorLine)(cursorColumn)
      )
    }

  private def splitLines(text: String): List[String] =
    text.split("\n", -1).toList match
      case Nil => List("")
      case xs  => xs

  private def lineAndColumnAt(text: String, cursor: Int): (Int, Int) =
    text.take(math.max(0, math.min(cursor, text.length))).foldLeft((0, 0)) {
      case ((line, _), '\n') => (line + 1, 0)
      case ((line, col), _)  => (line, col + 1)
    }

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
      title = titleFor(mode, "find"),
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
      OverlaySegment("Close Anyway", selected = workflow.selectedChoice == CloseWorkflowChoice.Discard),
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
      given com.serenity.command.CommandRegistry = com.serenity.command.CommandRegistry.withToggleUI
      val header =
        if runner.searchTerm.isEmpty then Some(categoryTabs(runner.activeCategory))
        else
          Some(
            OverlayRow(
              plainText = s"search: ${runner.searchTerm}",
              cursorColumn = Some(s"search: ${runner.searchTerm}".length)
            )
          )

      val allItems = runner.visibleItems
      val itemWindow = SurfaceFrameLayout
        .forContent(rect, SurfaceContent.CommandPalette(runner))
        .itemWindow(
          itemCount = allItems.size,
          selectedIndex = runner.selectedIndex,
          hasHeader = true,
          hasFooter = allItems.nonEmpty || runner.statusMessage.nonEmpty
        )
      val windowItems           = itemWindow.slice(allItems)
      val adjustedSelectedIndex = itemWindow.adjustedSelectedIndex(runner.selectedIndex)

      val rows = windowItems.zipWithIndex.map {
        case (CommandSurfaceItem.CommandItem(command), index) =>
          val prefix =
            if runner.searchTerm.isEmpty then ""
            else s"[${categoryLabel(command.category)}] "
          commandRow(command, index == adjustedSelectedIndex, prefix)
        case (option: CommandSurfaceItem.OptionItem, index) =>
          optionRow(option, index == adjustedSelectedIndex)
        case (item: CommandSurfaceItem.InputItem, index) =>
          val editingText = if runner.editingItemId.contains(item.id) then Some(runner.editingText) else None
          inputRow(item, index == adjustedSelectedIndex, editingText)
        case (group: CommandSurfaceItem.GroupItem, index) =>
          val groupLabel =
            if runner.searchTerm.nonEmpty then runner.settingsGroupBreadcrumbLabels(group.id).mkString(" > ")
            else group.label
          OverlayRow(
            plainText = groupLabel,
            selected = index == adjustedSelectedIndex,
            segments = List(
              OverlaySegment(groupLabel),
              OverlaySegment(group.hint.getOrElse(""), tone = OverlayTone.Normal)
            ).filterNot(_.text.isEmpty),
            layout = OverlayRowLayout.Columns
          )
      }
      val footer =
        runner.statusMessage
          .map(OverlayRow(_))
          .orElse(Option.when(allItems.nonEmpty)(OverlayRow(s"${runner.selectedIndex + 1}/${allItems.length}")))

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
    val group         = runner.submenuGroup(groupId)
    val submenuState  = runner.activeSubmenu.filter(_.groupId == groupId)
    val allItems      = runner.submenuItems(groupId)
    val items         = submenuState.map(_.filteredItems(allItems)).getOrElse(allItems)
    val selectedIndex = submenuState.map(_.selectedIndex).getOrElse(0)
    val detailRows    = presetPreviewRow(runner, groupId, items.lift(selectedIndex))
    val itemWindow = SurfaceFrameLayout
      .forContent(rect, SurfaceContent.CommandPaletteSubmenu(runner, groupId, previewOnly))
      .itemWindow(
        itemCount = items.size,
        selectedIndex = selectedIndex,
        hasHeader = group.nonEmpty,
        hasFooter = items.nonEmpty || runner.statusMessage.nonEmpty,
        reservedContentRows = detailRows.size
      )
    val windowItems           = itemWindow.slice(items)
    val adjustedSelectedIndex = itemWindow.adjustedSelectedIndex(selectedIndex)
    val rows = windowItems.zipWithIndex.map {
      case (option: CommandSurfaceItem.OptionItem, index) =>
        optionRow(option, !previewOnly && index == adjustedSelectedIndex)
      case (item: CommandSurfaceItem.InputItem, index) =>
        val editingText =
          if !previewOnly then submenuState.filter(_.editingItemId.contains(item.id)).map(_.editingText)
          else None
        inputRow(item, !previewOnly && index == adjustedSelectedIndex, editingText)
      case (CommandSurfaceItem.CommandItem(command), index) =>
        commandRow(command, !previewOnly && index == adjustedSelectedIndex)
      case (group: CommandSurfaceItem.GroupItem, index) =>
        OverlayRow(
          plainText = group.label,
          selected = !previewOnly && index == adjustedSelectedIndex,
          segments = List(
            OverlaySegment(group.label),
            OverlaySegment(group.hint.getOrElse(""), tone = OverlayTone.Normal)
          ).filterNot(_.text.isEmpty),
          layout = OverlayRowLayout.Columns
        )
    }
    val footer =
      runner.statusMessage
        .map(OverlayRow(_))
        .orElse(Option.when(items.nonEmpty)(OverlayRow(s"${selectedIndex + 1}/${items.length}")))

    ResolvedSurfaceContent(
      title = titleFor(mode, group.map(_.label).getOrElse("submenu")),
      header = group.map { _ =>
        submenuState.filter(_.searchTerm.nonEmpty) match
          case Some(submenu) =>
            breadcrumbHeader(runner.submenuBreadcrumbLabels(groupId), Some(submenu.searchTerm))
          case None =>
            breadcrumbHeader(runner.submenuBreadcrumbLabels(groupId), None)
      },
      rows = rows ++ detailRows,
      footer = footer
    )

  private def breadcrumbHeader(labels: List[String], searchTerm: Option[String]): OverlayRow =
    val safeLabels = labels.filter(_.nonEmpty) match
      case Nil      => List("submenu")
      case nonEmpty => nonEmpty
    val lastIndex = safeLabels.length - 1
    val breadcrumbSegments = safeLabels.zipWithIndex.map { (label, index) =>
      val suffix =
        if index < lastIndex then " >"
        else searchTerm.filter(_.nonEmpty).fold("")(_ => " search:")
      OverlaySegment(s"$label$suffix", selected = index < lastIndex)
    }
    val segments = searchTerm.filter(_.nonEmpty) match
      case Some(term) => breadcrumbSegments :+ OverlaySegment(term, selected = true)
      case None       => breadcrumbSegments
    val plainText = segments.map(_.text).mkString(" ")
    OverlayRow(
      plainText = plainText,
      cursorColumn = searchTerm.filter(_.nonEmpty).map(_ => plainText.length),
      segments = segments
    )

  private def presetPreviewRow(
    runner: com.serenity.command.CommandRunner,
    groupId: String,
    selectedItem: Option[CommandSurfaceItem]
  ): List[OverlayRow] =
    selectedItem.collect {
      case group: CommandSurfaceItem.GroupItem
          if groupId == "settings-ui-presets" && group.id == "settings-preset-create" =>
        presetPreviewDetail("Create New Preset", "name and save the current workspace setup")
      case group: CommandSurfaceItem.GroupItem
          if groupId == "settings-ui-presets" && group.id == "settings-preset-edit" =>
        presetPreviewDetail(
          runner.editingPresetName.getOrElse("Edit Preset"),
          "name, active panels, theme, animations, fonts, document defaults"
        )
      case option: CommandSurfaceItem.OptionItem
          if groupId == "settings-preset-select" && option.id == "ui-preset-select" =>
        val hint = option.selectedHint.getOrElse("")
        presetPreviewDetail(option.selectedOption, hint)
    }.toList

  private def presetPreviewDetail(name: String, hint: String): OverlayRow =
    OverlayRow(
      plainText = s"Preset Preview $name - $hint",
      segments = List(
        OverlaySegment("Preset Preview"),
        OverlaySegment(name, selected = true),
        OverlaySegment(hint, tone = OverlayTone.Normal)
      ).filterNot(_.text.isEmpty),
      layout = OverlayRowLayout.Columns
    )

  private def categoryTabs(activeCategory: CommandCategory): OverlayRow =
    val categories = List(
      CommandCategory.All,
      CommandCategory.File,
      CommandCategory.View,
      CommandCategory.Edit,
      CommandCategory.Project,
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
      case CommandCategory.Project  => "Project"
      case CommandCategory.Settings => "Settings"

  private def commandRow(command: com.serenity.command.Command, selected: Boolean, prefix: String = ""): OverlayRow =
    OverlayRow(
      plainText = s"$prefix${command.label} - ${command.description}",
      selected = selected,
      segments = List(
        OverlaySegment(s"$prefix${command.label}", fontFamily = fontFamilyForCommand(command)),
        OverlaySegment(command.description, tone = OverlayTone.Normal)
      ),
      layout = OverlayRowLayout.Columns
    )

  private def fontFamilyForCommand(command: com.serenity.command.Command): Option[String] =
    command.intent match
      case com.serenity.command.CommandIntent.SetCodeFontFamily(family) => Some(family)
      case com.serenity.command.CommandIntent.SetTextFontFamily(family) => Some(family)
      case com.serenity.command.CommandIntent.SetUiFontFamily(family)   => Some(family)
      case _                                                            => None

  private def optionRow(option: CommandSurfaceItem.OptionItem, selected: Boolean): OverlayRow =
    val selectedHint = option.selectedHint.getOrElse("")

    OverlayRow(
      plainText = s"${option.label}: $selectedHint ${option.selectedOption}".trim,
      selected = selected,
      segments = List(
        OverlaySegment(option.label),
        OverlaySegment(selectedHint, tone = OverlayTone.Normal),
        OverlaySegment(option.selectedOption, selected = true)
      ),
      layout = OverlayRowLayout.Columns
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
      layout = OverlayRowLayout.Columns
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
    symbols: List[Symbol],
    activeLocation: Option[Location]
  ): ResolvedSurfaceContent =
    val shaped: List[OverlayRow] = SurfaceLayoutKind.classify(rect) match
      case SurfaceLayoutKind.Horizontal =>
        val visibleSymbols = symbols.take(4)
        val activeVisible  = visibleSymbols.exists(symbol => activeLocation.contains(symbol.location))
        List(
          visibleSymbols
            .map(symbol => if activeLocation.contains(symbol.location) then s"[${symbol.name}]" else symbol.name)
            .mkString(" | ")
        ).filter(_.nonEmpty).map(text => OverlayRow(text, selected = activeVisible))
      case SurfaceLayoutKind.Vertical =>
        symbols.take(math.max(1, rect.height - 2)).map { symbol =>
          val active = activeLocation.contains(symbol.location)
          val prefix = if active then "> " else ""
          OverlayRow(s"$prefix${symbol.kind} ${symbol.name}", selected = active)
        }
      case SurfaceLayoutKind.Square =>
        symbols.take(math.max(1, rect.height - 2)).map { symbol =>
          val active = activeLocation.contains(symbol.location)
          val prefix = if active then "> " else ""
          OverlayRow(s"$prefix${symbol.name}", selected = active)
        }
      case SurfaceLayoutKind.Compact =>
        val current = activeLocation.flatMap(location => symbols.find(_.location == location)).map(_.name)
        current match
          case Some(name) => List(OverlayRow(s"${symbols.length} symbols", selected = true), OverlayRow(name))
          case None       => List(OverlayRow(s"${symbols.length} symbols"))

    ResolvedSurfaceContent(titleFor(mode, "outline"), rows = shaped)

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

  private def resolveThemeCreator(
    state: com.serenity.ui.theme.config.ThemeCreatorState,
    rect: LayoutRect,
    mode: SurfaceRenderMode
  ): ResolvedSurfaceContent =
    val allRows = state.rows.zipWithIndex.map { (row, index) =>
      val selected = index == state.selectedIndex
      val valueTone =
        if row.valid then OverlayTone.Normal
        else OverlayTone.Error
      val valueSegment = OverlaySegment(
        row.value,
        selected = selected,
        tone = valueTone,
        foregroundColor = row.previewColor.map(contrastColor),
        backgroundColor = row.previewColor
      )
      OverlayRow(
        plainText = s"${row.label}: ${row.value}",
        selected = selected,
        cursorColumn = Option.when(selected)(s"${row.label}: ${row.value}".length),
        segments = List(
          OverlaySegment(row.label),
          OverlaySegment(row.path, tone = OverlayTone.Muted),
          valueSegment
        ),
        layout = OverlayRowLayout.Columns
      )
    }
    val itemWindow = SurfaceFrameLayout(rect).itemWindow(
      itemCount = allRows.size,
      selectedIndex = state.selectedIndex,
      hasHeader = true,
      hasFooter = state.statusMessage.nonEmpty
    )
    ResolvedSurfaceContent(
      title = titleFor(mode, "Theme Creator"),
      header = Some(OverlayRow("theme creator")),
      rows = itemWindow.slice(allRows),
      footer = state.statusMessage.map(OverlayRow(_, foregroundColor = Some(java.awt.Color.RED)))
    )

  private def contrastColor(color: java.awt.Color): java.awt.Color =
    val luminance = (0.299 * color.getRed + 0.587 * color.getGreen + 0.114 * color.getBlue) / 255.0
    if luminance > 0.55 then java.awt.Color.BLACK else java.awt.Color.WHITE

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
      rows = resultRows,
      footer = Option.when(state.hasMoreResults)(OverlayRow(s"${state.results.length} loaded, more available"))
    )

  private def resolveContextMenu(
    menu: ContextMenu,
    rect: LayoutRect,
    mode: SurfaceRenderMode
  ): ResolvedSurfaceContent =
    val itemWindow = SurfaceFrameLayout(rect).itemWindow(
      itemCount = menu.items.size,
      selectedIndex = menu.selectedIndex,
      hasHeader = true,
      hasFooter = menu.items.nonEmpty
    )
    val visibleItems = itemWindow.slice(menu.items)
    val rows = visibleItems.zipWithIndex.map {
      case (item, index) =>
        OverlayRow(
          plainText = item.label,
          selected = index + itemWindow.offset == menu.selectedIndex
        )
    }

    ResolvedSurfaceContent(
      title = titleFor(mode, menu.title),
      header = Some(OverlayRow(menu.title)),
      rows = rows,
      footer = Option.when(menu.items.nonEmpty)(OverlayRow(s"${menu.selectedIndex + 1}/${menu.items.length}"))
    )

  def resolveMarkdownPreview(
    title: String,
    content: String,
    rect: LayoutRect,
    mode: SurfaceRenderMode
  ): ResolvedSurfaceContent =
    val contentRows = SurfaceFrameLayout(rect).contentRect.height.max(0)
    val rows =
      MarkdownDocumentPreview
        .renderInlineLines(content.linesIterator.toVector)
        .take(contentRows)
        .filter(_.trim.nonEmpty)
        .map(OverlayRow(_))
        .toList
    ResolvedSurfaceContent(
      title = titleFor(mode, s"Preview: $title"),
      rows = rows
    )
