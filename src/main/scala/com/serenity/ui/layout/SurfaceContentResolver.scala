package com.serenity.ui.layout

import java.awt.Color

import scala.annotation.unused

import com.serenity.command.{CommandCategory, CommandSurfaceItem, FontIntent, SettingsIntent, SettingsSurfaceState}
import com.serenity.config.ToolbarDisplayMode
import com.serenity.markdown.MarkdownDocumentPreview
import com.serenity.state.models.*
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.layout.*
import com.serenity.ui.theme.Theme

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
  case PriorityColumns

final case class OverlaySegment(
    text: String,
    selected: Boolean = false,
    tone: OverlayTone = OverlayTone.Normal,
    foregroundColor: Option[Color] = None,
    backgroundColor: Option[Color] = None,
    fontFamily: Option[String] = None,
    inlineIcon: Option[String] = None,
    inlineIconFontFamily: Option[String] = None,
    trailingSeparator: Boolean = false,
    allocatedWidth: Option[Int] = None
)

final case class OverlayRow(
    plainText: String,
    selected: Boolean = false,
    cursorColumn: Option[Int] = None,
    foregroundColor: Option[Color] = None,
    backgroundColor: Option[Color] = None,
    segments: List[OverlaySegment] = Nil,
    layout: OverlayRowLayout = OverlayRowLayout.Plain,
    leadingPadding: Int = 0
)

final case class ResolvedSurfaceContent(
    title: Option[String] = None,
    header: Option[OverlayRow] = None,
    rows: List[OverlayRow] = Nil,
    footer: Option[OverlayRow] = None
)

object SurfaceContentResolver:

  def resolve(
    content: SurfaceContent,
    rect: LayoutRect,
    mode: SurfaceRenderMode,
    itemGapRows: Double = 0.0,
    itemTargetRows: Int = 1
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
        resolveCommandPalette(runner, rect, mode, itemGapRows, itemTargetRows)
      case SurfaceContent.ModalWorkflow(modal) =>
        resolveModalWorkflow(modal, rect, mode)
      case SurfaceContent.Terminal(buffer, cursor) =>
        resolveTerminal(rect, mode, buffer, cursor)
      case SurfaceContent.Outline(symbols, activeLocation) =>
        resolveOutline(rect, mode, symbols, activeLocation)
      case SurfaceContent.Comments(symbols, activeLocation) =>
        resolveComments(rect, mode, symbols, activeLocation)
      case SurfaceContent.Diagnostics(issues, activeLocation) =>
        resolveDiagnostics(rect, mode, issues, activeLocation)
      case SurfaceContent.ThemePicker(state) =>
        resolveThemePicker(state, rect, mode)
      case SurfaceContent.ThemeCreator(state) =>
        resolveThemeCreator(state, rect, mode)
      case SurfaceContent.FileSearch(state) =>
        resolveFileSearch(state, rect, mode)
      case SurfaceContent.ContextualToolbar(_) =>
        ResolvedSurfaceContent()
      case SurfaceContent.ContextMenu(menu) =>
        resolveContextMenu(menu, rect, mode, itemGapRows)
      case SurfaceContent.CommentLens(lens) =>
        ResolvedSurfaceContent(
          title = titleFor(mode, "comment"),
          header = Some(OverlayRow("comment")),
          rows = commentLensRows(lens)
        )
      case SurfaceContent.MarkdownPreview(_, title) =>
        ResolvedSurfaceContent(title = titleFor(mode, s"Preview: $title"))
      case SurfaceContent.GhostOverlay(originalContent, cachedRect) =>
        resolve(originalContent, cachedRect, mode, itemGapRows)

  private def resolveModalWorkflow(
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
      title = titleFor(mode, operationLabel),
      header = Some(OverlayRow(operationLabel)),
      rows = filenameRow :: pathRow :: suggestionRows,
      footer = footer
    )

  private def resolveCommandPalette(
    runner: com.serenity.command.CommandRunner,
    rect: LayoutRect,
    mode: SurfaceRenderMode,
    itemGapRows: Double,
    itemTargetRows: Int
  ): ResolvedSurfaceContent =
    // Dispatch on `CommandRunnerSurface` (issue #931, Stage 2) rather than `isSettingsSurface`/
    // `activeSettingsSurface.isDefined` directly -- `Settings(_)` covers both entry points exactly as those two
    // conditions did (issue #1059: "one consistent settings experience regardless of entry point"), and is checked
    // first regardless of `isActive`, preserving this resolver's existing precedence (a settings-mode runner that
    // has since been deactivated still renders through `resolveSettingsSurface`, not the inactive placeholder below).
    runner.surface match
      case _: com.serenity.command.CommandRunnerSurface.Settings =>
        resolveSettingsSurface(runner, rect, itemGapRows, itemTargetRows)
      case com.serenity.command.CommandRunnerSurface.Palette(_) if !runner.isActive =>
        ResolvedSurfaceContent(titleFor(mode, "commands"))
      case com.serenity.command.CommandRunnerSurface.Palette(paletteState) =>
        // Category tabs are retired (issue #931): the header is always the live search box now, empty or not,
        // rather than switching to a category-switcher row when there's nothing typed yet.
        val header = Some(
          OverlayRow(
            plainText = s"search: ${paletteState.searchTerm}",
            cursorColumn = Some(s"search: ${paletteState.searchTerm}".length)
          )
        )

        val allItems = runner.visibleItems
        // Same capped, expand-in-place group preview as resolveSettingsSurface, for a settings group still sitting
        // in this mixed list (browsing the Settings tab before drilling into any group) -- issue #1059.
        val groupPreview = groupPreviewRows(SettingsSurfaceState.previewRows(allItems, runner.selectedIndex))
        val itemWindow = SurfaceFrameLayout
          .forContent(rect, SurfaceContent.CommandPalette(runner))
          .itemWindow(
            itemCount = allItems.size,
            selectedIndex = runner.selectedIndex,
            hasHeader = true,
            hasFooter = allItems.nonEmpty || runner.statusMessage.nonEmpty,
            reservedContentRows = groupPreview.size,
            itemGapRows = itemGapRows,
            itemTargetRows = itemTargetRows
          )
        val windowItems           = itemWindow.slice(allItems)
        val adjustedSelectedIndex = itemWindow.adjustedSelectedIndex(runner.selectedIndex)

        val rows = windowItems.zipWithIndex.flatMap {
          case (item, index) =>
            val selected = index == adjustedSelectedIndex
            val row = item match
              case CommandSurfaceItem.CommandItem(command) =>
                val prefix =
                  if runner.searchTerm.isEmpty then ""
                  else s"[${categoryLabel(command.category)}] "
                commandRow(command, selected, prefix, runner.bindingFor(command))
              case option: CommandSurfaceItem.OptionItem =>
                optionRow(option, selected)
              case item: CommandSurfaceItem.InputItem =>
                val editingText = if runner.editingItemId.contains(item.id) then Some(runner.editingText) else None
                inputRow(item, selected, editingText)
              case item: CommandSurfaceItem.SettingSearchItem =>
                settingSearchRow(item, selected)
              case group: CommandSurfaceItem.GroupItem =>
                val groupLabel =
                  if runner.searchTerm.nonEmpty then runner.settingsGroupBreadcrumbLabels(group.id).mkString(" > ")
                  else group.label
                OverlayRow(
                  plainText = groupLabel,
                  selected = selected,
                  segments = List(
                    OverlaySegment(groupLabel),
                    OverlaySegment(group.hint.getOrElse(""), tone = OverlayTone.Normal)
                  ).filterNot(_.text.isEmpty),
                  layout = OverlayRowLayout.Columns
                )
            if selected then row :: groupPreview else List(row)
        }
        val footer =
          runner.statusMessage
            .map(OverlayRow(_))
            .orElse(
              Option.when(allItems.nonEmpty)(
                OverlayRow(commandPaletteFooter(runner, allItems.length))
              )
            )

        ResolvedSurfaceContent(
          title = titleFor(mode, "commands"),
          header = header,
          rows = rows,
          footer = footer
        )

  private def resolveSettingsSurface(
    runner: com.serenity.command.CommandRunner,
    rect: LayoutRect,
    itemGapRows: Double,
    itemTargetRows: Int
  ): ResolvedSurfaceContent =
    val items         = runner.settingsSurfaceItems
    val selectedIndex = runner.settingsSurfaceSelectedIndex
    // Capped, expand-in-place group preview (issue #1059): when the selected row is itself a group, up to four of
    // its children render as indented, de-emphasized rows immediately under it, in this same list -- replacing the
    // second floating surface `CommandPaletteSubmenu` used to show for a hovered-but-not-yet-entered group.
    val groupPreview = groupPreviewRows(SettingsSurfaceState.previewRows(items, selectedIndex))
    val itemWindow = SurfaceFrameLayout
      .forContent(rect, SurfaceContent.CommandPalette(runner))
      .itemWindow(
        itemCount = items.size,
        selectedIndex = selectedIndex,
        hasHeader = true,
        hasFooter = true,
        reservedContentRows = groupPreview.size,
        itemGapRows = itemGapRows,
        itemTargetRows = itemTargetRows
      )
    val adjustedSelectedIndex = itemWindow.adjustedSelectedIndex(selectedIndex)
    val rows = itemWindow.slice(items).zipWithIndex.flatMap {
      case (item, index) =>
        val selected = index == adjustedSelectedIndex
        val row = item match
          case CommandSurfaceItem.CommandItem(command) =>
            commandRow(command, selected, binding = runner.bindingFor(command))
          case option: CommandSurfaceItem.OptionItem =>
            optionRow(option, selected)
          case item: CommandSurfaceItem.InputItem =>
            val editingText =
              runner.activeSettingsSurface
                .filter(_.current.editingItemId.contains(item.id))
                .map(_.current.draftText)
            inputRow(item, selected, editingText)
          case item: CommandSurfaceItem.SettingSearchItem =>
            OverlayRow(
              plainText = item.label,
              selected = selected,
              segments = List(
                OverlaySegment(item.label),
                OverlaySegment(item.effectiveValue.getOrElse("")),
                OverlaySegment(item.sourceScope),
                OverlaySegment(item.breadcrumb)
              ).filterNot(_.text.isEmpty),
              layout = OverlayRowLayout.Columns
            )
          case group: CommandSurfaceItem.GroupItem =>
            OverlayRow(
              plainText = group.label,
              selected = selected,
              segments =
                List(OverlaySegment(group.label), OverlaySegment(group.hint.getOrElse(""))).filterNot(_.text.isEmpty),
              layout = OverlayRowLayout.Columns
            )
        if selected then row :: groupPreview else List(row)
    }
    val searchTerm     = runner.activeSettingsSurface.fold(runner.searchTerm)(_.current.searchTerm)
    val selectedAction = settingsSurfaceSelectedAction(runner, items.lift(selectedIndex))
    ResolvedSurfaceContent(
      title = Some("Settings"),
      header =
        Some(breadcrumbHeader(runner.settingsSurfaceBreadcrumbLabels, Option.when(searchTerm.nonEmpty)(searchTerm))),
      rows = rows,
      footer = runner.statusMessage
        .map(OverlayRow(_))
        .orElse(
          Some(
            OverlayRow(
              s"Navigate • $selectedAction • Back • Dismiss • ${selectedIndex + 1}/${items.length.max(1)}"
            )
          )
        )
    )

  /** Renders `SettingsSurfaceState.previewRows`' capped child labels as indented, de-emphasized rows, with a trailing
    * "+N more" row when there are more children than fit. `leadingPadding` indents the row at render time
    * (`TextOverlayRenderer`); `OverlayTone.Muted` de-emphasizes it. Never selectable -- purely derived display, no new
    * state.
    */
  private def groupPreviewRows(preview: SettingsSurfaceState.PreviewRows): List[OverlayRow] =
    val labelRows   = preview.rows.map(label => previewRow(label))
    val overflowRow = Option.when(preview.overflowCount > 0)(previewRow(s"+${preview.overflowCount} more"))
    labelRows ++ overflowRow.toList

  private def previewRow(label: String): OverlayRow =
    OverlayRow(
      plainText = s"  $label",
      leadingPadding = 2,
      segments = List(OverlaySegment(label, tone = OverlayTone.Muted))
    )

  private def settingsSurfaceSelectedAction(
    runner: com.serenity.command.CommandRunner,
    selectedItem: Option[CommandSurfaceItem]
  ): String =
    selectedItem match
      case Some(_: CommandSurfaceItem.GroupItem) | Some(_: CommandSurfaceItem.SettingSearchItem) => "Open"
      case Some(_: CommandSurfaceItem.OptionItem)                                                => "Apply"
      case Some(item: CommandSurfaceItem.InputItem) =>
        if runner.activeSettingsSurface.exists(_.current.editingItemId.contains(item.id)) then "Save" else "Edit"
      case Some(_: CommandSurfaceItem.CommandItem) => "Run"
      case None                                    => "Select"

  private def commandPaletteFooter(runner: com.serenity.command.CommandRunner, itemCount: Int): String =
    val submitAction = runner.selectedItem match
      case Some(_: CommandSurfaceItem.GroupItem) | Some(_: CommandSurfaceItem.SettingSearchItem) => "Enter open"
      case _                                                                                     => "Enter run"
    // Category tabs are retired (issue #931): no more "Tab categories" hint -- search is the only navigation mode.
    List("↑↓ navigate", submitAction, "Esc dismiss", s"${runner.selectedIndex + 1}/$itemCount")
      .mkString(" • ")

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

  // issue #931: the tab-row renderer this fed (`categoryTabs`) is retired along with the category-switcher UI
  // itself. `categoryLabel` survives -- it now only labels a search result's quiet inline category tag
  // (`commandRow`'s `prefix`), not a clickable/switchable tab.
  private def categoryLabel(category: CommandCategory): String =
    category match
      case CommandCategory.All      => "All"
      case CommandCategory.File     => "File"
      case CommandCategory.View     => "View"
      case CommandCategory.Edit     => "Edit"
      case CommandCategory.Project  => "Project"
      case CommandCategory.Settings => "Settings"

  private def commandRow(
    command: com.serenity.command.Command,
    selected: Boolean,
    prefix: String = "",
    binding: Option[String]
  ): OverlayRow =
    val label = s"$prefix${command.label}"
    OverlayRow(
      plainText = (List(label) ++ binding.toList :+ command.description).mkString(" - "),
      selected = selected,
      segments = OverlaySegment(label, fontFamily = fontFamilyForCommand(command)) ::
        OverlaySegment(command.description, tone = OverlayTone.Muted) ::
        binding.map(value => OverlaySegment(value, tone = OverlayTone.Normal)).toList,
      layout = OverlayRowLayout.Columns
    )

  private def fontFamilyForCommand(command: com.serenity.command.Command): Option[String] =
    command.intent match
      case com.serenity.command.CommandIntent.Settings(SettingsIntent.Font(FontIntent.SetCodeFontFamily(family))) =>
        Some(family)
      case com.serenity.command.CommandIntent.Settings(SettingsIntent.Font(FontIntent.SetTextFontFamily(family))) =>
        Some(family)
      case com.serenity.command.CommandIntent.Settings(SettingsIntent.Font(FontIntent.SetUiFontFamily(family))) =>
        Some(family)
      case _ => None

  private def settingSearchRow(item: CommandSurfaceItem.SettingSearchItem, selected: Boolean): OverlayRow =
    OverlayRow(
      plainText = item.label,
      selected = selected,
      segments = List(
        OverlaySegment(item.label),
        OverlaySegment(item.effectiveValue.getOrElse(""), tone = OverlayTone.Normal),
        OverlaySegment(item.sourceScope, tone = OverlayTone.Normal),
        OverlaySegment(item.breadcrumb, tone = OverlayTone.Normal)
      ).filterNot(_.text.isEmpty),
      layout = OverlayRowLayout.Columns
    )

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

  private def resolveComments(
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
      case SurfaceLayoutKind.Vertical | SurfaceLayoutKind.Square =>
        symbols.take(math.max(1, rect.height - 2)).map { symbol =>
          val active = activeLocation.contains(symbol.location)
          val prefix = if active then "> " else ""
          OverlayRow(s"$prefix${symbol.name}", selected = active)
        }
      case SurfaceLayoutKind.Compact =>
        val current = activeLocation.flatMap(location => symbols.find(_.location == location)).map(_.name)
        current match
          case Some(name) => List(OverlayRow(s"${symbols.length} comments", selected = true), OverlayRow(name))
          case None       => List(OverlayRow(s"${symbols.length} comments"))

    ResolvedSurfaceContent(titleFor(mode, "comments"), rows = shaped)

  private def resolveDiagnostics(
    rect: LayoutRect,
    mode: SurfaceRenderMode,
    issues: List[com.serenity.ui.layout.Diagnostic],
    activeLocation: Option[Location]
  ): ResolvedSurfaceContent =
    val errorCount   = issues.count(_.severity == com.serenity.ui.layout.DiagnosticSeverity.Error)
    val warningCount = issues.count(_.severity == com.serenity.ui.layout.DiagnosticSeverity.Warning)
    val infoCount = issues.count(issue =>
      issue.severity == com.serenity.ui.layout.DiagnosticSeverity.Info ||
        issue.severity == com.serenity.ui.layout.DiagnosticSeverity.Hint
    )
    val shaped = SurfaceLayoutKind.classify(rect) match
      case SurfaceLayoutKind.Horizontal =>
        List(OverlayRow(s"$errorCount error | $warningCount warning | $infoCount info"))
      case SurfaceLayoutKind.Vertical =>
        issues.take(math.max(1, rect.height - 2)).map { issue =>
          OverlayRow(
            s"${issue.severity}: ${issue.message}",
            selected = activeLocation.contains(issue.location)
          )
        }
      case SurfaceLayoutKind.Square =>
        OverlayRow(s"$errorCount error, $warningCount warning") ::
          issues.take(math.max(0, rect.height - 3)).map { issue =>
            OverlayRow(issue.message, selected = activeLocation.contains(issue.location))
          }
      case SurfaceLayoutKind.Compact =>
        List(OverlayRow(s"${issues.length} issues"), OverlayRow(s"$errorCount error"))

    ResolvedSurfaceContent(titleFor(mode, "diagnostics"), rows = shaped)

  private def resolveThemePicker(
    state: ThemePickerState,
    rect: LayoutRect,
    mode: SurfaceRenderMode
  ): ResolvedSurfaceContent =
    val itemWindow = SurfaceFrameLayout(rect).itemWindow(
      itemCount = state.themes.size,
      selectedIndex = state.selectedIndex,
      hasHeader = false,
      hasFooter = false
    )
    val adjustedSelectedIndex = itemWindow.adjustedSelectedIndex(state.selectedIndex)
    val rows = itemWindow.slice(state.themes).zipWithIndex.map { (name, idx) =>
      OverlayRow(plainText = name, selected = idx == adjustedSelectedIndex)
    }
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
    if Theme.luminance(color) > Theme.EqualContrastLuminanceThreshold then java.awt.Color.BLACK
    else java.awt.Color.WHITE

  private def resolveFileSearch(
    state: FileSearchState,
    rect: LayoutRect,
    mode: SurfaceRenderMode
  ): ResolvedSurfaceContent =
    val headerRow = OverlayRow(
      plainText = if state.query.isEmpty then " " else state.query,
      cursorColumn = Some(state.query.length)
    )
    val itemWindow = SurfaceFrameLayout(rect).itemWindow(
      itemCount = state.results.size,
      selectedIndex = state.selectedIndex,
      hasHeader = true,
      hasFooter = state.hasMoreResults
    )
    val adjustedSelectedIndex = itemWindow.adjustedSelectedIndex(state.selectedIndex)
    val resultRows = itemWindow.slice(state.results).zipWithIndex.map { (result, idx) =>
      OverlayRow(
        plainText = s"${result.bufferName}:${result.line + 1}  ${result.lineContent}",
        selected = idx == adjustedSelectedIndex
      )
    }
    ResolvedSurfaceContent(
      title = titleFor(mode, "Search"),
      header = Some(headerRow),
      rows = resultRows,
      footer = Option.when(state.hasMoreResults)(OverlayRow(s"${state.results.length} loaded, more available"))
    )

  def resolveContextualToolbar(
    toolbarState: ContextualToolbarState,
    state: AppState,
    rect: LayoutRect,
    @unused mode: SurfaceRenderMode
  ): ResolvedSurfaceContent =
    val borderCells = SurfaceFrameLayout.borderCellsFor(SurfaceContent.ContextualToolbar(toolbarState))
    val contentRect = SurfaceFrameLayout(rect, borderCells).contentRect
    val items       = ContextualToolbar.itemsFor(state)
    val normalized  = toolbarState.normalized(items)
    val rowGroups   = ContextualToolbarLayout.rowGroups(items, contentRect.width.max(1), normalized.displayMode)
    val focused     = normalized.focusedIndex
    val iconFont    = FontLoader.toolbarIconFontFamily
    val topRows = rowGroups
      .foldLeft((0, List.empty[OverlayRow])) {
        case ((offset, acc), rowItems) =>
          val cellWidths =
            ContextualToolbarLayout.itemCellWidths(rowItems, contentRect.width.max(1), normalized.displayMode)
          val leadingPadding = ContextualToolbarLayout.rowLeadingPadding(
            rowItems,
            contentRect.width.max(1),
            normalized.displayMode
          )
          val segments = rowItems.zip(cellWidths).zipWithIndex.map {
            case ((item, cellWidth), index) =>
              val selected          = isSelected(item) || offset + index == focused
              val trailingSeparator = ContextualToolbar.hasTrailingGroupSeparator(item, rowItems.lift(index + 1))
              normalized.displayMode match
                case ToolbarDisplayMode.IconOnly if iconFont.nonEmpty =>
                  OverlaySegment(
                    item.icon,
                    selected = selected,
                    fontFamily = iconFont,
                    trailingSeparator = trailingSeparator,
                    allocatedWidth = Some(cellWidth)
                  )
                case ToolbarDisplayMode.IconAndText if iconFont.nonEmpty =>
                  OverlaySegment(
                    ContextualToolbar.displayText(item, ToolbarDisplayMode.TextOnly),
                    selected = selected,
                    inlineIcon = Some(item.icon),
                    inlineIconFontFamily = iconFont,
                    trailingSeparator = trailingSeparator,
                    allocatedWidth = Some(cellWidth)
                  )
                case _ =>
                  OverlaySegment(
                    ContextualToolbar.displayText(item, ToolbarDisplayMode.TextOnly),
                    selected = selected,
                    trailingSeparator = trailingSeparator,
                    allocatedWidth = Some(cellWidth)
                  )
          }
          (
            offset + rowItems.length,
            acc :+ OverlayRow(
              plainText = segments.map(_.text).mkString(" "),
              segments = segments,
              layout = OverlayRowLayout.Distributed,
              leadingPadding = leadingPadding
            )
          )
      }
      ._2

    val dropdownDetailRows = ContextualToolbarLayout.detailRowGroups(normalized, items, contentRect.width.max(1))
    val detailRows =
      if dropdownDetailRows.nonEmpty then
        val selectedIndex = normalized.detailState.collect {
          case ContextualToolbarDetailState.Dropdown(_, index) => index
        }
        dropdownDetailRows
          .foldLeft((0, List.empty[OverlayRow])) {
            case ((offset, acc), rowOptions) =>
              val segments = rowOptions.zipWithIndex.map {
                case (option, index) =>
                  OverlaySegment(option.label, selected = selectedIndex.contains(offset + index))
              }
              (
                offset + rowOptions.length,
                acc :+ OverlayRow(
                  plainText = segments.map(_.text).mkString(" "),
                  segments = segments,
                  layout = OverlayRowLayout.Distributed
                )
              )
          }
          ._2
      else
        ContextualToolbar
          .detailInputItem(normalized, items)
          .map {
            case (item, text) =>
              inputRow(item.inputItem, selected = true, editingText = Some(text))
          }
          .toList

    ResolvedSurfaceContent(rows = topRows ++ detailRows)

  private def isSelected(item: ContextualToolbarItem): Boolean =
    item match
      case ContextualToolbarItem.Button(_, _, _, _, selected) => selected
      case _                                                  => false

  private def resolveContextMenu(
    menu: ContextMenu,
    rect: LayoutRect,
    mode: SurfaceRenderMode,
    itemGapRows: Double
  ): ResolvedSurfaceContent =
    val itemWindow = SurfaceFrameLayout(rect).itemWindow(
      itemCount = menu.items.size,
      selectedIndex = menu.selectedIndex,
      hasHeader = true,
      hasFooter = menu.items.nonEmpty,
      itemGapRows = itemGapRows
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
