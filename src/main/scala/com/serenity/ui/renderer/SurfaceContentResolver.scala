package com.serenity.ui.renderer

import com.serenity.command.{CommandCategory, CommandRegistry, CommandSurfaceItem}
import com.serenity.state.models.{CloseWorkflowChoice, CloseWorkflowState, FileWorkflowField, FileWorkflowMode, FileWorkflowState, Modal, ReplaceWorkflowField, ReplaceWorkflowState, SurfaceContent}
import com.serenity.ui.layout.LayoutRect
import com.googlecode.lanterna.TextColor
import com.serenity.ui.layout.SurfaceLayoutKind

enum SurfaceRenderMode:
  case Floating
  case Pinned

enum OverlayTone:
  case Normal
  case Muted

enum OverlayRowLayout:
  case Plain
  case Distributed
  case Split

case class OverlaySegment(
    text: String,
    selected: Boolean = false,
    tone: OverlayTone = OverlayTone.Normal,
    foregroundColor: Option[TextColor] = None,
    backgroundColor: Option[TextColor] = None
)

case class OverlayRow(
    plainText: String,
    selected: Boolean = false,
    cursorColumn: Option[Int] = None,
    foregroundColor: Option[TextColor] = None,
    backgroundColor: Option[TextColor] = None,
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
        resolveDirectoryListing(rect, mode, path.getFileName.toString, entries.map(_.name), selectedPath.flatMap(p => Option(p.getFileName).map(_.toString)))
      case SurfaceContent.CommandPalette(runner) =>
        resolveCommandPalette(runner, rect, mode)
      case SurfaceContent.ModalWorkflow(modal) =>
        resolveModalWorkflow(modal, rect, mode)
      case SurfaceContent.Terminal(buffer, cursor) =>
        resolveTerminal(rect, mode, buffer, cursor)
      case SurfaceContent.Outline(symbols) =>
        resolveOutline(rect, mode, symbols.map(symbol => (symbol.kind.toString, symbol.name)))
      case SurfaceContent.Diagnostics(issues) =>
        resolveDiagnostics(rect, mode, issues)

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
      case Modal.GotoLine(input)      => List("goto-line", input)
      case Modal.Find(query, _, _)    => List("find", query)
      case Modal.FileWorkflow(workflow) =>
        List("file", workflow.filename, workflow.path)
      case Modal.ReplaceWorkflow(workflow) =>
        List("replace", workflow.findText, workflow.replacementText)
      case Modal.CloseWorkflow(workflow) =>
        List("unsaved changes", workflow.currentBufferLabel)
      case Modal.Custom(name, input)  => List(name, input)

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

    ResolvedSurfaceContent(
      title = titleFor(mode, "replace"),
      header = Some(OverlayRow("replace")),
      rows = List(findRow, replaceRow),
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
    val operationLabel =
      workflow.mode match
        case FileWorkflowMode.Open   => "open"
        case FileWorkflowMode.SaveAs => "save-as"

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
              foregroundColor = if isMissing then Some(TextColor.ANSI.RED) else None
            )
          }

    val pathRow = OverlayRow(
      plainText = s"Path ${workflow.path}",
      selected = workflow.activeField == FileWorkflowField.Path,
      segments = OverlaySegment("Path") :: pathSegments,
      layout = OverlayRowLayout.Split
    )

    val suggestionRows = workflow.suggestions.zipWithIndex.map { case (suggestion, index) =>
      val suffix = if suggestion.isDirectory then "/" else ""
      OverlayRow(
        plainText = suggestion.value + suffix,
        selected = index == workflow.selectedSuggestionIndex
      )
    }

    val footer =
      workflow.statusMessage.map(OverlayRow(_))
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
        if runner.searchTerm.isEmpty then
          Some(categoryTabs(runner.activeCategory))
        else
          Some(OverlayRow(
            plainText = s"search: ${runner.searchTerm}",
            cursorColumn = Some(s"search: ${runner.searchTerm}".length)
          ))

      val visibleItems = runner.visibleItems
      val rows = visibleItems.zipWithIndex.map {
        case (CommandSurfaceItem.CommandItem(command), index) =>
          val prefix =
            if runner.searchTerm.isEmpty then ""
            else s"[${categoryLabel(command.category)}] "
          OverlayRow(
            plainText = s"$prefix${command.name} - ${command.description}",
            selected = index == runner.selectedIndex
          )
        case (option: CommandSurfaceItem.OptionItem, index) =>
          optionRow(option, index == runner.selectedIndex)
      }
      val footer =
        if visibleItems.nonEmpty then Some(OverlayRow(s"${runner.selectedIndex + 1}/${visibleItems.length}"))
        else None

      ResolvedSurfaceContent(
        title = titleFor(mode, "commands"),
        header = header,
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
      option.hint.toList.map(hint => OverlaySegment(hint, tone = OverlayTone.Muted)) :+
        OverlaySegment(option.selectedOption, selected = true)

    OverlayRow(
      plainText = s"${option.label}: ${option.hint.map(_ + " ").getOrElse("")}${option.selectedOption}",
      selected = selected,
      segments = OverlaySegment(option.label) :: rightSegments,
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
    val errorCount = issues.count(_.severity == com.serenity.ui.layout.DiagnosticSeverity.Error)
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
