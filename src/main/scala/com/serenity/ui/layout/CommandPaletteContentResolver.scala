package com.serenity.ui.layout

import com.serenity.command.{
  CommandCategory,
  CommandSurfaceItem,
  FontIntent,
  SettingsIntent,
  SettingsPage,
  SettingsSurfaceState
}
import com.serenity.state.models.*

/** Resolves `SurfaceContent.CommandPalette` -- both the flat search/run palette and the settings surface it also hosts
  * -- into overlay rows. Split out of `SurfaceContentResolver` to keep that file's dispatcher readable -- see the doc
  * comment there.
  */
private[layout] object CommandPaletteContentResolver:

  def resolveCommandPalette(
    runner: com.serenity.command.CommandRunner,
    rect: LayoutRect,
    mode: SurfaceRenderMode,
    itemGapRows: Double,
    itemTargetRows: Int,
    showKeyHints: Boolean
  ): ResolvedSurfaceContent =
    // Dispatch on `CommandRunnerSurface` (issue #931, Stage 2) rather than `isSettingsSurface`/
    // `activeSettingsSurface.isDefined` directly -- `Settings(_)` covers both entry points exactly as those two
    // conditions did (issue #1059: "one consistent settings experience regardless of entry point"), and is checked
    // first regardless of `isActive`, preserving this resolver's existing precedence (a settings-mode runner that
    // has since been deactivated still renders through `resolveSettingsSurface`, not the inactive placeholder below).
    runner.surface match
      case _: com.serenity.command.CommandRunnerSurface.Settings =>
        resolveSettingsSurface(runner, rect, itemGapRows, itemTargetRows, showKeyHints)
      case com.serenity.command.CommandRunnerSurface.Palette(_) if !runner.isActive =>
        ResolvedSurfaceContent(mode.titleFor("commands"))
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
        // The persistent key-hint row (issue #931, Stage 3) only ever shows when there is a list to navigate --
        // mirrors the existing dynamic footer's own `allItems.nonEmpty` gate.
        val hasKeyHint = showKeyHints && allItems.nonEmpty
        val itemWindow = SurfaceFrameLayout
          .forContent(rect, SurfaceContent.CommandPalette(runner))
          .itemWindow(
            itemCount = allItems.size,
            selectedIndex = runner.selectedIndex,
            hasHeader = true,
            hasFooter = allItems.nonEmpty || runner.statusMessage.nonEmpty,
            reservedContentRows = groupPreview.size,
            itemGapRows = itemGapRows,
            itemTargetRows = itemTargetRows,
            hasKeyHint = hasKeyHint
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
        // With the persistent row on, `footer` reverts to being purely the transient status-message slot -- the
        // dynamic hint text moves to `keyHintRow` and is no longer suppressed while a status message shows (issue
        // #931, Stage 3). With it off, `footer` keeps doing both jobs exactly as before.
        val footer =
          if showKeyHints then runner.statusMessage.map(OverlayRow(_))
          else
            runner.statusMessage
              .map(OverlayRow(_))
              .orElse(
                Option.when(allItems.nonEmpty)(
                  OverlayRow(commandPaletteFooter(runner, allItems.length))
                )
              )

        ResolvedSurfaceContent(
          title = mode.titleFor("commands"),
          header = header,
          rows = rows,
          footer = footer,
          keyHintRow = Option.when(hasKeyHint)(OverlayRow(paletteKeyHintText))
        )

  def resolveSettingsSurface(
    runner: com.serenity.command.CommandRunner,
    rect: LayoutRect,
    itemGapRows: Double,
    itemTargetRows: Int,
    showKeyHints: Boolean
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
        itemTargetRows = itemTargetRows,
        hasKeyHint = showKeyHints
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
    // Same footer/keyHintRow split as the palette above: with the persistent row on, `footer` is status-message-only
    // and the always-current "Navigate • ... • Back • Dismiss" hint moves to `keyHintRow` (issue #931, Stage 3).
    val footer =
      if showKeyHints then runner.statusMessage.map(OverlayRow(_))
      else
        runner.statusMessage
          .map(OverlayRow(_))
          .orElse(
            Some(
              OverlayRow(
                s"Navigate • $selectedAction • Back • Dismiss • ${selectedIndex + 1}/${items.length.max(1)}"
              )
            )
          )
    ResolvedSurfaceContent(
      title = Some("Settings"),
      header =
        Some(breadcrumbHeader(runner.settingsSurfaceBreadcrumbLabels, Option.when(searchTerm.nonEmpty)(searchTerm))),
      rows = rows,
      footer = footer,
      keyHintRow = Option.when(showKeyHints)(OverlayRow(settingsSurfaceKeyHintText(runner)))
    )

  /** Static key-hint text for the palette (issue #931, Stage 3) -- the palette's key semantics don't vary by selection
    * the way the settings surface's do, so unlike `settingsSurfaceKeyHintText` this needs no runner state.
    */
  private def paletteKeyHintText: String =
    "↑↓ navigate • Enter run • Esc dismiss"

  /** Key-hint text for whichever settings-surface state is actually active, matching the real reducer semantics
    * (`CommandRunnerReducer`) post Stage 1/2 rather than the dynamic footer's transient per-selection action word:
    *   - recording a keybinding: only Escape does anything (cancels the recording)
    *   - editing a value's text: typing edits it, Enter saves, Escape cancels the edit without navigating
    *   - browsing a group's rows: arrow keys navigate, Enter opens/runs the selected row, Escape backs out one level,
    *     and Left/Right cycle an `OptionItem` row's value in place
    */
  private def settingsSurfaceKeyHintText(runner: com.serenity.command.CommandRunner): String =
    runner.activeSettingsSurface.map(_.current) match
      case Some(editing: SettingsPage.Editing) if editing.recording.nonEmpty =>
        "Esc cancel"
      case Some(_: SettingsPage.Editing) =>
        "Type to edit • Enter save • Esc cancel"
      case _ =>
        "↑↓ navigate • Enter open • Esc back • ←→ cycle option"

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

  /** Renders an `InputItem` (label, hint, and current/edited value) as a single overlay row. Shared with
    * `ContextualToolbarContentResolver`, which uses it verbatim for the toolbar's own dropdown input detail row -- both
    * resolvers need the same label/hint/value/cursor layout for an editable field, so this is that one definition
    * rather than two that could drift.
    */
  def inputRow(
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
