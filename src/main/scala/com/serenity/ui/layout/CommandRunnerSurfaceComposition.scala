package com.serenity.ui.layout

import com.serenity.command.*
import com.serenity.config.InterfaceDensityMetrics
import com.serenity.state.models.{AppState, SurfaceContent}

/** Declarative composition plan for the command runner (issue #819, slice 2: command runner and submenu) -- the palette
  * and the settings surface it also hosts (issue #1059), resolved into one paint/hit-test/focus plan the same way
  * `ModalSurfaceComposition` and `ContextMenuSurfaceComposition` already do for the other migrated surfaces.
  *
  * Row *content* (segments, columns, editing state) stays sourced from `CommandPaletteContentResolver`'s existing,
  * separately-tested row builders -- this object owns row *position* and *hit-testing* only, built directly on
  * `SurfaceFrameLayout.contentRowSlotsFor`/`itemWindow`, exactly mirroring the row-count and windowing arguments
  * `CommandPaletteContentResolver` itself uses for painting. That parity matters: the window's absolute item index is
  * computed once per row here, at the same place its position is assigned, so a row after another item's
  * expand-in-place group preview (issue #1059) can no longer be mis-addressed the way a row-slot-index-based lookup
  * computed separately from the window could drift.
  *
  * Scope note: this composition's `hitAt` covers only the cell/keyboard-coordinate hit-test path. The command runner's
  * sub-cell fractional-pixel hover/click path (`FloatingSurfaceGeometry`, exercised by `CommandRunnerMouseSpec`'s
  * "fractional floating pixel offset" tests) is a distinct, already-tested feature this migration does not touch --
  * callers keep using `MouseHitTestGeometry.overlayItemIndex` for that case.
  */
object CommandRunnerSurfaceComposition:

  private val FocusIdPrefix = "command-runner-item-"

  def focusId(absoluteIndex: Int): SurfaceFocusId = SurfaceFocusId(s"$FocusIdPrefix$absoluteIndex")

  /** Recovers the absolute item index a `hitAt` hit's focus id addresses, the inverse of `focusId`. */
  def absoluteIndexOf(id: SurfaceFocusId): Option[Int] = id.value.stripPrefix(FocusIdPrefix).toIntOption

  /** The command runner's frame height: the palette and the settings surface (submenu) it also hosts size identically
    * today -- both cap at [[FloatingSurfaceLayout.commandSurfaceMaxHeight]] and floor at the density's own
    * `commandSurfaceMinHeight` -- so this one method, not a `CommandRunnerSurface.Palette`/`.Settings` match, covers
    * both, mirroring `ModalSurfaceComposition.frameHeight`'s role for blocking workflows.
    */
  def frameHeight(state: AppState, maxHeight: Int, roomOnPreferredSide: Int = Int.MaxValue): Int =
    val densityMetrics   = InterfaceDensityMetrics.forDensity(state.persisted.config.interfaceDensity)
    val commandMaxHeight = FloatingSurfaceLayout.commandSurfaceMaxHeight(state, maxHeight, roomOnPreferredSide)
    math.min(commandMaxHeight, math.max(densityMetrics.commandSurfaceMinHeight, maxHeight - 1))

  def forRunner(
    runner: CommandRunner,
    frameRect: LayoutRect,
    itemGapRows: Double,
    itemTargetRows: Int,
    showKeyHints: Boolean
  ): ResolvedSurfaceComposition =
    runner.surface match
      case _: CommandRunnerSurface.Settings =>
        forSettingsSurface(runner, frameRect, itemGapRows, itemTargetRows, showKeyHints)
      case CommandRunnerSurface.Palette(_) =>
        forPalette(runner, frameRect, itemGapRows, itemTargetRows, showKeyHints)

  private def forPalette(
    runner: CommandRunner,
    frameRect: LayoutRect,
    itemGapRows: Double,
    itemTargetRows: Int,
    showKeyHints: Boolean
  ): ResolvedSurfaceComposition =
    val frameLayout      = SurfaceFrameLayout.forContent(frameRect, SurfaceContent.CommandPalette(runner))
    val content          = frameLayout.contentRect
    val allItems         = runner.visibleItems
    val preview          = SettingsSurfaceState.previewRows(allItems, runner.selectedIndex)
    val groupPreviewFull = CommandPaletteContentResolver.groupPreviewRows(preview)
    val hasKeyHint       = showKeyHints && allItems.nonEmpty
    val hasFooter        = allItems.nonEmpty || runner.statusMessage.nonEmpty
    // Capped to the same claim `itemWindow` itself enforces (issue #1548) -- otherwise the rows rendered here
    // (windowItems.size + groupPreview.size) would exceed the frame's actual row budget once itemWindow's own cap
    // kicks in, since these two computations must agree on how many rows the preview is allowed.
    val groupPreview =
      groupPreviewFull.take(frameLayout.cappedReservedContentRows(allItems.size, groupPreviewFull.size))

    val itemWindow = frameLayout.itemWindow(
      itemCount = allItems.size,
      selectedIndex = runner.selectedIndex,
      hasHeader = true,
      hasFooter = hasFooter,
      reservedContentRows = groupPreview.size,
      itemGapRows = itemGapRows,
      itemTargetRows = itemTargetRows,
      hasKeyHint = hasKeyHint
    )
    val windowItems           = itemWindow.slice(allItems)
    val adjustedSelectedIndex = itemWindow.adjustedSelectedIndex(runner.selectedIndex)

    val entries: List[(OverlayRow, Option[Int])] = windowItems.zipWithIndex.flatMap {
      case (item, index) =>
        val selected      = index == adjustedSelectedIndex
        val absoluteIndex = index + itemWindow.offset
        val row           = paletteRow(runner, item, selected)
        if selected then (row, Some(absoluteIndex)) :: groupPreview.map(previewRow => (previewRow, None))
        else List((row, Some(absoluteIndex)))
    }

    val headerText = s"search: ${runner.searchTerm}"
    val header     = OverlayRow(plainText = headerText, cursorColumn = Some(headerText.length))
    val footer =
      if showKeyHints then runner.statusMessage
      else
        runner.statusMessage.orElse(
          Option.when(allItems.nonEmpty)(CommandPaletteContentResolver.commandPaletteFooter(runner, allItems.length))
        )
    val keyHint = Option.when(hasKeyHint)(CommandPaletteContentResolver.paletteKeyHintText)

    assemble(
      content,
      entries,
      Some(header),
      footer.map(OverlayRow(_)),
      keyHint.map(OverlayRow(_)),
      itemGapRows,
      itemTargetRows,
      hasFooter,
      hasKeyHint
    )

  private def forSettingsSurface(
    runner: CommandRunner,
    frameRect: LayoutRect,
    itemGapRows: Double,
    itemTargetRows: Int,
    showKeyHints: Boolean
  ): ResolvedSurfaceComposition =
    val frameLayout      = SurfaceFrameLayout.forContent(frameRect, SurfaceContent.CommandPalette(runner))
    val content          = frameLayout.contentRect
    val items            = runner.settingsSurfaceItems
    val selectedIndex    = runner.settingsSurfaceSelectedIndex
    val preview          = SettingsSurfaceState.previewRows(items, selectedIndex)
    val groupPreviewFull = CommandPaletteContentResolver.groupPreviewRows(preview)
    // Capped to the same claim `itemWindow` itself enforces -- see `forPalette`'s identical note (issue #1548).
    val groupPreview = groupPreviewFull.take(frameLayout.cappedReservedContentRows(items.size, groupPreviewFull.size))

    val itemWindow = frameLayout.itemWindow(
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

    val entries: List[(OverlayRow, Option[Int])] = itemWindow.slice(items).zipWithIndex.flatMap {
      case (item, index) =>
        val selected      = index == adjustedSelectedIndex
        val absoluteIndex = index + itemWindow.offset
        val row           = settingsRow(runner, item, selected)
        if selected then (row, Some(absoluteIndex)) :: groupPreview.map(previewRow => (previewRow, None))
        else List((row, Some(absoluteIndex)))
    }

    val searchTerm = runner.activeSettingsSurface.fold(runner.searchTerm)(_.current.searchTerm)
    val header = CommandPaletteContentResolver.breadcrumbHeader(
      runner.settingsSurfaceBreadcrumbLabels,
      Option.when(searchTerm.nonEmpty)(searchTerm)
    )
    val selectedAction = CommandPaletteContentResolver.settingsSurfaceSelectedAction(runner, items.lift(selectedIndex))
    val footer =
      if showKeyHints then runner.statusMessage
      else
        runner.statusMessage.orElse(
          Some(KeyHintVocabulary.footer(selectedAction.toLowerCase, "back", selectedIndex + 1, items.length))
        )
    val keyHint = Option.when(showKeyHints)(CommandPaletteContentResolver.settingsSurfaceKeyHintText(runner))

    assemble(
      content,
      entries,
      Some(header),
      footer.map(OverlayRow(_)),
      keyHint.map(OverlayRow(_)),
      itemGapRows,
      itemTargetRows,
      hasFooter = true,
      showKeyHints
    )

  private def paletteRow(runner: CommandRunner, item: CommandSurfaceItem, selected: Boolean): OverlayRow =
    item match
      case CommandSurfaceItem.CommandItem(command) =>
        val prefix =
          if runner.searchTerm.isEmpty then ""
          else s"[${CommandPaletteContentResolver.categoryLabel(command.category)}] "
        CommandPaletteContentResolver.commandRow(command, selected, prefix, runner.bindingFor(command))
      case option: CommandSurfaceItem.OptionItem =>
        CommandPaletteContentResolver.optionRow(option, selected)
      case toggle: CommandSurfaceItem.ToggleItem =>
        CommandPaletteContentResolver.toggleRow(toggle, runner.effectiveChecked(toggle), selected)
      case item: CommandSurfaceItem.InputItem =>
        val editingText = if runner.editingItemId.contains(item.id) then Some(runner.editingText) else None
        CommandPaletteContentResolver.inputRow(item, selected, editingText)
      case item: CommandSurfaceItem.SettingSearchItem =>
        CommandPaletteContentResolver.settingSearchRow(item, selected)
      case group: CommandSurfaceItem.GroupItem =>
        val groupLabel =
          if runner.searchTerm.nonEmpty then runner.settingsGroupBreadcrumbLabels(group.id).mkString(" > ")
          else group.label
        CommandPaletteContentResolver.groupRow(groupLabel, group.hint, selected)

  private def settingsRow(runner: CommandRunner, item: CommandSurfaceItem, selected: Boolean): OverlayRow =
    item match
      case CommandSurfaceItem.CommandItem(command) =>
        CommandPaletteContentResolver.commandRow(command, selected, binding = runner.bindingFor(command))
      case option: CommandSurfaceItem.OptionItem =>
        CommandPaletteContentResolver.optionRow(option, selected)
      case toggle: CommandSurfaceItem.ToggleItem =>
        CommandPaletteContentResolver.toggleRow(toggle, runner.effectiveChecked(toggle), selected)
      case item: CommandSurfaceItem.InputItem =>
        val editingText =
          runner.activeSettingsSurface.filter(_.current.editingItemId.contains(item.id)).map(_.current.draftText)
        CommandPaletteContentResolver.inputRow(item, selected, editingText)
      case item: CommandSurfaceItem.SettingSearchItem =>
        CommandPaletteContentResolver.settingSearchRow(item, selected)
      case group: CommandSurfaceItem.GroupItem =>
        CommandPaletteContentResolver.groupRow(group.label, group.hint, selected)

  private def assemble(
    content: LayoutRect,
    entries: List[(OverlayRow, Option[Int])],
    header: Option[OverlayRow],
    footer: Option[OverlayRow],
    keyHint: Option[OverlayRow],
    itemGapRows: Double,
    itemTargetRows: Int,
    hasFooter: Boolean,
    hasKeyHint: Boolean
  ): ResolvedSurfaceComposition =
    val bounds = logicalRect(content.x, content.y, content.width, content.height)
    val slots = SurfaceFrameLayout.contentRowSlotsFor(
      content,
      entries.size,
      hasHeader = header.isDefined,
      hasFooter = hasFooter,
      itemGapRows = itemGapRows,
      itemTargetRows = itemTargetRows,
      hasKeyHint = hasKeyHint
    )

    val headerBoxes = header.toList.flatMap(row =>
      slots.collectFirst {
        case SurfaceContentRowSlot(SurfaceContentRowKind.Header, y) => toBox(row, rowRect(bounds, y - content.y), None)
      }
    )
    val itemBoxes = slots.collect {
      case SurfaceContentRowSlot(SurfaceContentRowKind.Item(index), y) if entries.isDefinedAt(index) =>
        val (row, absoluteIndex) = entries(index)
        toBox(row, rowRect(bounds, y - content.y), absoluteIndex.map(focusId))
    }
    val keyHintBoxes = keyHint.toList.flatMap(row =>
      slots.collectFirst {
        case SurfaceContentRowSlot(SurfaceContentRowKind.KeyHint, y) => toBox(row, rowRect(bounds, y - content.y), None)
      }
    )
    val footerBoxes = footer.toList.flatMap(row =>
      slots.collectFirst {
        case SurfaceContentRowSlot(SurfaceContentRowKind.Footer, y) => toBox(row, rowRect(bounds, y - content.y), None)
      }
    )

    plan(bounds, headerBoxes ++ itemBoxes ++ keyHintBoxes ++ footerBoxes)

  private[layout] def toBox(row: OverlayRow, rect: LogicalPixelRect, id: Option[SurfaceFocusId]): SurfacePaintBox =
    SurfacePaintBox(
      kind = if id.isDefined then SurfacePaintKind.ActionItem else SurfacePaintKind.Text,
      rect = rect,
      text = Some(row.plainText),
      focusId = id,
      actionId = id.map(focusId => SurfaceActionId(focusId.value)),
      semanticLabel = Some(row.plainText),
      selected = row.selected,
      cursorOffset = row.cursorColumn,
      segments = row.segments,
      layout = row.layout match
        case OverlayRowLayout.Plain           => SurfacePaintLayout.Plain
        case OverlayRowLayout.Split           => SurfacePaintLayout.Split
        case OverlayRowLayout.Columns         => SurfacePaintLayout.Columns
        case OverlayRowLayout.Distributed     => SurfacePaintLayout.Distributed
        case OverlayRowLayout.PriorityColumns => SurfacePaintLayout.Plain
    )

  private def plan(bounds: LogicalPixelRect, boxes: List[SurfacePaintBox]): ResolvedSurfaceComposition =
    val clipped = boxes.flatMap(box => box.rect.intersection(bounds).map(rect => box.copy(rect = rect)))
    val hits = clipped.flatMap { box =>
      for
        id    <- box.focusId
        label <- box.semanticLabel
      yield SurfaceHitRegion(box.rect, id, box.actionId, label)
    }
    ResolvedSurfaceComposition(
      bounds = bounds,
      intrinsicSize = SurfaceIntrinsicSize(bounds.width, bounds.height),
      paintBoxes = clipped,
      hitRegions = hits,
      focusOrder = hits.map(_.focusId)
    )

  private def rowRect(bounds: LogicalPixelRect, row: Int): LogicalPixelRect =
    LogicalPixelRect(
      bounds.x,
      bounds.y + row,
      bounds.width,
      math.min(1.0, math.max(0.0, bounds.bottom - bounds.y - row))
    )

  private def logicalRect(x: Int, y: Int, width: Int, height: Int): LogicalPixelRect =
    LogicalPixelRect(x.toDouble, y.toDouble, width.toDouble, height.toDouble)
