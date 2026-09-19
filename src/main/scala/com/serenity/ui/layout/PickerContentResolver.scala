package com.serenity.ui.layout

import com.serenity.state.models.*
import com.serenity.ui.theme.Theme

/** Resolves the windowed single-selection pickers -- the theme picker/creator and fuzzy file search -- into overlay
  * rows. Split out of `SurfaceContentResolver` to keep that file's dispatcher readable -- see the doc comment there.
  * The generic context menu's own resolver, `resolveContextMenu`, was retired here (issue #819, slice 2): it now
  * paints entirely through `ContextMenuSurfaceComposition`.
  */
private[layout] object PickerContentResolver:

  def resolveThemePicker(
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
    ResolvedSurfaceContent(SurfaceContentResolver.titleFor(mode, "Theme"), rows = rows)

  def resolveThemeCreator(
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
      title = SurfaceContentResolver.titleFor(mode, "Theme Creator"),
      header = Some(OverlayRow("theme creator")),
      rows = itemWindow.slice(allRows),
      footer = state.statusMessage.map(OverlayRow(_, foregroundColor = Some(java.awt.Color.RED)))
    )

  private def contrastColor(color: java.awt.Color): java.awt.Color =
    if Theme.luminance(color) > Theme.EqualContrastLuminanceThreshold then java.awt.Color.BLACK
    else java.awt.Color.WHITE

  def resolveFileSearch(
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
      title = SurfaceContentResolver.titleFor(mode, "Search"),
      header = Some(headerRow),
      rows = resultRows,
      footer = Option.when(state.hasMoreResults)(OverlayRow(s"${state.results.length} loaded, more available"))
    )
