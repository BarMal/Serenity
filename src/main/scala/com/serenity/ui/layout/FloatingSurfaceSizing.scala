package com.serenity.ui.layout

import com.serenity.config.AppConfigOps.*
import com.serenity.config.InterfaceDensityMetrics
import com.serenity.state.models.*

/** Intrinsic width/height for a floating surface's frame -- moved out of [[FloatingSurfaceLayout]] (issue #1683) so
  * that object is purely about anchor/placement geometry, not sizing. Every surface asks its own composition for its
  * size where one already exists (`ContextualToolbarLayout`, `CommandRunnerSurfaceComposition`,
  * `ContextMenuSurfaceComposition`, `CommentLensSurfaceComposition`, `ModalSurfaceComposition`), exactly as
  * `FloatingSurfaceLayout` did inline before this move; the remaining content kinds have no bespoke composition
  * (`RowsSurfaceComposition`'s generic one is a post-hoc paint plan built from an already-known rect, not usable here)
  * and size from their own raw data instead.
  *
  * This content-kind knowledge cannot disappear -- a directory listing and a quick-info popup do genuinely want
  * different heights -- but it now lives in exactly one place, asked for by width/height, rather than duplicated
  * between here and a second, independently-maintained formula elsewhere (the drift #1681 was a symptom of).
  */
private[layout] object FloatingSurfaceSizing:

  def width(content: SurfaceContent, state: AppState, contentRect: LayoutRect): Int =
    val borderCells = SurfaceFrameLayout.borderCellsFor(content)
    content match
      case SurfaceContent.ContextualToolbar(toolbarState) =>
        ContextualToolbarLayout.compactContentWidth(
          toolbarState,
          state,
          contentRect.width - (borderCells * 2)
        ) + (borderCells * 2)
      case SurfaceContent.CommandPalette(_) | SurfaceContent.ShortcutsHelp(_) |
          SurfaceContent.ModalWorkflow(Modal.FileWorkflow(_)) =>
        capWidth(contentRect.width)
      // A quiet single row: as wide as its text (plus a cell of padding each side), never the whole pane.
      case SurfaceContent.StatusLine(text) =>
        math.min(contentRect.width, text.length + 2)
      case _ =>
        contentRect.width

  /** The flat width cap every command-surface-like content shares, content-agnostic -- also used as-is by the
    * cursor-peek prototype's own frozen-anchor sizing (`FloatingSurfaceLayout.calculateFrozenCursorPeekRect`), which
    * never varies its width by content kind.
    */
  def capWidth(maxWidth: Int): Int =
    math.min(math.max(0, maxWidth), 72)

  def height(
    content: SurfaceContent,
    maxWidth: Int,
    maxHeight: Int,
    state: AppState,
    roomOnPreferredSide: Int = Int.MaxValue
  ): Int =
    val densityMetrics   = InterfaceDensityMetrics.forDensity(state.persisted.config.interfaceDensity)
    val commandMaxHeight = FloatingSurfaceLayout.commandSurfaceMaxHeight(state, maxHeight, roomOnPreferredSide)
    val preferredHeight = content match
      case SurfaceContent.StartPage(_)            => maxHeight
      case SurfaceContent.QuickInfo(text)         => math.max(3, text.linesIterator.size + 2)
      case SurfaceContent.FilePreview(_, content) => math.max(4, math.min(6, content.linesIterator.take(4).size + 2))
      case SurfaceContent.SymbolDefinition(_, _)  => 4
      case SurfaceContent.StatusLine(_)           => 1
      case SurfaceContent.DirectoryListing(_, entries, _) => math.max(4, math.min(6, entries.take(4).size + 2))
      case SurfaceContent.DirectoryTree(tree, _) =>
        math.max(4, math.min(8, DirectoryTreeData.visibleRows(tree).size + 2))
      case SurfaceContent.CommandPalette(_) =>
        CommandRunnerSurfaceComposition.frameHeight(state, maxHeight, roomOnPreferredSide)
      case SurfaceContent.CommandRunnerPeek(_) =>
        math.min(
          commandMaxHeight,
          math.max(densityMetrics.commandSurfaceMinHeight, maxHeight - 1)
        )
      case SurfaceContent.ThemeCreator(_) =>
        math.min(
          densityMetrics.commandSurfaceMaxHeight,
          math.max(densityMetrics.commandSurfaceMinHeight, maxHeight - 1)
        )
      case SurfaceContent.ContextualToolbar(toolbarState) =>
        val borderCells  = SurfaceFrameLayout.borderCellsFor(content)
        val contentWidth = (maxWidth - (borderCells * 2)).max(1)
        val toolbarRows  = ContextualToolbarLayout.rowCount(toolbarState, state, contentWidth)
        SurfaceFrameLayout.frameHeightForItemRows(
          toolbarRows,
          hasHeader = false,
          hasFooter = false,
          borderCells = borderCells,
          itemGapRows = state.effectiveUiElementGap,
          itemTargetRows = SurfaceFrameLayout.itemTargetRowsFor(content, state.persisted.config.interfaceDensity)
        )
      case SurfaceContent.ContextMenu(menu) =>
        ContextMenuSurfaceComposition.frameHeight(
          menu,
          itemGapRows = state.persisted.config.effectiveCommandRunnerItemGapRows,
          itemTargetRows = SurfaceFrameLayout.itemTargetRowsFor(content, state.persisted.config.interfaceDensity)
        )
      case SurfaceContent.CommentLens(lens) =>
        CommentLensSurfaceComposition.frameHeight(lens)
      case SurfaceContent.ModalWorkflow(modal) =>
        ModalSurfaceComposition.frameHeight(
          modal,
          SurfaceFrameLayout.minimumTargetRows(state.persisted.config.interfaceDensity)
        )
      case SurfaceContent.Terminal(_, _) | SurfaceContent.Outline(_, _) | SurfaceContent.Comments(_, _) |
          SurfaceContent.Diagnostics(_, _) | SurfaceContent.MarkdownPreview(_, _) =>
        math.min(8, math.max(4, maxHeight - 1))
      case SurfaceContent.ShortcutsHelp(groups) =>
        // Wants enough rows for every group heading plus its entries, but never more than the viewport allows --
        // `resolveShortcutsHelp` clips to whatever height it is actually given.
        math.min(maxHeight - 1, math.max(4, groups.map(g => g.entries.size + 1).sum + 2))
      case SurfaceContent.TabList(entries, _) =>
        // Scales with the number of open tabs rather than the fixed `commandSurfaceMaxHeight` cap the command
        // palette itself is still capped by (issue #1045) -- a many-tab session should see all of its tabs, not
        // just the ~3 that cap would allow.
        math.min(maxHeight - 1, math.max(4, entries.size + 2))
      case SurfaceContent.RecentFilesInMode(_, paths) =>
        math.min(maxHeight - 1, math.max(4, paths.size + 2))
      case SurfaceContent.TabBar(_, _) =>
        // A single always-visible strip row, the same "quiet single line" sizing as StatusLine below -- see also
        // the matching `floor` case for it just below.
        1

    val floor = content match
      case SurfaceContent.StatusLine(_) | SurfaceContent.TabBar(_, _) => 1
      case _                                                          => 3
    math.max(floor, math.min(maxHeight, preferredHeight))
