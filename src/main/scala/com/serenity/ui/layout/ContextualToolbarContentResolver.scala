package com.serenity.ui.layout

import scala.annotation.unused

import com.serenity.config.ToolbarDisplayMode
import com.serenity.state.models.*
import com.serenity.ui.fonts.FontLoader

/** Resolves `SurfaceContent.ContextualToolbar` -- the floating per-mode action bar and its dropdown/input detail row --
  * into overlay rows. Split out of `SurfaceContentResolver` to keep that file's dispatcher readable -- see the doc
  * comment there.
  */
object ContextualToolbarContentResolver:

  def resolve(
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
              CommandPaletteContentResolver.inputRow(item.inputItem, selected = true, editingText = Some(text))
          }
          .toList

    ResolvedSurfaceContent(rows = topRows ++ detailRows)

  private def isSelected(item: ContextualToolbarItem): Boolean =
    item match
      case ContextualToolbarItem.Button(_, _, _, _, selected) => selected
      case _                                                  => false
