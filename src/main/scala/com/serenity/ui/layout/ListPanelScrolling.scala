package com.serenity.ui.layout

import com.serenity.document.DocumentNavigation
import com.serenity.state.models.{CursorPosition, SurfaceContent}
import com.serenity.ui.widget.ListScroll

/** A list panel's [[ListScroll]] resolved against the frame it is painted in, so the panel's keys, the wheel, painting
  * and hit-testing agree on which rows are shown.
  */
object ListPanelScrolling:

  /** How many items a list panel framed by `frameRect` shows at once. */
  def viewportRows(content: SurfaceContent, frameRect: LayoutRect): Int =
    content match
      case _: SurfaceContent.Diagnostics => diagnosticsItemRows(frameRect)
      case _                             => itemRows(frameRect)

  private[layout] def itemRows(frameRect: LayoutRect): Int = math.max(1, frameRect.height - 2)

  /** A square diagnostics panel spends its first row on the error and warning counts. */
  private[layout] def diagnosticsItemRows(frameRect: LayoutRect): Int =
    SurfaceLayoutKind.classify(frameRect) match
      case SurfaceLayoutKind.Square => math.max(1, frameRect.height - 3)
      case _                        => itemRows(frameRect)

  /** `content` scrolled `lines` by the wheel (positive towards the end), its highlight left where it is; `None` when
    * `content` is not a list that scrolls in this frame. An outline or comments panel with nothing picked highlights
    * the entry `cursor` is in, so that is where it is shown from.
    */
  def wheeled(
    content: SurfaceContent,
    frameRect: LayoutRect,
    lines: Int,
    cursor: Option[CursorPosition]
  ): Option[SurfaceContent] =
    val rows = viewportRows(content, frameRect)
    content match
      case SurfaceContent.DirectoryTree(tree, selectedPath, scroll) =>
        val shown    = DirectoryTreeData.rowList(tree, selectedPath, scroll, rows)
        val scrolled = scroll.scrolledBy(lines, shown.items.size, shown.selected, rows)
        Some(SurfaceContent.DirectoryTree(tree, selectedPath, scrolled))
      case SurfaceContent.Outline(symbols, active, scroll) if listsItems(frameRect) =>
        val highlighted = active.orElse(cursorEntry(symbols, cursor))
        val scrolled    = scrolledBy(scroll, symbols.map(_.location), highlighted, lines, rows)
        Some(SurfaceContent.Outline(symbols, active, scrolled))
      case SurfaceContent.Comments(symbols, active, scroll) if listsItems(frameRect) =>
        val highlighted = active.orElse(cursorEntry(symbols, cursor))
        val scrolled    = scrolledBy(scroll, symbols.map(_.location), highlighted, lines, rows)
        Some(SurfaceContent.Comments(symbols, active, scrolled))
      case SurfaceContent.Diagnostics(issues, active, scroll) if listsItems(frameRect) =>
        Some(
          SurfaceContent.Diagnostics(issues, active, scrolledBy(scroll, issues.map(_.location), active, lines, rows))
        )
      case _ => None

  /** `scroll` pinned to the rows shown with `highlighted` highlighted, so a click or hover that moves the highlight to
    * one of them leaves every row where it is.
    */
  def pinned(scroll: ListScroll, locations: List[Location], highlighted: Option[Location], rows: Int): ListScroll =
    scroll.copy(offset = scroll.shownOffset(locations.size, indexOf(locations, highlighted), rows))

  /** Only the vertical and square layouts list items a row each; the others summarise them. */
  private def listsItems(frameRect: LayoutRect): Boolean =
    SurfaceLayoutKind.classify(frameRect) match
      case SurfaceLayoutKind.Vertical | SurfaceLayoutKind.Square    => true
      case SurfaceLayoutKind.Horizontal | SurfaceLayoutKind.Compact => false

  private def scrolledBy(
    scroll: ListScroll,
    locations: List[Location],
    highlighted: Option[Location],
    lines: Int,
    rows: Int
  ): ListScroll =
    scroll.scrolledBy(lines, locations.size, indexOf(locations, highlighted), rows)

  private def cursorEntry(symbols: List[Symbol], cursor: Option[CursorPosition]): Option[Location] =
    cursor.flatMap(DocumentNavigation.currentSymbol(symbols, _)).map(_.location)

  private def indexOf(locations: List[Location], target: Option[Location]): Option[Int] =
    target.map(locations.indexOf).filter(_ >= 0)
