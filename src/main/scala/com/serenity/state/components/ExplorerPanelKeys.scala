package com.serenity.state.components

import java.nio.file.Path

import com.serenity.keystroke.events.{Direction, PanelInputEvent}
import com.serenity.state.models.{AppState, SurfaceContent, UiSurface}
import com.serenity.state.reducers.{AppEffect, FileEffect, ReducerResult}
import com.serenity.ui.layout.{DirectoryTreeData, DirectoryTreeNode}
import com.serenity.ui.widget.{ListScroll, TreeOutcome, TreeView, WidgetInput}

/** The explorer's keys, which [[TreeView]] interprets: Up/Down/Home/End/PageUp/PageDown move the selection, Right opens
  * a file, expands a folder or steps into an expanded one, Enter opens a file or toggles a folder, and Left collapses a
  * folder or selects its parent. Whatever ends up selected is scrolled into view, even when the wheel had scrolled the
  * selection away.
  */
private[components] object ExplorerPanelKeys:

  def handle(
    event: PanelInputEvent,
    surface: UiSurface,
    tree: DirectoryTreeData,
    selectedPath: Option[Path],
    scroll: ListScroll,
    currentState: AppState,
    visibleRows: Int
  ): Option[ComponentResult] =
    val view = anchored(event, DirectoryTreeData.treeView(tree, selectedPath, scroll, visibleRows), visibleRows)
    def shown(nextTree: DirectoryTreeData, nextSelection: Option[Path]): ComponentResult =
      val following  = scroll.copy(followsSelection = true)
      val keptInView = DirectoryTreeData.rowList(nextTree, nextSelection, following, visibleRows).offset
      PanelSurfaces.replaced(surface, SurfaceContent.DirectoryTree(nextTree, nextSelection, ListScroll(keptInView)))
    def moved(next: TreeView[Path, DirectoryTreeNode]): Option[ComponentResult] =
      val nextSelection = next.selectedNode.map(_.key)
      if nextSelection == view.selectedNode.map(_.key) && next.expanded == view.expanded then None
      else Some(shown(expanded(tree, view, next), nextSelection))
    event match
      case PanelInputEvent.Page(delta) => moved(view.copy(rows = view.rows.pageBy(delta, visibleRows)))
      case other =>
        failedListing(other, view)
          .map(path => shown(tree.retried(path), selectedPath))
          .orElse(
            widgetInput(other, view).flatMap { input =>
              view.update(input, visibleRows) match
                case (_, Some(TreeOutcome.Activated(node))) => Some(open(node.value.path, currentState))
                case (next, _)                              => moved(next)
            }
          )

  /** Moving with nothing selected starts from the first row, as the explorer always has. */
  private def anchored(
    event: PanelInputEvent,
    view: TreeView[Path, DirectoryTreeNode],
    visibleRows: Int
  ): TreeView[Path, DirectoryTreeNode] =
    val vertical = event match
      case PanelInputEvent.Navigate(Direction.Up | Direction.Down) | PanelInputEvent.Page(_) => true
      case _                                                                                 => false
    if vertical && view.rows.selected.isEmpty then view.copy(rows = view.rows.select(0, visibleRows)) else view

  private def failedListing(event: PanelInputEvent, view: TreeView[Path, DirectoryTreeNode]): Option[Path] =
    val opens = event match
      case PanelInputEvent.Navigate(Direction.Right) | PanelInputEvent.Activate => true
      case _                                                                    => false
    view.selectedNode
      .filter(node => opens && node.value.isDirectory && node.value.failure.nonEmpty)
      .map(_.key)
      .filter(view.expanded.contains)

  private def widgetInput(event: PanelInputEvent, view: TreeView[Path, DirectoryTreeNode]): Option[WidgetInput] =
    event match
      case PanelInputEvent.Navigate(Direction.Up)   => Some(WidgetInput.Up)
      case PanelInputEvent.Navigate(Direction.Down) => Some(WidgetInput.Down)
      case PanelInputEvent.Navigate(Direction.Left) => Some(WidgetInput.Left)
      case PanelInputEvent.Navigate(Direction.Right) =>
        Some(if view.selectedNode.exists(_.value.isDirectory) then WidgetInput.Right else WidgetInput.Activate)
      case PanelInputEvent.First    => Some(WidgetInput.First)
      case PanelInputEvent.Last     => Some(WidgetInput.Last)
      case PanelInputEvent.Activate => Some(WidgetInput.Activate)
      case _                        => None

  /** The tree after `next`'s expansions, with a folder that failed to list asked to list again when it is opened. */
  private def expanded(
    tree: DirectoryTreeData,
    before: TreeView[Path, DirectoryTreeNode],
    next: TreeView[Path, DirectoryTreeNode]
  ): DirectoryTreeData =
    val expandedPaths = next.expanded - tree.rootPath
    (expandedPaths -- before.expanded)
      .foldLeft(tree.copy(expandedPaths = expandedPaths))(_.retried(_))

  private def open(path: Path, currentState: AppState): ComponentResult =
    ComponentResult.reducerResult(
      ReducerResult.withEffect(currentState, AppEffect.File(FileEffect.DirectLoadFile(path)))
    )
