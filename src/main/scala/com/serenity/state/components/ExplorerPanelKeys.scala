package com.serenity.state.components

import java.nio.file.Path

import com.serenity.keystroke.events.{Direction, PanelInputEvent}
import com.serenity.state.models.{AppState, SurfaceContent, UiSurface}
import com.serenity.state.reducers.{AppEffect, FileEffect, ReducerResult}
import com.serenity.ui.layout.{DirectoryTreeData, DirectoryTreeRow}

/** The explorer's keys: Up/Down/Home/End/PageUp/PageDown move the selection, Right or Enter opens a file or expands a
  * folder, and Left collapses a folder or selects its parent. Whatever ends up selected is scrolled into view.
  */
private[components] object ExplorerPanelKeys:

  def handle(
    event: PanelInputEvent,
    surface: UiSurface,
    tree: DirectoryTreeData,
    selectedPath: Option[Path],
    scrollOffset: Int,
    currentState: AppState,
    visibleRows: Int
  ): Option[ComponentResult] =
    val rows = DirectoryTreeData.visibleRows(tree)
    def shown(nextTree: DirectoryTreeData, nextSelection: Option[Path]): ComponentResult =
      val keptInView = DirectoryTreeData.rowList(nextTree, nextSelection, scrollOffset, visibleRows).offset
      PanelSurfaces.replaced(surface, SurfaceContent.DirectoryTree(nextTree, nextSelection, keptInView))
    def moved(target: Int => Int) = moveSelection(rows, selectedPath, target).map(path => shown(tree, Some(path)))
    event match
      case PanelInputEvent.Navigate(Direction.Up)   => moved(_ - 1)
      case PanelInputEvent.Navigate(Direction.Down) => moved(_ + 1)
      case PanelInputEvent.First                    => moved(_ => 0)
      case PanelInputEvent.Last                     => moved(_ => rows.length - 1)
      case PanelInputEvent.Page(delta)              => moved(_ + delta * math.max(1, visibleRows - 1))
      case PanelInputEvent.Navigate(Direction.Left) =>
        selectedPath
          .flatMap(collapseOrSelectParent(tree, _))
          .map((nextTree, nextSelection) => shown(nextTree, nextSelection))
      case PanelInputEvent.Navigate(Direction.Right) => activate(tree, rows, selectedPath, currentState, shown)
      case PanelInputEvent.Activate                  => activate(tree, rows, selectedPath, currentState, shown)
      case _                                         => None

  private def moveSelection(
    rows: Vector[DirectoryTreeRow],
    selectedPath: Option[Path],
    target: Int => Int
  ): Option[Path] =
    val selectedIndex = selectedIndexFor(rows, selectedPath)
    val nextIndex     = target(selectedIndex).max(0).min(rows.length - 1)
    if rows.isEmpty || nextIndex == selectedIndex then None
    else Some(rows(nextIndex).path)

  private def activate(
    tree: DirectoryTreeData,
    rows: Vector[DirectoryTreeRow],
    selectedPath: Option[Path],
    currentState: AppState,
    shown: (DirectoryTreeData, Option[Path]) => ComponentResult
  ): Option[ComponentResult] =
    selectedPath
      .flatMap(path => rows.find(_.path == path))
      .map { row =>
        if row.isDirectory then
          if row.isExpanded && row.failure.isEmpty then ComponentResult.noChange
          else shown(tree.copy(expandedPaths = tree.expandedPaths + row.path).retried(row.path), Some(row.path))
        else
          ComponentResult.reducerResult(
            ReducerResult.withEffect(currentState, AppEffect.File(FileEffect.DirectLoadFile(row.path)))
          )
      }

  private def collapseOrSelectParent(
    tree: DirectoryTreeData,
    selectedPath: Path
  ): Option[(DirectoryTreeData, Option[Path])] =
    if tree.expandedPaths.contains(selectedPath) then
      Some((tree.copy(expandedPaths = tree.expandedPaths - selectedPath), Some(selectedPath)))
    else
      Option(selectedPath.getParent)
        .filter(parent => parent != tree.rootPath)
        .map(parent => (tree, Some(parent)))

  private def selectedIndexFor(rows: Vector[DirectoryTreeRow], selectedPath: Option[Path]): Int =
    selectedPath
      .flatMap(path =>
        rows.indexWhere(_.path == path) match
          case -1  => None
          case idx => Some(idx)
      )
      .getOrElse(0)
