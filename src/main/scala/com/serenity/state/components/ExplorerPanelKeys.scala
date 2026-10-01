package com.serenity.state.components

import com.serenity.keystroke.events.{Direction, PanelInputEvent}
import com.serenity.state.models.{AppState, SurfaceContent, UiSurface}
import com.serenity.state.reducers.{AppEffect, FileEffect, ReducerResult}
import com.serenity.ui.layout.{DirectoryTreeData, DirectoryTreeRow}

/** The explorer's keys: Up/Down/Home/End/PageUp/PageDown move the selection, Right or Enter opens a file or expands a
  * folder, and Left collapses a folder or selects its parent.
  */
private[components] object ExplorerPanelKeys:

  def handle(
    event: PanelInputEvent,
    surface: UiSurface,
    tree: DirectoryTreeData,
    selectedPath: Option[java.nio.file.Path],
    currentState: AppState,
    visibleRows: Int
  ): Option[ComponentResult] =
    val rows                      = DirectoryTreeData.visibleRows(tree)
    def moved(target: Int => Int) = moveSelection(surface, tree, rows, selectedPath, target)
    event match
      case PanelInputEvent.Navigate(Direction.Up)    => moved(_ - 1)
      case PanelInputEvent.Navigate(Direction.Down)  => moved(_ + 1)
      case PanelInputEvent.First                     => moved(_ => 0)
      case PanelInputEvent.Last                      => moved(_ => rows.length - 1)
      case PanelInputEvent.Page(delta)               => moved(_ + delta * math.max(1, visibleRows - 1))
      case PanelInputEvent.Navigate(Direction.Left)  => selectedPath.flatMap(collapseOrSelectParent(surface, tree, _))
      case PanelInputEvent.Navigate(Direction.Right) => activate(surface, tree, rows, selectedPath, currentState)
      case PanelInputEvent.Activate                  => activate(surface, tree, rows, selectedPath, currentState)
      case _                                         => None

  private def moveSelection(
    surface: UiSurface,
    tree: DirectoryTreeData,
    rows: List[DirectoryTreeRow],
    selectedPath: Option[java.nio.file.Path],
    target: Int => Int
  ): Option[ComponentResult] =
    val selectedIndex = selectedIndexFor(rows, selectedPath)
    val nextIndex     = target(selectedIndex).max(0).min(rows.length - 1)
    if rows.isEmpty || nextIndex == selectedIndex then None
    else Some(PanelSurfaces.replaced(surface, SurfaceContent.DirectoryTree(tree, Some(rows(nextIndex).path))))

  private def activate(
    surface: UiSurface,
    tree: DirectoryTreeData,
    rows: List[DirectoryTreeRow],
    selectedPath: Option[java.nio.file.Path],
    currentState: AppState
  ): Option[ComponentResult] =
    selectedPath
      .flatMap(path => rows.find(_.path == path))
      .map { row =>
        if row.isDirectory then
          if row.isExpanded && row.failure.isEmpty then ComponentResult.noChange
          else
            PanelSurfaces.replaced(
              surface,
              SurfaceContent.DirectoryTree(
                tree.copy(expandedPaths = tree.expandedPaths + row.path).retried(row.path),
                Some(row.path)
              )
            )
        else
          ComponentResult.reducerResult(
            ReducerResult.withEffect(currentState, AppEffect.File(FileEffect.DirectLoadFile(row.path)))
          )
      }

  private def collapseOrSelectParent(
    surface: UiSurface,
    tree: DirectoryTreeData,
    selectedPath: java.nio.file.Path
  ): Option[ComponentResult] =
    if tree.expandedPaths.contains(selectedPath) then
      Some(
        PanelSurfaces.replaced(
          surface,
          SurfaceContent.DirectoryTree(tree.copy(expandedPaths = tree.expandedPaths - selectedPath), Some(selectedPath))
        )
      )
    else
      Option(selectedPath.getParent)
        .filter(parent => parent != tree.rootPath)
        .map(parent => PanelSurfaces.replaced(surface, SurfaceContent.DirectoryTree(tree, Some(parent))))

  private def selectedIndexFor(rows: List[DirectoryTreeRow], selectedPath: Option[java.nio.file.Path]): Int =
    selectedPath
      .flatMap(path =>
        rows.indexWhere(_.path == path) match
          case -1  => None
          case idx => Some(idx)
      )
      .getOrElse(0)
