package com.serenity.state.components

import com.serenity.keystroke.events.*
import com.serenity.state.models.{AppState, Focus, SurfacePresentation}
import com.serenity.state.reducers.{AppEffect, ExplorerEffect, FileEffect, ReducerResult}
import com.serenity.ui.layout.{DirectoryTreeData, PanelPosition}

class PinnedPanelComponent(
    position: PanelPosition
) extends TypedFocusedComponent[PanelInputEvent]:

  protected def decodeEvent(event: Event): Option[PanelInputEvent] =
    PanelInputEvent.fromEvent(event)

  protected def processTypedEvent(event: PanelInputEvent, currentState: AppState): ComponentResult =
    currentState.runtime.uiSurfaces.find {
      _.presentation match
        case SurfacePresentation.Pinned(pos, _) if pos == position   => true
        case SurfacePresentation.Expanded(pos, _) if pos == position => true
        case _                                                       => false
    } match
      case Some(_) =>
        processPanelEvent(event, currentState)
      case None => ComponentResult.noChange

  private def processPanelEvent(event: PanelInputEvent, currentState: AppState): ComponentResult =
    event match
      case PanelInputEvent.Navigate(direction) =>
        navigateDirectoryPanel(direction, currentState).getOrElse(ComponentResult.noChange)
      case PanelInputEvent.Activate =>
        activateDirectorySelection(currentState).getOrElse(ComponentResult.noChange)
      case PanelInputEvent.NoOp =>
        ComponentResult.noChange
      case PanelInputEvent.ReturnFocus =>
        currentState.persisted.layout.activeEditorPaneId match
          case Some(paneId) => ComponentResult.transferFocus(Focus.EditorPane(paneId))
          case None         => ComponentResult.noChange

  private def navigateDirectoryPanel(direction: Direction, currentState: AppState): Option[ComponentResult] =
    direction match
      case Direction.Up | Direction.Down =>
        moveDirectorySelection(direction, currentState)
      case Direction.Left =>
        activeDirectorySurface(currentState).flatMap(parentDirectoryNavigation(currentState, _))
      case Direction.Right =>
        activateDirectorySelection(currentState)

  private def moveDirectorySelection(direction: Direction, currentState: AppState): Option[ComponentResult] =
    activeDirectorySurface(currentState).flatMap { surface =>
      surface.content match
        case com.serenity.state.models.SurfaceContent.DirectoryTree(tree, selectedPath) =>
          val visibleRows   = DirectoryTreeData.visibleRows(tree)
          val selectedIndex = selectedIndexFor(visibleRows, selectedPath)
          val delta = direction match
            case Direction.Up   => -1
            case Direction.Down => 1
            case _              => 0
          val nextIndex = (selectedIndex + delta).max(0).min(visibleRows.length - 1)
          if visibleRows.isEmpty || nextIndex == selectedIndex then None
          else
            val updated = surface.copy(
              content = com.serenity.state.models.SurfaceContent.DirectoryTree(tree, Some(visibleRows(nextIndex).path))
            )
            Some(ComponentResult.updateState(replaceSurface(_, updated)))
        case _ =>
          None
    }

  private def activateDirectorySelection(currentState: AppState): Option[ComponentResult] =
    activeDirectorySurface(currentState).flatMap { surface =>
      surface.content match
        case com.serenity.state.models.SurfaceContent.DirectoryTree(tree, selectedPath) =>
          val visibleRows = DirectoryTreeData.visibleRows(tree)
          selectedPath
            .flatMap(path => visibleRows.find(_.path == path))
            .map { row =>
              if row.isDirectory then
                if row.isExpanded then ComponentResult.noChange
                else if row.isLoaded then
                  val updated = surface.copy(
                    content = com.serenity.state.models.SurfaceContent.DirectoryTree(
                      tree.copy(expandedPaths = tree.expandedPaths + row.path),
                      Some(row.path)
                    )
                  )
                  ComponentResult.updateState(replaceSurface(_, updated))
                else
                  ComponentResult.reducerResult(
                    ReducerResult.withEffect(
                      currentState,
                      AppEffect.Explorer(ExplorerEffect.LoadDirectory(position, row.path))
                    )
                  )
              else
                ComponentResult.reducerResult(
                  ReducerResult.withEffect(currentState, AppEffect.File(FileEffect.DirectLoadFile(row.path)))
                )
            }
        case _ =>
          None
    }

  private def parentDirectoryNavigation(
    currentState: AppState,
    surface: com.serenity.state.models.UiSurface
  ): Option[ComponentResult] =
    surface.content match
      case com.serenity.state.models.SurfaceContent.DirectoryTree(tree, selectedPath) =>
        selectedPath.flatMap(path => collapseOrSelectParent(surface, tree, path))
      case _ =>
        None

  private def collapseOrSelectParent(
    surface: com.serenity.state.models.UiSurface,
    tree: DirectoryTreeData,
    selectedPath: java.nio.file.Path
  ): Option[ComponentResult] =
    if tree.expandedPaths.contains(selectedPath) then
      val updated = surface.copy(
        content = com.serenity.state.models.SurfaceContent.DirectoryTree(
          tree.copy(expandedPaths = tree.expandedPaths - selectedPath),
          Some(selectedPath)
        )
      )
      Some(ComponentResult.updateState(replaceSurface(_, updated)))
    else
      Option(selectedPath.getParent)
        .filter(parent => parent != tree.rootPath)
        .map(parent =>
          val updated = surface.copy(
            content = com.serenity.state.models.SurfaceContent.DirectoryTree(tree, Some(parent))
          )
          ComponentResult.updateState(replaceSurface(_, updated))
        )

  private def selectedIndexFor(
    rows: List[com.serenity.ui.layout.DirectoryTreeRow],
    selectedPath: Option[java.nio.file.Path]
  ): Int =
    selectedPath
      .flatMap(path =>
        rows.indexWhere(_.path == path) match
          case -1  => None
          case idx => Some(idx)
      )
      .getOrElse(0)

  private def activeDirectorySurface(currentState: AppState) =
    focusedPinnedSurface(currentState).orElse(currentState.runtime.uiSurfaces.reverse.find {
      _.presentation match
        case SurfacePresentation.Pinned(pos, _) if pos == position   => true
        case SurfacePresentation.Expanded(pos, _) if pos == position => true
        case _                                                       => false
    })

  private def focusedPinnedSurface(currentState: AppState) =
    currentState.persisted.focus match
      case Focus.Surface(surfaceId) =>
        currentState.surfaceById(surfaceId).filter { surface =>
          surface.presentation match
            case SurfacePresentation.Pinned(pos, _) if pos == position   => true
            case SurfacePresentation.Expanded(pos, _) if pos == position => true
            case _                                                       => false
        }
      case _ =>
        None

  private def replaceSurface(currentState: AppState, updated: com.serenity.state.models.UiSurface): AppState =
    currentState.copy(runtime =
      currentState.runtime.copy(uiSurfaces = currentState.runtime.uiSurfaces.filterNot(_.id == updated.id) :+ updated)
    )
