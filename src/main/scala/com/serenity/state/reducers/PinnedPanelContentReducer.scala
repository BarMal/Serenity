package com.serenity.state.reducers

import java.nio.file.Path

import com.serenity.state.models.*
import com.serenity.ui.layout.{DirEntry, DirectoryTreeData, PanelContent, PanelPosition}

/** Refreshes the content of an already-pinned terminal or explorer panel in place, pinning a new one only when none is
  * there -- a periodic refresh must not stack up a fresh panel (and undo step) per call (issue #1294).
  */
object PinnedPanelContentReducer:

  private val ExplorerSize = 30

  def pinOrUpdateTerminal(text: String, position: PanelPosition, size: Int, state: AppState): ReducerResult =
    newestPinned(state)(isTerminal) match
      case Some(surface) =>
        val refreshed = surface.copy(content = SurfaceContent.Terminal(text, text.length))
        ReducerResult.noEffects(replaceSurface(state, refreshed))
      case None =>
        PanelStateReducer.pin(PanelContent.Terminal(text, text.length), position, size, state)

  def loadDirectoryTree(rootPath: Path, files: List[String], state: AppState): ReducerResult =
    val entries = files.map(name => DirEntry(rootPath.resolve(name), name, isDirectory = name.endsWith("/")))
    val tree    = DirectoryTreeData(rootPath, entries = Map(rootPath -> entries))
    newestPinned(state)(isDirectoryTree) match
      case Some(surface) =>
        ReducerResult.noEffects(replaceSurface(state, surface.copy(content = SurfaceContent.DirectoryTree(tree, None))))
      case None =>
        val content = PanelContent.DirectoryTree(tree, selectedPath = None)
        PanelStateReducer.pin(content, PanelPosition.Left, ExplorerSize, state)

  /** Pins an explorer on `root` before its listing is read, so the panel appears at once; [[applyRootListing]] fills it
    * in when the listing arrives.
    */
  def pinExplorerRoot(position: PanelPosition, root: Path, size: Int, state: AppState): ReducerResult =
    PanelStateReducer.pin(
      PanelContent.DirectoryTree(DirectoryTreeData(root), selectedPath = None),
      position,
      size,
      state
    )

  /** Applied only while the explorer at `position` is still rooted at `root`. */
  def applyRootListing(position: PanelPosition, root: Path, listing: List[DirEntry], state: AppState): AppState =
    explorerAt(position, state)(_.rootPath == root).fold(state) { (surface, tree, selectedPath) =>
      val listed   = tree.copy(entries = tree.entries.updated(root, listing))
      val selected = selectedPath.orElse(listing.headOption.map(_.path))
      replaceSurface(state, surface.copy(content = SurfaceContent.DirectoryTree(listed, selected)))
    }

  /** Expands `path` with its listing -- applied only while the explorer at `position` still shows the tree `path` is
    * in.
    */
  def applyDirectoryListing(position: PanelPosition, path: Path, listing: List[DirEntry], state: AppState): AppState =
    explorerAt(position, state)(tree => path.startsWith(tree.rootPath)).fold(state) { (surface, tree, selectedPath) =>
      val expanded = tree.copy(expandedPaths = tree.expandedPaths + path, entries = tree.entries.updated(path, listing))
      val selected = if selectedPath.forall(_ == path) then Some(path) else selectedPath
      replaceSurface(state, surface.copy(content = SurfaceContent.DirectoryTree(expanded, selected)))
    }

  def selectFileInExplorer(targetPath: Path, state: AppState): ReducerResult =
    val selected = newestPinned(state)(isDirectoryTree).flatMap { surface =>
      surface.content match
        case SurfaceContent.DirectoryTree(tree, _) =>
          Some(replaceSurface(state, surface.copy(content = SurfaceContent.DirectoryTree(tree, Some(targetPath)))))
        case _ => None
    }
    ReducerResult.noEffects(selected.getOrElse(state))

  /** Drops a file that has just been moved on disk from every explorer listing its old directory. */
  def forgetMovedFile(source: Path, state: AppState): ReducerResult =
    val withoutSource = Option(source.getParent).fold(state) { sourceDir =>
      state.pinnedSurfaces.foldLeft(state) { (current, surface) =>
        surface.content match
          case SurfaceContent.DirectoryTree(tree, selectedPath) =>
            tree.entries.get(sourceDir).fold(current) { listed =>
              val pruned = tree.copy(entries = tree.entries.updated(sourceDir, listed.filterNot(_.path == source)))
              replaceSurface(current, surface.copy(content = SurfaceContent.DirectoryTree(pruned, selectedPath)))
            }
          case _ => current
      }
    }
    ReducerResult.noEffects(withoutSource)

  private def explorerAt(position: PanelPosition, state: AppState)(
    shows: DirectoryTreeData => Boolean
  ): Option[(UiSurface, DirectoryTreeData, Option[Path])] =
    state.pinnedSurfaces.reverse.collectFirst {
      case surface @ UiSurface(_, SurfaceContent.DirectoryTree(tree, selectedPath), _, _)
          if shows(tree) &&
            state.persisted.layout.workspaceTree.flatMap(_.positionForSurface(surface.id)).contains(position) =>
        (surface, tree, selectedPath)
    }

  private def newestPinned(state: AppState)(matches: SurfaceContent => Boolean): Option[UiSurface] =
    state.pinnedSurfaces.reverse.find(surface => matches(surface.content))

  private def isTerminal(content: SurfaceContent): Boolean =
    content match
      case SurfaceContent.Terminal(_, _) => true
      case _                             => false

  private def isDirectoryTree(content: SurfaceContent): Boolean =
    content match
      case SurfaceContent.DirectoryTree(_, _) => true
      case _                                  => false

  private def replaceSurface(state: AppState, surface: UiSurface): AppState =
    state.copy(runtime =
      state.runtime.copy(uiSurfaces = state.runtime.uiSurfaces.movedToEndWhere(_.id == surface.id)(surface))
    )
