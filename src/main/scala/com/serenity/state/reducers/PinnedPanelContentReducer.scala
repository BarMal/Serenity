package com.serenity.state.reducers

import java.nio.file.Path

import com.serenity.state.models.*
import com.serenity.ui.layout.{DirEntry, DirectoryTreeData, PanelContent, PanelPosition}

/** Refreshes the content of an already-pinned terminal or explorer panel in place, pinning a new one only when none is
  * there -- a periodic refresh must not stack up a fresh panel (and undo step) per call (issue #1294).
  */
object PinnedPanelContentReducer:

  private val ExplorerSize = 30

  /** Shows `text` in the project output panel, docking it if it isn't, and records it as the latest output. */
  def pinOrUpdateTerminal(text: String, position: PanelPosition, size: Int, state: AppState): ReducerResult =
    val recorded =
      state.copy(runtime = state.runtime.copy(projectTasks = state.runtime.projectTasks.copy(terminalText = text)))
    newestPinned(recorded)(isPanel(PanelId.ProjectOutput)) match
      case Some(surface) =>
        val refreshed = surface.copy(content = SurfaceContent.Terminal(text, text.length))
        ReducerResult.noEffects(replaceSurface(recorded, refreshed))
      case None =>
        PanelStateReducer.pin(PanelContent.Terminal(text, text.length), position, size, recorded)

  def loadDirectoryTree(rootPath: Path, files: List[String], state: AppState): ReducerResult =
    val entries = files.map(name => DirEntry(rootPath.resolve(name), name, isDirectory = name.endsWith("/")))
    val tree    = DirectoryTreeData(rootPath, entries = Map(rootPath -> entries))
    newestPinned(state)(isPanel(PanelId.Explorer)) match
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

  /** Applies a listing of `path` to the explorer `surfaceId`, wherever it is docked now -- only while that explorer is
    * still waiting on it, so a listing it no longer wants (collapsed, re-rooted, closed) is dropped. The root's first
    * listing also selects its first entry, so keyboard navigation has somewhere to start.
    */
  def applyListing(
    surfaceId: SurfaceId,
    path: Path,
    listing: Either[String, List[DirEntry]],
    state: AppState
  ): AppState =
    state.surfaceById(surfaceId).fold(state) { surface =>
      surface.content match
        case SurfaceContent.DirectoryTree(tree, selectedPath, scroll) if tree.loading.contains(path) =>
          val content = listing match
            case Right(entries) =>
              val selected =
                if path == tree.rootPath then selectedPath.orElse(entries.headOption.map(_.path)) else selectedPath
              SurfaceContent.DirectoryTree(tree.listed(path, entries), selected, scroll)
            case Left(reason) =>
              SurfaceContent.DirectoryTree(tree.listingFailed(path, reason), selectedPath, scroll)
          replaceSurface(state, surface.copy(content = content))
        case _ => state
    }

  def selectFileInExplorer(targetPath: Path, state: AppState): ReducerResult =
    val selected = newestPinned(state)(isPanel(PanelId.Explorer)).flatMap { surface =>
      surface.content match
        case SurfaceContent.DirectoryTree(tree, _, scroll) =>
          val revealed = SurfaceContent.DirectoryTree(tree, Some(targetPath), scroll.copy(followsSelection = true))
          Some(replaceSurface(state, surface.copy(content = revealed)))
        case _ => None
    }
    ReducerResult.noEffects(selected.getOrElse(state))

  /** Drops a file that has just been moved on disk from every explorer listing its old directory. */
  def forgetMovedFile(source: Path, state: AppState): ReducerResult =
    val withoutSource = Option(source.getParent).fold(state) { sourceDir =>
      state.pinnedSurfaces.foldLeft(state) { (current, surface) =>
        surface.content match
          case SurfaceContent.DirectoryTree(tree, selectedPath, scroll) =>
            tree.entries.get(sourceDir).fold(current) { listed =>
              val pruned  = tree.copy(entries = tree.entries.updated(sourceDir, listed.filterNot(_.path == source)))
              val content = SurfaceContent.DirectoryTree(pruned, selectedPath, scroll)
              replaceSurface(current, surface.copy(content = content))
            }
          case _ => current
      }
    }
    ReducerResult.noEffects(withoutSource)

  private def newestPinned(state: AppState)(matches: SurfaceContent => Boolean): Option[UiSurface] =
    state.pinnedSurfaces.reverse.find(surface => matches(surface.content))

  private def isPanel(id: PanelId)(content: SurfaceContent): Boolean =
    PanelId.forContent(content).contains(id)

  private def replaceSurface(state: AppState, surface: UiSurface): AppState =
    state.copy(runtime =
      state.runtime.copy(uiSurfaces = state.runtime.uiSurfaces.movedToEndWhere(_.id == surface.id)(surface))
    )
