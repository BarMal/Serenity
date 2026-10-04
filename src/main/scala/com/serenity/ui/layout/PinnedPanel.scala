package com.serenity.ui.layout

import java.nio.file.Path

import com.serenity.state.models.{BufferId, SurfaceContent, SurfaceId}
import com.serenity.ui.widget.{EndBehaviour, SelectableList}

enum PanelPosition:
  case Left, Right, Bottom, Top

/** Identifies which pinned panel a panel operation addresses -- either a specific surface, or "whichever panel is
  * pinned at this position" (see `PanelStateReducer`'s per-operation resolution for what "whichever" means there).
  */
enum PanelTarget:
  case ById(surfaceId: SurfaceId)
  case ByPosition(position: PanelPosition)

final case class PinnedPanel(
    position: PanelPosition,
    content: PanelContent,
    size: Int
)

/** The [[SurfaceContent]] cases that can be pinned to a panel position. `SurfaceContent` is the single authoritative
  * content model; each case here wraps the exact `SurfaceContent` value it stands for, so a panel and its underlying
  * surface can never carry different payloads for the same content -- there is no second enum to keep in sync by hand
  * (issue #1009).
  */
enum PanelContent(val asSurfaceContent: SurfaceContent):
  case DirectoryTree(tree: DirectoryTreeData, selectedPath: Option[Path] = None, scrollOffset: Int = 0)
      extends PanelContent(SurfaceContent.DirectoryTree(tree, selectedPath, scrollOffset))
  case Terminal(buffer: String, cursor: Int) extends PanelContent(SurfaceContent.Terminal(buffer, cursor))
  case Outline(symbols: List[Symbol], activeLocation: Option[Location] = None)
      extends PanelContent(SurfaceContent.Outline(symbols, activeLocation))
  case Comments(symbols: List[Symbol], activeLocation: Option[Location] = None)
      extends PanelContent(SurfaceContent.Comments(symbols, activeLocation))
  case Diagnostics(issues: List[Diagnostic], activeLocation: Option[Location] = None)
      extends PanelContent(SurfaceContent.Diagnostics(issues, activeLocation))
  case MarkdownPreview(bufferId: BufferId, title: String)
      extends PanelContent(SurfaceContent.MarkdownPreview(bufferId, title))

object PanelContent:

  /** The pinnable subset of `SurfaceContent` -- `None` for any case that is not a panel content kind. */
  def fromSurfaceContent(content: SurfaceContent): Option[PanelContent] =
    content match
      case SurfaceContent.DirectoryTree(tree, selectedPath, scrollOffset) =>
        Some(DirectoryTree(tree, selectedPath, scrollOffset))
      case SurfaceContent.Terminal(buffer, cursor)            => Some(Terminal(buffer, cursor))
      case SurfaceContent.Outline(symbols, activeLocation)    => Some(Outline(symbols, activeLocation))
      case SurfaceContent.Comments(symbols, activeLocation)   => Some(Comments(symbols, activeLocation))
      case SurfaceContent.Diagnostics(issues, activeLocation) => Some(Diagnostics(issues, activeLocation))
      case SurfaceContent.MarkdownPreview(bufferId, title)    => Some(MarkdownPreview(bufferId, title))
      case _                                                  => None

/** `loading`, `stale` and `failed` track each shown directory's listing: a directory is listed when it is shown (the
  * root, or expanded) and has no listing yet or a stale one, unless a listing is already on its way or last failed.
  */
final case class DirectoryTreeData(
    rootPath: Path,
    expandedPaths: Set[Path] = Set.empty,
    entries: Map[Path, List[DirEntry]] = Map.empty,
    loading: Set[Path] = Set.empty,
    stale: Set[Path] = Set.empty,
    failed: Map[Path, String] = Map.empty
):

  /** Flattened once per tree value. Moving the selection or repainting reuses the same tree, so only what makes a new
    * one -- expanding, collapsing or a new listing -- pays for flattening again.
    */
  private lazy val flattenedRows: Vector[DirectoryTreeRow] = DirectoryTreeData.flatten(this)

  def awaitingListing: Set[Path] =
    (expandedPaths + rootPath).filter(path =>
      (!entries.contains(path) || stale.contains(path)) && !loading.contains(path) && !failed.contains(path)
    )

  def listingRequested(paths: Set[Path]): DirectoryTreeData =
    copy(loading = loading ++ paths)

  def listed(path: Path, listing: List[DirEntry]): DirectoryTreeData =
    copy(
      entries = entries.updated(path, listing),
      loading = loading - path,
      stale = stale - path,
      failed = failed - path
    )

  def listingFailed(path: Path, reason: String): DirectoryTreeData =
    copy(loading = loading - path, failed = failed.updated(path, reason))

  /** Clears `path`'s failure so it is listed again the next time it is shown. */
  def retried(path: Path): DirectoryTreeData =
    copy(failed = failed - path)

final case class DirectoryTreeRow(
    path: Path,
    name: String,
    isDirectory: Boolean,
    depth: Int,
    isRoot: Boolean,
    isExpanded: Boolean,
    isLoaded: Boolean,
    isLoading: Boolean = false,
    failure: Option[String] = None
)

object DirectoryTreeData:

  def visibleRows(tree: DirectoryTreeData): Vector[DirectoryTreeRow] = tree.flattenedRows

  /** The visible rows as a list selecting `selectedPath`'s row, scrolled from `scrollOffset` just far enough to show
    * that row in `viewportRows` -- the one place the explorer's keys, painting and hit-testing get their window from.
    */
  def rowList(
    tree: DirectoryTreeData,
    selectedPath: Option[Path],
    scrollOffset: Int,
    viewportRows: Int
  ): SelectableList[DirectoryTreeRow] =
    val rows   = visibleRows(tree)
    val stored = SelectableList(rows, offset = scrollOffset, endBehaviour = EndBehaviour.Stop)
    selectedPath
      .map(path => rows.indexWhere(_.path == path))
      .filter(_ >= 0)
      .fold(stored.scrollBy(0, viewportRows))(stored.select(_, viewportRows))

  def visibleEntries(tree: DirectoryTreeData): List[(DirEntry, Int)] =
    visibleRows(tree)
      .filterNot(_.isRoot)
      .map(row => DirEntry(row.path, row.name, row.isDirectory) -> (row.depth - 1))
      .toList

  private def flatten(tree: DirectoryTreeData): Vector[DirectoryTreeRow] =
    rootRow(tree) +: flattenChildren(tree, tree.rootPath, depth = 1)

  private def rootRow(tree: DirectoryTreeData): DirectoryTreeRow =
    DirectoryTreeRow(
      path = tree.rootPath,
      name = tree.rootPath.getFileName.toString,
      isDirectory = true,
      depth = 0,
      isRoot = true,
      isExpanded = true,
      isLoaded = tree.entries.contains(tree.rootPath),
      isLoading = tree.loading.contains(tree.rootPath),
      failure = tree.failed.get(tree.rootPath)
    )

  private def flattenChildren(tree: DirectoryTreeData, directory: Path, depth: Int): Vector[DirectoryTreeRow] =
    tree.entries.getOrElse(directory, Nil).toVector.flatMap { entry =>
      val isExpanded = entry.isDirectory && tree.expandedPaths.contains(entry.path)
      val row = DirectoryTreeRow(
        path = entry.path,
        name = entry.name,
        isDirectory = entry.isDirectory,
        depth = depth,
        isRoot = false,
        isExpanded = isExpanded,
        isLoaded = entry.isDirectory && tree.entries.contains(entry.path),
        isLoading = tree.loading.contains(entry.path),
        failure = tree.failed.get(entry.path)
      )
      if isExpanded then row +: flattenChildren(tree, entry.path, depth + 1) else Vector(row)
    }

final case class DirEntry(
    path: Path,
    name: String,
    isDirectory: Boolean,
    isHidden: Boolean = false
)

final case class Symbol(
    name: String,
    kind: SymbolKind,
    location: Location
)

enum SymbolKind:
  case Function, Class, Method, Variable, Constant, Heading, Bookmark, Comment, Section, Placeholder

final case class Location(
    line: Int,
    column: Int
)

final case class Diagnostic(
    message: String,
    severity: DiagnosticSeverity,
    location: Location
)

enum DiagnosticSeverity:
  case Error, Warning, Info, Hint
