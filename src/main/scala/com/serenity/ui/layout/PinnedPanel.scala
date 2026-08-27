package com.serenity.ui.layout

import java.nio.file.Path

import com.serenity.state.models.{BufferId, SurfaceId}

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

enum PanelContent:
  case DirectoryTree(tree: DirectoryTreeData, selectedPath: Option[Path])
  case Terminal(buffer: String, cursor: Int)
  case Outline(symbols: List[Symbol], activeLocation: Option[Location] = None)
  case Comments(symbols: List[Symbol], activeLocation: Option[Location] = None)
  case Diagnostics(issues: List[Diagnostic])
  case MarkdownPreview(bufferId: BufferId, title: String)

final case class DirectoryTreeData(
    rootPath: Path,
    expandedPaths: Set[Path] = Set.empty,
    entries: Map[Path, List[DirEntry]] = Map.empty
)

final case class DirectoryTreeRow(
    path: Path,
    name: String,
    isDirectory: Boolean,
    depth: Int,
    isRoot: Boolean,
    isExpanded: Boolean,
    isLoaded: Boolean
)

object DirectoryTreeData:

  def visibleRows(tree: DirectoryTreeData): List[DirectoryTreeRow] =
    rootRow(tree) :: flattenChildren(tree, tree.rootPath, depth = 1)

  def visibleEntries(tree: DirectoryTreeData): List[(DirEntry, Int)] =
    visibleRows(tree)
      .filterNot(_.isRoot)
      .map(row => DirEntry(row.path, row.name, row.isDirectory) -> (row.depth - 1))

  private def rootRow(tree: DirectoryTreeData): DirectoryTreeRow =
    DirectoryTreeRow(
      path = tree.rootPath,
      name = tree.rootPath.getFileName.toString,
      isDirectory = true,
      depth = 0,
      isRoot = true,
      isExpanded = true,
      isLoaded = tree.entries.contains(tree.rootPath)
    )

  private def flattenChildren(tree: DirectoryTreeData, directory: Path, depth: Int): List[DirectoryTreeRow] =
    tree.entries.getOrElse(directory, Nil).flatMap { entry =>
      val current = List(
        DirectoryTreeRow(
          path = entry.path,
          name = entry.name,
          isDirectory = entry.isDirectory,
          depth = depth,
          isRoot = false,
          isExpanded = entry.isDirectory && tree.expandedPaths.contains(entry.path),
          isLoaded = entry.isDirectory && tree.entries.contains(entry.path)
        )
      )
      if entry.isDirectory && tree.expandedPaths.contains(entry.path) then
        current ++ flattenChildren(tree, entry.path, depth + 1)
      else current
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
  case Function, Class, Method, Variable, Constant, Heading, Bookmark, Comment, Section

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
