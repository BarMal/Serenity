package com.serenity.ui.layout

import java.nio.file.Path

enum PanelPosition:
  case Left, Right, Bottom, Top

case class PinnedPanel(
    position: PanelPosition,
    content: PanelContent,
    size: Int
)

enum PanelContent:
  case DirectoryTree(tree: DirectoryTreeData, selectedPath: Option[Path])
  case Terminal(buffer: String, cursor: Int)
  case Outline(symbols: List[Symbol])
  case Diagnostics(issues: List[Diagnostic])

case class DirectoryTreeData(
    rootPath: Path,
    expandedPaths: Set[Path] = Set.empty,
    entries: Map[Path, List[DirEntry]] = Map.empty
)

case class DirEntry(
    path: Path,
    name: String,
    isDirectory: Boolean,
    isHidden: Boolean = false
)

case class Symbol(
    name: String,
    kind: SymbolKind,
    location: Location
)

enum SymbolKind:
  case Function, Class, Method, Variable, Constant

case class Location(
    line: Int,
    column: Int
)

case class Diagnostic(
    message: String,
    severity: DiagnosticSeverity,
    location: Location
)

enum DiagnosticSeverity:
  case Error, Warning, Info, Hint
