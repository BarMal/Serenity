package com.serenity.ui.layout

import com.serenity.state.models.*

/** Resolves the fixed-shape informational panels -- directory listings/trees, the terminal, outline/comments/
  * diagnostics lists, the shortcuts reference, and the tab/recent-files corner widgets -- into overlay rows. Split out
  * of `SurfaceContentResolver` to keep that file's dispatcher readable -- see the doc comment there.
  */
private[layout] object PanelContentResolver:

  def resolveDirectoryListing(
    rect: LayoutRect,
    mode: SurfaceRenderMode,
    rootName: String,
    entryNames: List[String],
    selectedName: Option[String]
  ): ResolvedSurfaceContent =
    val layoutKind = SurfaceLayoutKind.classify(rect)
    val lines = (mode, layoutKind) match
      case (SurfaceRenderMode.Floating, SurfaceLayoutKind.Horizontal) =>
        List(s"$rootName  ${entryNames.take(4).mkString(" | ")}")
      case (SurfaceRenderMode.Floating, SurfaceLayoutKind.Vertical) =>
        rootName :: entryNames.take(4)
      case (SurfaceRenderMode.Floating, SurfaceLayoutKind.Square) =>
        s"Directory: $rootName" :: entryNames.take(3)
      case (SurfaceRenderMode.Floating, SurfaceLayoutKind.Compact) =>
        List(s"$rootName (${entryNames.length})")
      case (SurfaceRenderMode.Pinned, SurfaceLayoutKind.Horizontal) =>
        List(entryNames.take(4).mkString(" | ")).filter(_.nonEmpty)
      case (SurfaceRenderMode.Pinned, SurfaceLayoutKind.Vertical) =>
        entryNames.take(4)
      case (SurfaceRenderMode.Pinned, SurfaceLayoutKind.Square) =>
        selectedName.map(name => s"Selected: $name").toList ++ entryNames.take(3)
      case (SurfaceRenderMode.Pinned, SurfaceLayoutKind.Compact) =>
        List(s"${entryNames.length} entries")

    ResolvedSurfaceContent(
      title = mode.titleFor(rootName),
      rows = lines.map(OverlayRow(_))
    )

  def resolveDirectoryTree(
    rect: LayoutRect,
    mode: SurfaceRenderMode,
    tree: com.serenity.ui.layout.DirectoryTreeData,
    selectedPath: Option[java.nio.file.Path]
  ): ResolvedSurfaceContent =
    val visibleRows = com.serenity.ui.layout.DirectoryTreeData.visibleRows(tree)
    val maxRows     = math.max(1, rect.height - 2)
    val rows = visibleRows.take(maxRows).map { row =>
      val marker =
        if row.isDirectory then
          if row.isExpanded then "▾ "
          else if row.isLoaded then "▸ "
          else "▹ "
        else ""
      val indent = "  " * row.depth
      OverlayRow(
        plainText = s"$indent$marker${row.name}",
        selected = selectedPath.contains(row.path)
      )
    }

    ResolvedSurfaceContent(
      title = mode.titleFor(tree.rootPath.getFileName.toString),
      rows = rows
    )

  def resolveTerminal(
    rect: LayoutRect,
    mode: SurfaceRenderMode,
    buffer: String,
    cursor: Int
  ): ResolvedSurfaceContent =
    val lines = buffer.linesIterator.toList
    val shaped = SurfaceLayoutKind.classify(rect) match
      case SurfaceLayoutKind.Horizontal =>
        lines.take(math.max(1, rect.height - 2))
      case SurfaceLayoutKind.Vertical =>
        lines.take(math.max(1, rect.height - 2)).zipWithIndex.map { case (line, index) => s"${index + 1}: $line" }
      case SurfaceLayoutKind.Square =>
        s"cursor: $cursor" :: lines.take(math.max(0, rect.height - 3))
      case SurfaceLayoutKind.Compact =>
        List(s"${lines.length} lines", s"cursor $cursor")

    ResolvedSurfaceContent(mode.titleFor("terminal"), rows = shaped.map(OverlayRow(_)))

  def resolveOutline(
    rect: LayoutRect,
    mode: SurfaceRenderMode,
    symbols: List[Symbol],
    activeLocation: Option[Location]
  ): ResolvedSurfaceContent =
    val shaped: List[OverlayRow] = SurfaceLayoutKind.classify(rect) match
      case SurfaceLayoutKind.Horizontal =>
        val visibleSymbols = symbols.take(4)
        val activeVisible  = visibleSymbols.exists(symbol => activeLocation.contains(symbol.location))
        List(
          visibleSymbols
            .map(symbol => if activeLocation.contains(symbol.location) then s"[${symbol.name}]" else symbol.name)
            .mkString(" | ")
        ).filter(_.nonEmpty).map(text => OverlayRow(text, selected = activeVisible))
      case SurfaceLayoutKind.Vertical =>
        symbols.take(math.max(1, rect.height - 2)).map { symbol =>
          val active = activeLocation.contains(symbol.location)
          val prefix = if active then "> " else ""
          OverlayRow(s"$prefix${symbol.kind} ${symbol.name}", selected = active)
        }
      case SurfaceLayoutKind.Square =>
        symbols.take(math.max(1, rect.height - 2)).map { symbol =>
          val active = activeLocation.contains(symbol.location)
          val prefix = if active then "> " else ""
          OverlayRow(s"$prefix${symbol.name}", selected = active)
        }
      case SurfaceLayoutKind.Compact =>
        val current = activeLocation.flatMap(location => symbols.find(_.location == location)).map(_.name)
        current match
          case Some(name) => List(OverlayRow(s"${symbols.length} symbols", selected = true), OverlayRow(name))
          case None       => List(OverlayRow(s"${symbols.length} symbols"))

    ResolvedSurfaceContent(mode.titleFor("outline"), rows = shaped)

  def resolveComments(
    rect: LayoutRect,
    mode: SurfaceRenderMode,
    symbols: List[Symbol],
    activeLocation: Option[Location]
  ): ResolvedSurfaceContent =
    val shaped: List[OverlayRow] = SurfaceLayoutKind.classify(rect) match
      case SurfaceLayoutKind.Horizontal =>
        val visibleSymbols = symbols.take(4)
        val activeVisible  = visibleSymbols.exists(symbol => activeLocation.contains(symbol.location))
        List(
          visibleSymbols
            .map(symbol => if activeLocation.contains(symbol.location) then s"[${symbol.name}]" else symbol.name)
            .mkString(" | ")
        ).filter(_.nonEmpty).map(text => OverlayRow(text, selected = activeVisible))
      case SurfaceLayoutKind.Vertical | SurfaceLayoutKind.Square =>
        symbols.take(math.max(1, rect.height - 2)).map { symbol =>
          val active = activeLocation.contains(symbol.location)
          val prefix = if active then "> " else ""
          OverlayRow(s"$prefix${symbol.name}", selected = active)
        }
      case SurfaceLayoutKind.Compact =>
        val current = activeLocation.flatMap(location => symbols.find(_.location == location)).map(_.name)
        current match
          case Some(name) => List(OverlayRow(s"${symbols.length} comments", selected = true), OverlayRow(name))
          case None       => List(OverlayRow(s"${symbols.length} comments"))

    ResolvedSurfaceContent(mode.titleFor("comments"), rows = shaped)

  def resolveDiagnostics(
    rect: LayoutRect,
    mode: SurfaceRenderMode,
    issues: List[com.serenity.ui.layout.Diagnostic],
    activeLocation: Option[Location]
  ): ResolvedSurfaceContent =
    val errorCount   = issues.count(_.severity == com.serenity.ui.layout.DiagnosticSeverity.Error)
    val warningCount = issues.count(_.severity == com.serenity.ui.layout.DiagnosticSeverity.Warning)
    val infoCount = issues.count(issue =>
      issue.severity == com.serenity.ui.layout.DiagnosticSeverity.Info ||
        issue.severity == com.serenity.ui.layout.DiagnosticSeverity.Hint
    )
    val shaped = SurfaceLayoutKind.classify(rect) match
      case SurfaceLayoutKind.Horizontal =>
        List(OverlayRow(s"$errorCount error | $warningCount warning | $infoCount info"))
      case SurfaceLayoutKind.Vertical =>
        issues.take(math.max(1, rect.height - 2)).map { issue =>
          OverlayRow(
            s"${issue.severity}: ${issue.message}",
            selected = activeLocation.contains(issue.location)
          )
        }
      case SurfaceLayoutKind.Square =>
        OverlayRow(s"$errorCount error, $warningCount warning") ::
          issues.take(math.max(0, rect.height - 3)).map { issue =>
            OverlayRow(issue.message, selected = activeLocation.contains(issue.location))
          }
      case SurfaceLayoutKind.Compact =>
        List(OverlayRow(s"${issues.length} issues"), OverlayRow(s"$errorCount error"))

    ResolvedSurfaceContent(mode.titleFor("diagnostics"), rows = shaped)

  /** The toggleable keyboard-shortcuts reference (issue #1247). Deliberately not layout-kind-branched like
    * `resolveOutline`/`resolveDiagnostics` above -- there is no "current" entry to highlight or compact down to a
    * one-line summary, so every render mode gets the same group-then-entries listing, clipped to what `rect` fits.
    */
  def resolveShortcutsHelp(
    rect: LayoutRect,
    mode: SurfaceRenderMode,
    groups: List[ShortcutHelpGroup]
  ): ResolvedSurfaceContent =
    val maxRows = math.max(1, rect.height - 2)
    val rows = groups
      .flatMap { group =>
        OverlayRow(group.title) :: group.entries.map(entry => OverlayRow(s"${entry.label}: ${entry.keys}"))
      }
      .take(maxRows)
    ResolvedSurfaceContent(mode.titleFor("Keyboard Shortcuts"), rows = rows)

  /** The mode/tab corner widget's tab list (issue #1307). A dirty tab carries a persistent trailing marker rather than
    * the per-pane header's "- unsaved" text suffix -- this list already names the tab, so repeating the word would just
    * be noise; the highlighted row already marks which tab is focused.
    */
  def resolveTabList(
    rect: LayoutRect,
    mode: SurfaceRenderMode,
    entries: List[TabListEntry],
    activeBufferId: Option[BufferId]
  ): ResolvedSurfaceContent =
    val maxRows = math.max(1, rect.height - 2)
    val rows = entries.take(maxRows).map { entry =>
      val text = if entry.isDirty then s"${entry.title} ●" else entry.title
      OverlayRow(text, selected = activeBufferId.contains(entry.bufferId))
    }
    ResolvedSurfaceContent(mode.titleFor("Tabs"), rows = rows)

  /** The mode/tab corner widget's "recent in this mode" list (issue #1307), one row per path -- same full-path label
    * the startup page's own recent-files list already uses (`AppStartup.createStartPage`).
    */
  def resolveRecentFilesInMode(
    rect: LayoutRect,
    mode: SurfaceRenderMode,
    recentMode: com.serenity.config.AppMode,
    paths: List[java.nio.file.Path]
  ): ResolvedSurfaceContent =
    val maxRows = math.max(1, rect.height - 2)
    val rows =
      if paths.isEmpty then List(OverlayRow("No recent files in this mode yet"))
      else paths.take(maxRows).map(path => OverlayRow(path.toString))
    ResolvedSurfaceContent(mode.titleFor(s"Recent in ${recentMode.toString} Mode"), rows = rows)
