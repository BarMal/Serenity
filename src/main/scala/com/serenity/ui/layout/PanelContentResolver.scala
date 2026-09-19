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
      title = SurfaceContentResolver.titleFor(mode, rootName),
      rows = lines.map(OverlayRow(_))
    )

  /** One rendered directory-tree row paired with the filesystem path it represents -- shared by `resolveDirectoryTree`
    * (which only needs `row`) and `DirectoryTreeSurfaceComposition` (issue #819, slice 4), which also needs `path` to
    * build the row's hit region, so both build the exact same row text and clipping from one place.
    */
  final private[layout] case class DirectoryTreeRowView(row: OverlayRow, path: java.nio.file.Path)

  private[layout] def directoryTreeRowViews(
    rect: LayoutRect,
    tree: com.serenity.ui.layout.DirectoryTreeData,
    selectedPath: Option[java.nio.file.Path]
  ): List[DirectoryTreeRowView] =
    val visibleRows = com.serenity.ui.layout.DirectoryTreeData.visibleRows(tree)
    val maxRows     = math.max(1, rect.height - 2)
    visibleRows.take(maxRows).map { row =>
      val marker =
        if row.isDirectory then
          if row.isExpanded then "▾ "
          else if row.isLoaded then "▸ "
          else "▹ "
        else ""
      val indent = "  " * row.depth
      DirectoryTreeRowView(
        row = OverlayRow(
          plainText = s"$indent$marker${row.name}",
          selected = selectedPath.contains(row.path)
        ),
        path = row.path
      )
    }

  def resolveDirectoryTree(
    rect: LayoutRect,
    mode: SurfaceRenderMode,
    tree: com.serenity.ui.layout.DirectoryTreeData,
    selectedPath: Option[java.nio.file.Path]
  ): ResolvedSurfaceContent =
    ResolvedSurfaceContent(
      title = SurfaceContentResolver.titleFor(mode, tree.rootPath.getFileName.toString),
      rows = directoryTreeRowViews(rect, tree, selectedPath).map(_.row)
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

    ResolvedSurfaceContent(SurfaceContentResolver.titleFor(mode, "terminal"), rows = shaped.map(OverlayRow(_)))

  /** One rendered outline row paired with the index into `symbols` it represents, when the row is addressable at all --
    * `None` for the `Horizontal`/`Compact` summary rows, which have never been mouse-selectable
    * (`PinnedPanelMouseHitTesting.pinnedOutlineMouseHitAt` only ever resolves a hit for `Vertical`/`Square`). Shared by
    * `resolveOutline` (which only needs `row`) and `OutlineSurfaceComposition` (issue #819, slice 4), which also needs
    * `symbolIndex` to build a row's hit region, so both build the exact same row text from one place.
    */
  final private[layout] case class OutlineRowView(row: OverlayRow, symbolIndex: Option[Int])

  private[layout] def outlineRowViews(
    rect: LayoutRect,
    symbols: List[Symbol],
    activeLocation: Option[Location]
  ): List[OutlineRowView] =
    SurfaceLayoutKind.classify(rect) match
      case SurfaceLayoutKind.Horizontal =>
        val visibleSymbols = symbols.take(4)
        val activeVisible  = visibleSymbols.exists(symbol => activeLocation.contains(symbol.location))
        List(
          visibleSymbols
            .map(symbol => if activeLocation.contains(symbol.location) then s"[${symbol.name}]" else symbol.name)
            .mkString(" | ")
        ).filter(_.nonEmpty).map(text => OutlineRowView(OverlayRow(text, selected = activeVisible), None))
      case SurfaceLayoutKind.Vertical =>
        symbols.take(math.max(1, rect.height - 2)).zipWithIndex.map {
          case (symbol, index) =>
            val active = activeLocation.contains(symbol.location)
            val prefix = if active then "> " else ""
            OutlineRowView(OverlayRow(s"$prefix${symbol.kind} ${symbol.name}", selected = active), Some(index))
        }
      case SurfaceLayoutKind.Square =>
        symbols.take(math.max(1, rect.height - 2)).zipWithIndex.map {
          case (symbol, index) =>
            val active = activeLocation.contains(symbol.location)
            val prefix = if active then "> " else ""
            OutlineRowView(OverlayRow(s"$prefix${symbol.name}", selected = active), Some(index))
        }
      case SurfaceLayoutKind.Compact =>
        val current = activeLocation.flatMap(location => symbols.find(_.location == location)).map(_.name)
        current match
          case Some(name) =>
            List(
              OutlineRowView(OverlayRow(s"${symbols.length} symbols", selected = true), None),
              OutlineRowView(OverlayRow(name), None)
            )
          case None =>
            List(OutlineRowView(OverlayRow(s"${symbols.length} symbols"), None))

  def resolveOutline(
    rect: LayoutRect,
    mode: SurfaceRenderMode,
    symbols: List[Symbol],
    activeLocation: Option[Location]
  ): ResolvedSurfaceContent =
    ResolvedSurfaceContent(
      SurfaceContentResolver.titleFor(mode, "outline"),
      rows = outlineRowViews(rect, symbols, activeLocation).map(_.row)
    )

  /** One rendered comments row paired with the index into `symbols` it represents, when the row is addressable at all
    * -- `None` for the `Horizontal`/`Compact` summary rows, which have never been mouse-selectable
    * (`PinnedPanelMouseHitTesting.pinnedCommentsMouseHitAt` only ever resolves a hit for `Vertical`/`Square`). Shared
    * by `resolveComments` (which only needs `row`) and `CommentsSurfaceComposition` (issue #819, slice 5), which also
    * needs `symbolIndex` to build a row's hit region, so both build the exact same row text from one place.
    */
  final private[layout] case class CommentsRowView(row: OverlayRow, symbolIndex: Option[Int])

  private[layout] def commentsRowViews(
    rect: LayoutRect,
    symbols: List[Symbol],
    activeLocation: Option[Location]
  ): List[CommentsRowView] =
    SurfaceLayoutKind.classify(rect) match
      case SurfaceLayoutKind.Horizontal =>
        val visibleSymbols = symbols.take(4)
        val activeVisible  = visibleSymbols.exists(symbol => activeLocation.contains(symbol.location))
        List(
          visibleSymbols
            .map(symbol => if activeLocation.contains(symbol.location) then s"[${symbol.name}]" else symbol.name)
            .mkString(" | ")
        ).filter(_.nonEmpty).map(text => CommentsRowView(OverlayRow(text, selected = activeVisible), None))
      case SurfaceLayoutKind.Vertical | SurfaceLayoutKind.Square =>
        symbols.take(math.max(1, rect.height - 2)).zipWithIndex.map {
          case (symbol, index) =>
            val active = activeLocation.contains(symbol.location)
            val prefix = if active then "> " else ""
            CommentsRowView(OverlayRow(s"$prefix${symbol.name}", selected = active), Some(index))
        }
      case SurfaceLayoutKind.Compact =>
        val current = activeLocation.flatMap(location => symbols.find(_.location == location)).map(_.name)
        current match
          case Some(name) =>
            List(
              CommentsRowView(OverlayRow(s"${symbols.length} comments", selected = true), None),
              CommentsRowView(OverlayRow(name), None)
            )
          case None =>
            List(CommentsRowView(OverlayRow(s"${symbols.length} comments"), None))

  def resolveComments(
    rect: LayoutRect,
    mode: SurfaceRenderMode,
    symbols: List[Symbol],
    activeLocation: Option[Location]
  ): ResolvedSurfaceContent =
    ResolvedSurfaceContent(
      SurfaceContentResolver.titleFor(mode, "comments"),
      rows = commentsRowViews(rect, symbols, activeLocation).map(_.row)
    )

  /** One rendered diagnostics row paired with the index into `issues` it represents, when the row is addressable at all
    * -- `None` for `Horizontal`/`Compact`'s summary-only rows and `Square`'s leading count row, which have never been
    * mouse-selectable (`PinnedPanelMouseHitTesting.pinnedDiagnosticsMouseHitAt` only ever resolves a hit for `Vertical`
    * rows, or `Square` rows past the first). Shared by `resolveDiagnostics` (which only needs `row`) and
    * `DiagnosticsSurfaceComposition` (issue #819, slice 4), which also needs `issueIndex` to build a row's hit region,
    * so both build the exact same row text from one place.
    */
  final private[layout] case class DiagnosticsRowView(row: OverlayRow, issueIndex: Option[Int])

  private[layout] def diagnosticsRowViews(
    rect: LayoutRect,
    issues: List[com.serenity.ui.layout.Diagnostic],
    activeLocation: Option[Location]
  ): List[DiagnosticsRowView] =
    val errorCount   = issues.count(_.severity == com.serenity.ui.layout.DiagnosticSeverity.Error)
    val warningCount = issues.count(_.severity == com.serenity.ui.layout.DiagnosticSeverity.Warning)
    val infoCount = issues.count(issue =>
      issue.severity == com.serenity.ui.layout.DiagnosticSeverity.Info ||
        issue.severity == com.serenity.ui.layout.DiagnosticSeverity.Hint
    )
    SurfaceLayoutKind.classify(rect) match
      case SurfaceLayoutKind.Horizontal =>
        List(DiagnosticsRowView(OverlayRow(s"$errorCount error | $warningCount warning | $infoCount info"), None))
      case SurfaceLayoutKind.Vertical =>
        issues.take(math.max(1, rect.height - 2)).zipWithIndex.map {
          case (issue, index) =>
            DiagnosticsRowView(
              OverlayRow(s"${issue.severity}: ${issue.message}", selected = activeLocation.contains(issue.location)),
              Some(index)
            )
        }
      case SurfaceLayoutKind.Square =>
        DiagnosticsRowView(OverlayRow(s"$errorCount error, $warningCount warning"), None) ::
          issues.take(math.max(0, rect.height - 3)).zipWithIndex.map {
            case (issue, index) =>
              DiagnosticsRowView(
                OverlayRow(issue.message, selected = activeLocation.contains(issue.location)),
                Some(index)
              )
          }
      case SurfaceLayoutKind.Compact =>
        List(
          DiagnosticsRowView(OverlayRow(s"${issues.length} issues"), None),
          DiagnosticsRowView(OverlayRow(s"$errorCount error"), None)
        )

  def resolveDiagnostics(
    rect: LayoutRect,
    mode: SurfaceRenderMode,
    issues: List[com.serenity.ui.layout.Diagnostic],
    activeLocation: Option[Location]
  ): ResolvedSurfaceContent =
    ResolvedSurfaceContent(
      SurfaceContentResolver.titleFor(mode, "diagnostics"),
      rows = diagnosticsRowViews(rect, issues, activeLocation).map(_.row)
    )

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
    ResolvedSurfaceContent(SurfaceContentResolver.titleFor(mode, "Keyboard Shortcuts"), rows = rows)

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
    ResolvedSurfaceContent(SurfaceContentResolver.titleFor(mode, "Tabs"), rows = rows)

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
    ResolvedSurfaceContent(SurfaceContentResolver.titleFor(mode, s"Recent in ${recentMode.toString} Mode"), rows = rows)
