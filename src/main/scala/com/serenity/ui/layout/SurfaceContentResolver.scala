package com.serenity.ui.layout

import scala.annotation.unused

import com.serenity.markdown.{MarkdownDocumentPreview, MarkdownPreviewCache}
import com.serenity.rope.Rope
import com.serenity.state.models.*
import com.serenity.ui.color.RenderColor
import com.serenity.ui.layout.*

enum SurfaceRenderMode:
  case Floating
  case Pinned

enum OverlayTone:
  case Normal
  case Muted
  case Error
  case Accent

enum OverlayRowLayout:
  case Plain
  case Distributed
  case Split
  case Columns
  case PriorityColumns

final case class OverlaySegment(
    text: String,
    selected: Boolean = false,
    tone: OverlayTone = OverlayTone.Normal,
    foregroundColor: Option[RenderColor] = None,
    backgroundColor: Option[RenderColor] = None,
    fontFamily: Option[String] = None,
    inlineIcon: Option[String] = None,
    inlineIconFontFamily: Option[String] = None,
    trailingSeparator: Boolean = false,
    allocatedWidth: Option[Int] = None
)

final case class OverlayRow(
    plainText: String,
    selected: Boolean = false,
    cursorColumn: Option[Int] = None,
    foregroundColor: Option[RenderColor] = None,
    backgroundColor: Option[RenderColor] = None,
    segments: List[OverlaySegment] = Nil,
    layout: OverlayRowLayout = OverlayRowLayout.Plain,
    leadingPadding: Int = 0
)

final case class ResolvedSurfaceContent(
    title: Option[String] = None,
    header: Option[OverlayRow] = None,
    rows: List[OverlayRow] = Nil,
    footer: Option[OverlayRow] = None,
    // Persistent key-hint chrome row (issue #931, Stage 3) -- distinct from `footer`, which stays the transient
    // status-message slot. Only the command palette and settings surface ever populate this, and only when
    // `resolve`'s `showKeyHints` is true; every other content kind leaves it `None`.
    keyHintRow: Option[OverlayRow] = None
)

/** Turns a `SurfaceContent` into the plain overlay rows a renderer paints, independent of any particular render
  * surface. The individual content kinds' resolution logic lives in sibling `*ContentResolver` objects in this package
  * -- this file keeps only the dispatcher, the shared data model above, and the handful of helpers (`titleFor` chief
  * among them) every one of those siblings calls back into.
  */
object SurfaceContentResolver:

  def resolve(
    content: SurfaceContent,
    rect: LayoutRect,
    mode: SurfaceRenderMode,
    itemGapRows: Double = 0.0,
    itemTargetRows: Int = 1,
    // Whether the command runner's persistent key-hint footer (issue #931, Stage 3) should be populated for
    // `CommandPalette` content. Defaults to off so every caller that does not pass it explicitly -- including every
    // pre-existing test -- keeps the pre-Stage-3 single dynamic-footer behaviour unchanged; production call sites
    // pass `AppConfig.surfaceConfig.commandRunnerShowKeyHints` explicitly.
    showKeyHints: Boolean = false
  ): ResolvedSurfaceContent =
    content match
      case SurfaceContent.StartPage(_) =>
        ResolvedSurfaceContent()
      case SurfaceContent.QuickInfo(text) =>
        ResolvedSurfaceContent(
          title = None,
          rows = text.linesIterator.toList match
            case Nil   => List(OverlayRow(""))
            case lines => lines.map(OverlayRow(_))
        )
      case SurfaceContent.FilePreview(path, content) =>
        ResolvedSurfaceContent(
          titleFor(mode, s"Preview: ${path.getFileName}"),
          rows = content.linesIterator.take(4).toList.map(OverlayRow(_))
        )
      case SurfaceContent.SymbolDefinition(symbol, location) =>
        ResolvedSurfaceContent(
          titleFor(mode, "symbol"),
          rows = List(
            OverlayRow(s"Symbol: $symbol"),
            OverlayRow(s"Line ${location.line + 1}, Col ${location.column + 1}")
          )
        )
      case SurfaceContent.StatusLine(text) =>
        ResolvedSurfaceContent(rows = List(OverlayRow(text)))
      case SurfaceContent.DirectoryListing(path, entries, selectedPath) =>
        PanelContentResolver.resolveDirectoryListing(
          rect,
          mode,
          path.getFileName.toString,
          entries.map(_.name),
          selectedPath.flatMap(p => Option(p.getFileName).map(_.toString))
        )
      case SurfaceContent.DirectoryTree(tree, selectedPath, scroll) =>
        PanelContentResolver.resolveDirectoryTree(rect, mode, tree, selectedPath, scroll)
      case SurfaceContent.CommandPalette(runner) =>
        // `OverlayViewModel.contentView` bypasses this call entirely for `CommandPalette` (issue #819, slice 2):
        // painting is done via `CommandRunnerSurfaceComposition`, and `TextOverlayRenderer` ignores `rows` whenever
        // `composition` is set, which it always is there. This dispatcher still resolves it for real, though --
        // `EditorLayoutContract.floatingGeometry` calls `resolve` independently and genuinely needs these real,
        // item-count-accurate rows/header/footer for its own (non-composition) row-slot/header-rect geometry.
        CommandPaletteContentResolver.resolveCommandPalette(
          runner,
          rect,
          mode,
          itemGapRows,
          itemTargetRows,
          showKeyHints
        )
      case SurfaceContent.CommandRunnerPeek(runner) =>
        // Cursor-peek prototype: same rendering as CommandPalette, reused as-is (see UiSurface.scala's doc comment
        // on why this is a distinct SurfaceContent case rather than the same one).
        CommandPaletteContentResolver.resolveCommandPalette(
          runner,
          rect,
          mode,
          itemGapRows,
          itemTargetRows,
          showKeyHints
        )
      case SurfaceContent.ModalWorkflow(_) =>
        // Painted entirely via `ModalSurfaceComposition` (issue #819), not this plain-rows path -- mirrors
        // `TabBar`'s own empty fallback above.
        ResolvedSurfaceContent()
      case SurfaceContent.Terminal(buffer, cursor) =>
        PanelContentResolver.resolveTerminal(rect, mode, buffer, cursor)
      case SurfaceContent.Outline(symbols, activeLocation, scroll) =>
        PanelContentResolver.resolveOutline(rect, mode, symbols, activeLocation, scroll)
      case SurfaceContent.Comments(symbols, activeLocation, scroll) =>
        PanelContentResolver.resolveComments(rect, mode, symbols, activeLocation, scroll)
      case SurfaceContent.Diagnostics(issues, activeLocation, scroll) =>
        PanelContentResolver.resolveDiagnostics(rect, mode, issues, activeLocation, scroll)
      case SurfaceContent.ShortcutsHelp(groups) =>
        PanelContentResolver.resolveShortcutsHelp(rect, mode, groups)
      case SurfaceContent.TabList(entries, activeBufferId) =>
        PanelContentResolver.resolveTabList(rect, mode, entries, activeBufferId)
      case SurfaceContent.RecentFilesInMode(recentMode, paths) =>
        PanelContentResolver.resolveRecentFilesInMode(rect, mode, recentMode, paths)
      case SurfaceContent.ThemeCreator(state) =>
        PickerContentResolver.resolveThemeCreator(state, rect, mode)
      case SurfaceContent.ContextualToolbar(_) =>
        ResolvedSurfaceContent()
      case SurfaceContent.TabBar(_, _) =>
        // Painted entirely via `TabBarSurfaceComposition` (issue #1075/#1076), not this plain-rows path -- mirrors
        // `ContextualToolbar`'s own empty fallback just above.
        ResolvedSurfaceContent()
      case SurfaceContent.ContextMenu(menu) =>
        // `OverlayViewModel.contentView` bypasses this call entirely for `ContextMenu` too (issue #819, slice 2), for
        // the same reason and with the same `EditorLayoutContract` caveat as `CommandPalette` just above.
        PickerContentResolver.resolveContextMenu(menu, rect, mode, itemGapRows)
      case SurfaceContent.CommentLens(lens) =>
        ResolvedSurfaceContent(
          title = titleFor(mode, "comment"),
          header = Some(OverlayRow(lens.headline)),
          rows = commentLensRows(lens)
        )
      case SurfaceContent.MarkdownPreview(_, title) =>
        ResolvedSurfaceContent(title = titleFor(mode, s"Preview: $title"))
      case SurfaceContent.Notice(notice, _) =>
        NoticeContent.resolve(notice, rect)

  private[layout] def titleFor(mode: SurfaceRenderMode, title: String): Option[String] =
    mode match
      case SurfaceRenderMode.Floating => None
      case SurfaceRenderMode.Pinned   => Some(title)

  /** Shared with `CommentLensSurfaceComposition` (issue #819, slice 3) so the composed paint plan and this dispatcher's
    * own plain-rows fallback (still the real, live path for `EditorLayoutContract.floatingGeometry`) build identical
    * rows from one place.
    */
  private[layout] def commentLensRows(lens: CommentLensState): List[OverlayRow] =
    val (cursorLine, cursorColumn) = lineAndColumnAt(lens.draft, lens.clampedCursor)
    val draftRows = splitLines(lens.draft).zipWithIndex.map { (line, index) =>
      OverlayRow(
        plainText = line,
        selected = index == cursorLine,
        cursorColumn = Option.when(index == cursorLine)(cursorColumn)
      )
    }
    draftRows ++ lens.threadLines.map(OverlayRow(_))

  private def splitLines(text: String): List[String] =
    text.split("\n", -1).toList match
      case Nil => List("")
      case xs  => xs

  private def lineAndColumnAt(text: String, cursor: Int): (Int, Int) =
    text.take(math.max(0, math.min(cursor, text.length))).foldLeft((0, 0)) {
      case ((line, _), '\n') => (line + 1, 0)
      case ((line, col), _)  => (line, col + 1)
    }

  def resolveContextualToolbar(
    toolbarState: ContextualToolbarState,
    state: AppState,
    rect: LayoutRect,
    @unused mode: SurfaceRenderMode
  ): ResolvedSurfaceContent =
    ContextualToolbarContentResolver.resolve(toolbarState, state, rect, mode)

  /** `cache` defaults to a freshly constructed [[MarkdownPreviewCache]] (issue #1677) rather than threading a
    * per-`StateManager` instance down from [[com.serenity.state.manager.RenderCaches]]: this method is reached from
    * pure layout/geometry computation with no render-cache instance in scope -- `EditorLayoutContract.pinnedGeometry`
    * (itself called from `PinnedPanelMouseHitTesting`, which carries no `RenderContext`) -- as well as from the one
    * paint-time caller that does, [[com.serenity.ui.renderer.PinnedPanelViewModel.resolve]]. The cache here is pure
    * memoization of inline line splitting with no bearing on correctness, so a caller without a real instance to pass
    * loses only the caching benefit for this one text-preview path, never a wrong result.
    */
  def resolveMarkdownPreview(
    title: String,
    content: String,
    rect: LayoutRect,
    mode: SurfaceRenderMode,
    cache: MarkdownPreviewCache = MarkdownPreviewCache()
  ): ResolvedSurfaceContent =
    val contentRows = SurfaceFrameLayout(rect).contentRect.height.max(0)
    val rows =
      MarkdownDocumentPreview
        .renderInlineLines(content.linesIterator.toVector, cache)
        .take(contentRows)
        .filter(_.trim.nonEmpty)
        .map(OverlayRow(_))
        .toList
    ResolvedSurfaceContent(
      title = titleFor(mode, s"Preview: $title"),
      rows = rows
    )

  /** [[resolveMarkdownPreview]] for a buffer's `Rope`: the same rows, but the text is read only when this document has
    * not been resolved at this panel height before, so resolving an unchanged document on every frame and for every
    * layout contract costs no pass over it. A missing buffer resolves to an empty preview.
    */
  def resolveBufferMarkdownPreview(
    title: String,
    content: Option[Rope],
    rect: LayoutRect,
    mode: SurfaceRenderMode,
    cache: MarkdownPreviewCache = MarkdownPreviewCache()
  ): ResolvedSurfaceContent =
    val contentRows = SurfaceFrameLayout(rect).contentRect.height.max(0)
    ResolvedSurfaceContent(
      title = titleFor(mode, s"Preview: $title"),
      rows = content
        .fold(Vector.empty[String])(MarkdownDocumentPreview.panelPreviewRows(_, contentRows, cache))
        .map(OverlayRow(_))
        .toList
    )
