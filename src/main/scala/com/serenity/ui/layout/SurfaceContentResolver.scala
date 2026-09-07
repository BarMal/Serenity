package com.serenity.ui.layout

import java.awt.Color

import com.serenity.markdown.MarkdownDocumentPreview
import com.serenity.state.models.*
import com.serenity.ui.layout.*

enum SurfaceRenderMode:
  case Floating
  case Pinned

  /** A pinned surface paints its name in the frame chrome; a floating one has no title bar to paint it in, so every
    * resolver drops the title it would otherwise supply. Lives on the mode itself because that is the only thing the
    * answer depends on -- resolvers ask `mode.titleFor(...)` rather than routing through a shared helper.
    */
  def titleFor(title: String): Option[String] = this match
    case Floating => None
    case Pinned   => Some(title)

enum OverlayTone:
  case Normal
  case Muted
  case Error

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
    foregroundColor: Option[Color] = None,
    backgroundColor: Option[Color] = None,
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
    foregroundColor: Option[Color] = None,
    backgroundColor: Option[Color] = None,
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
  * surface. This object is the dispatcher and the home of the handful of trivially-shaped content kinds; each of the
  * substantial ones is resolved by its own `*ContentResolver` in this package, which the match below delegates to.
  * Nothing in a sibling resolver reaches back here -- the data model above and `SurfaceRenderMode.titleFor` are what
  * they share.
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
          mode.titleFor(s"Preview: ${path.getFileName}"),
          rows = content.linesIterator.take(4).toList.map(OverlayRow(_))
        )
      case SurfaceContent.SymbolDefinition(symbol, location) =>
        ResolvedSurfaceContent(
          mode.titleFor("symbol"),
          rows = List(
            OverlayRow(s"Symbol: $symbol"),
            OverlayRow(s"Line ${location.line + 1}, Col ${location.column + 1}")
          )
        )
      case SurfaceContent.CursorInfoBar(text) =>
        ResolvedSurfaceContent(rows = List(OverlayRow(text)))
      case SurfaceContent.DirectoryListing(path, entries, selectedPath) =>
        PanelContentResolver.resolveDirectoryListing(
          rect,
          mode,
          path.getFileName.toString,
          entries.map(_.name),
          selectedPath.flatMap(p => Option(p.getFileName).map(_.toString))
        )
      case SurfaceContent.DirectoryTree(tree, selectedPath) =>
        PanelContentResolver.resolveDirectoryTree(rect, mode, tree, selectedPath)
      case SurfaceContent.CommandPalette(runner) =>
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
      case SurfaceContent.ModalWorkflow(modal) =>
        ModalWorkflowContentResolver.resolve(modal, rect, mode)
      case SurfaceContent.Terminal(buffer, cursor) =>
        PanelContentResolver.resolveTerminal(rect, mode, buffer, cursor)
      case SurfaceContent.Outline(symbols, activeLocation) =>
        PanelContentResolver.resolveOutline(rect, mode, symbols, activeLocation)
      case SurfaceContent.Comments(symbols, activeLocation) =>
        PanelContentResolver.resolveComments(rect, mode, symbols, activeLocation)
      case SurfaceContent.Diagnostics(issues, activeLocation) =>
        PanelContentResolver.resolveDiagnostics(rect, mode, issues, activeLocation)
      case SurfaceContent.ShortcutsHelp(groups) =>
        PanelContentResolver.resolveShortcutsHelp(rect, mode, groups)
      case SurfaceContent.TabList(entries, activeBufferId) =>
        PanelContentResolver.resolveTabList(rect, mode, entries, activeBufferId)
      case SurfaceContent.RecentFilesInMode(recentMode, paths) =>
        PanelContentResolver.resolveRecentFilesInMode(rect, mode, recentMode, paths)
      case SurfaceContent.ThemePicker(state) =>
        PickerContentResolver.resolveThemePicker(state, rect, mode)
      case SurfaceContent.ThemeCreator(state) =>
        PickerContentResolver.resolveThemeCreator(state, rect, mode)
      case SurfaceContent.FileSearch(state) =>
        PickerContentResolver.resolveFileSearch(state, rect, mode)
      case SurfaceContent.ContextualToolbar(_) =>
        ResolvedSurfaceContent()
      case SurfaceContent.ContextMenu(menu) =>
        PickerContentResolver.resolveContextMenu(menu, rect, mode, itemGapRows)
      case SurfaceContent.CommentLens(lens) =>
        ResolvedSurfaceContent(
          title = mode.titleFor("comment"),
          header = Some(OverlayRow("comment")),
          rows = commentLensRows(lens)
        )
      case SurfaceContent.MarkdownPreview(_, title) =>
        ResolvedSurfaceContent(title = mode.titleFor(s"Preview: $title"))
      case SurfaceContent.CompanionSprite =>
        // Painted directly by Renderer's dedicated companion-sprite paint step (surface.pixels.drawImage), not
        // through this cell-text path -- see the doc comment on SurfaceContent.CompanionSprite.
        ResolvedSurfaceContent()
      case SurfaceContent.GhostOverlay(originalContent, cachedRect) =>
        resolve(originalContent, cachedRect, mode, itemGapRows)

  private def commentLensRows(lens: CommentLensState): List[OverlayRow] =
    val (cursorLine, cursorColumn) = lineAndColumnAt(lens.draft, lens.clampedCursor)
    splitLines(lens.draft).zipWithIndex.map { (line, index) =>
      OverlayRow(
        plainText = line,
        selected = index == cursorLine,
        cursorColumn = Option.when(index == cursorLine)(cursorColumn)
      )
    }

  private def splitLines(text: String): List[String] =
    text.split("\n", -1).toList match
      case Nil => List("")
      case xs  => xs

  private def lineAndColumnAt(text: String, cursor: Int): (Int, Int) =
    text.take(math.max(0, math.min(cursor, text.length))).foldLeft((0, 0)) {
      case ((line, _), '\n') => (line + 1, 0)
      case ((line, col), _)  => (line, col + 1)
    }

  def resolveMarkdownPreview(
    title: String,
    content: String,
    rect: LayoutRect,
    mode: SurfaceRenderMode
  ): ResolvedSurfaceContent =
    val contentRows = SurfaceFrameLayout(rect).contentRect.height.max(0)
    val rows =
      MarkdownDocumentPreview
        .renderInlineLines(content.linesIterator.toVector)
        .take(contentRows)
        .filter(_.trim.nonEmpty)
        .map(OverlayRow(_))
        .toList
    ResolvedSurfaceContent(
      title = mode.titleFor(s"Preview: $title"),
      rows = rows
    )
