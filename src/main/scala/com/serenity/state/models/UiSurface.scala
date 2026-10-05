package com.serenity.state.models

import java.nio.file.Path

import com.serenity.command.{Command, CommandRunner, SessionIntent}
import com.serenity.config.{AppMode, CornerPosition}
import com.serenity.document.RenderedComment
import com.serenity.ui.layout.*
import com.serenity.ui.theme.config.ThemeCreatorState
import com.serenity.ui.widget.ListScroll

opaque type SurfaceId = String

object SurfaceId:
  def apply(value: String): SurfaceId      = value
  def unapply(id: SurfaceId): Some[String] = Some(id)

  extension (id: SurfaceId) def value: String = id

  /** Reserved id for the experimental command-runner cursor-peek prototype's single peek panel
    * (`SurfaceConfig.commandRunnerCursorPeekEnabled`) -- a fixed constant rather than an allocated id, since this
    * single-panel prototype only ever has the one peek surface at a time. `LayoutEngine.calculateFloatingSurfaceRect`
    * checks for this id to route the surface through the frozen-anchor stack (`resolveFrozenCursorPeekStack`) instead
    * of the live floating-anchor path every other floating surface uses.
    */
  val CursorPeek: SurfaceId = SurfaceId("command-runner-cursor-peek")

  /** Reserved id for the toggleable keyboard-shortcuts reference (issue #1247) -- a fixed constant like [[CursorPeek]]
    * rather than an allocated id, since at most one instance of this surface exists at a time and
    * `AppEventReducer.toggleShortcutsHelp` looks it up by id to decide whether to open or close it.
    */
  val ShortcutsHelp: SurfaceId = SurfaceId("shortcuts-help")

  /** Reserved ids for the mode/tab corner widget's two summonable lists (issue #1307) -- fixed constants for the same
    * reason as [[ShortcutsHelp]]: at most one instance of each exists at a time, and the toggle reducers look them up
    * by id.
    */
  val TabList: SurfaceId = SurfaceId("tab-list")

  val RecentFilesInMode: SurfaceId = SurfaceId("recent-files-in-mode")

/** An executable option displayed on the startup launch surface. */
enum StartupActionSection:
  case Session
  case Workflow

final case class StartupAction(
    id: String,
    label: String,
    command: Command,
    shortcut: Option[Char] = None,
    detail: Option[String] = None,
    section: StartupActionSection = StartupActionSection.Session
):

  def renderedLabel: String =
    val prefix = shortcut.fold("")(key => s"[$key] ")
    val suffix = detail.fold("")(value => s"  $value")
    s"$prefix$label$suffix"

/** The "quick resume last session" affordance: a keyboard hint (Tab) pinned to the bottom of the startup page, showing
  * the previous session's identifier (its main file/dir name) so the user can recognise what they'd be resuming.
  */
final case class StartupResumeHint(identifier: String, command: Command)

/** Pixel-space click target for an action rendered on the startup page. */
final case class StartupActionBounds(index: Int, xPx: Int, yPx: Int, widthPx: Int, heightPx: Int):
  def contains(pixelX: Int, pixelY: Int): Boolean =
    pixelX >= xPx && pixelX < xPx + widthPx && pixelY >= yPx && pixelY < yPx + heightPx

final case class StartupPage(
    title: String,
    options: List[String] = Nil,
    statusMessage: Option[String] = None,
    selectedIndex: Int = 0,
    actions: List[StartupAction] = Nil,
    // Workflow presets are not part of the navigable list -- they render as a keyboard-shortcut hint pinned to the
    // bottom of the screen (activated by their `shortcut` letter), so they stay out of `launchActions`.
    workflows: List[StartupAction] = Nil,
    // Quick-resume (Tab) also lives in the bottom hint area rather than the navigable list, on its own line.
    resume: Option[StartupResumeHint] = None
):

  private def legacyActions: List[StartupAction] =
    options.zipWithIndex.map {
      case (label, index) =>
        val (id, command) = index match
          case 0 =>
            "new-session" -> Command.typed(
              "startup.new-session",
              "Start a new session",
              com.serenity.command.CommandIntent.Session(SessionIntent.StartupNewSession)
            )
          case 1 =>
            "restore-session" -> Command.typed(
              "startup.restore-session",
              "Restore an existing session",
              com.serenity.command.CommandIntent.Session(SessionIntent.StartupRestoreSession)
            )
          case 2 =>
            "open-file" -> Command.typed(
              "startup.open-file",
              "Open an existing file or directory",
              com.serenity.command.CommandIntent.Session(SessionIntent.StartupOpenFile)
            )
          case _ =>
            s"option-$index" -> Command.typed(
              "startup.new-session",
              "Start a new session",
              com.serenity.command.CommandIntent.Session(SessionIntent.StartupNewSession)
            )
        StartupAction(id, label, command)
    }

  def launchActions: List[StartupAction] =
    if actions.nonEmpty then actions else legacyActions

  def selectedAction: Option[StartupAction] =
    launchActions.lift(selectedIndex)

  /** Zero-based render-line index for each launch action, including section spacing and headings. */
  def actionLineIndices: List[Int] =
    launchActions.zipWithIndex
      .foldLeft((List.empty[Int], 2, Option.empty[StartupActionSection])) {
        case ((indices, nextLine, previousSection), (action, _)) =>
          val sectionLines =
            if previousSection.exists(_ != action.section) then 2 else 0
          (indices :+ (nextLine + sectionLines), nextLine + sectionLines + 1, Some(action.section))
      }
      ._1

  def actionBounds(
    viewportSize: ViewportSize,
    codeMetrics: CellMetrics,
    uiMetrics: CellMetrics
  ): List[StartupActionBounds] =
    val lineHeightPx     = math.max(codeMetrics.lineHeight, uiMetrics.lineHeight)
    val viewportWidthPx  = viewportSize.width * codeMetrics.charWidth
    val viewportHeightPx = viewportSize.height * codeMetrics.lineHeight
    val startYPx         = math.max(0, (viewportHeightPx - (renderLines.size * lineHeightPx)) / 2)

    launchActions.zip(actionLineIndices).zipWithIndex.flatMap {
      case ((action, lineIndex), index) =>
        val widthPx = math.min(viewportWidthPx, (action.renderedLabel.length + 4) * codeMetrics.charWidth)
        val yPx     = startYPx + (lineIndex * lineHeightPx)
        Option.when(yPx + lineHeightPx > 0 && yPx < viewportHeightPx)(
          StartupActionBounds(
            index = index,
            xPx = math.max(0, (viewportWidthPx - widthPx) / 2),
            yPx = yPx,
            widthPx = widthPx,
            heightPx = lineHeightPx
          )
        )
    }

  def actionIndexAtPixel(
    pixelX: Int,
    pixelY: Int,
    viewportSize: ViewportSize,
    codeMetrics: CellMetrics,
    uiMetrics: CellMetrics
  ): Option[Int] =
    actionBounds(viewportSize, codeMetrics, uiMetrics).find(_.contains(pixelX, pixelY)).map(_.index)

  def renderLines: List[String] =
    val actionLines = launchActions.map(_.renderedLabel)
    val baseLines   = List(title, "") ++ actionLines
    statusMessage match
      case Some(message) => baseLines ++ List("", message, "", "↑↓ Navigate  •  Enter Select  •  Esc Close")
      case None          => baseLines ++ List("", "↑↓ Navigate  •  Enter Select  •  Esc Close")

  def withSelectedIndex(index: Int): StartupPage =
    val clampedIndex =
      if launchActions.isEmpty then 0 else ((index % launchActions.size) + launchActions.size) % launchActions.size
    copy(selectedIndex = clampedIndex)

  def moveSelectionUp: StartupPage =
    withSelectedIndex(selectedIndex - 1)

  def moveSelectionDown: StartupPage =
    withSelectedIndex(selectedIndex + 1)

final case class ContextMenuItem(
    id: String,
    label: String,
    command: Command
)

final case class ContextMenu(
    title: String,
    targetFocus: Focus,
    items: List[ContextMenuItem],
    selectedIndex: Int = 0
):

  def selectedItem: Option[ContextMenuItem] =
    items.lift(selectedIndex)

  def withSelectedIndex(index: Int): ContextMenu =
    val clampedIndex = if items.isEmpty then 0 else ((index % items.size) + items.size) % items.size
    copy(selectedIndex = clampedIndex)

/** One bound key combination (or the handful bound to the same action) for one action, rendered for the shortcuts-help
  * reference (issue #1247). `keys` is already display text (e.g. "ctrl+s"), produced by `ShortcutsHelpContent` from
  * `HotkeyTrigger.render` -- this case class carries no live config so it stays a plain, renderer-friendly snapshot.
  */
final case class ShortcutHelpEntry(label: String, keys: String)

/** One named section of the shortcuts-help reference, e.g. "Global" or "Editor". */
final case class ShortcutHelpGroup(title: String, entries: List[ShortcutHelpEntry])

/** One row of the mode/tab corner widget's tab list (issue #1307): `title` is the buffer's display name (a file's name,
  * or `Buffer <id>` for one not yet saved to disk) -- never the full path, matching
  * `RendererPaneContent.renderBufferHeader`'s existing per-pane header title.
  */
final case class TabListEntry(bufferId: BufferId, title: String, isDirty: Boolean)

enum SurfacePlacement:
  case AboveCursor
  case BelowCursor

  /** A screen corner, for a floating overlay panel assigned an edge/corner position (issue #1310, mode 3) rather than
    * anchored to the cursor -- laid out by `LayoutEngine.calculateCornerOverlayStack`, stacking as a list when more
    * than one surface shares the same corner.
    */
  case Corner(position: CornerPosition)

enum SurfacePresentation:
  case Floating(anchor: Option[CursorPosition], placement: SurfacePlacement)

  /** A surface docked into the workspace tree (issue #817) -- carries no position or size of its own: both are owned
    * exclusively by `Layout.workspaceTree` (a `WorkspaceNode.DockedSurface`'s position, and its owning
    * `WorkspaceNode.Split.ratio` for size), the single source of truth for every docked surface's placement.
    * "Expanded/maximized" is likewise owned solely by `Layout.maximizedWorkspaceNodeId`, not a separate presentation.
    */
  case Docked

/** Whether the above-cursor comment lens is a passive, read-only display or the existing always-editable draft state
  * (#1222). `ReadOnly` is reached by clicking a highlighted comment range in floating display mode; a further click
  * inside the lens body transitions it to `Editable`. Every keyboard-invoked open (the `comment-lens` command) opens
  * directly into `Editable`. A source-code comment is always `ReadOnly` (see [[CommentLensState.withMode]]). A
  * `ReadOnly` lens is a [[SurfaceFocusPolicy.Peek]], so it never takes focus from the editor (#1674).
  */
enum CommentLensMode:
  case ReadOnly
  case Editable

/** The authored comment a lens writes back to. `index` (its position in the buffer's `documentComments`) is what
  * identifies it: two comments can be structurally equal, and an edit elsewhere shifts a comment's range without moving
  * its position in the list. `comment` is the snapshot the draft was opened from.
  */
final case class CommentLensTarget(index: Int, comment: DocumentComment)

/** Focused draft state for editing an authored document comment from the above-cursor lens. */
final case class CommentLensState(
    comment: RenderedComment,
    draft: String,
    cursor: Int,
    target: Option[CommentLensTarget],
    mode: CommentLensMode = CommentLensMode.Editable
):
  def clampedCursor: Int =
    math.max(0, math.min(cursor, draft.length))

  /** A source-code comment has no authored target to write a draft back to, so it stays read-only whatever mode is
    * requested -- an editable lens on it would discard everything typed (#1914).
    */
  def withMode(requested: CommentLensMode): CommentLensState =
    copy(mode = if target.isDefined then requested else CommentLensMode.ReadOnly)

enum SurfaceContent:
  case StartPage(page: StartupPage)
  case QuickInfo(text: String)
  case FilePreview(path: Path, content: String)
  case SymbolDefinition(symbol: String, location: Location)
  case StatusLine(text: String)
  case DirectoryListing(path: Path, entries: List[DirEntry], selectedPath: Option[Path] = None)

  /** `scroll` is where the list is scrolled to, kept apart from the selection -- see [[ListScroll]]. */
  case DirectoryTree(tree: DirectoryTreeData, selectedPath: Option[Path] = None, scroll: ListScroll = ListScroll())
  case CommandPalette(runner: CommandRunner)

  /** The experimental command-runner cursor-peek prototype's single peek panel (`SurfaceId.CursorPeek`,
    * `SurfaceConfig.commandRunnerCursorPeekEnabled`) -- deliberately a distinct content case from `CommandPalette`, not
    * the same case reused with a different id: `AppState.commandRunnerSurface` matches *any* `CommandPalette` surface
    * regardless of id, so reusing that case here would make the peek panel masquerade as "the command runner is already
    * open" the moment it appears, breaking `AppEventReducer.openCommandRunnerFully`'s already-open check. Renders via
    * the same `CommandPalette` machinery in `SurfaceContentResolver` and `LayoutEngine`'s frame sizing, just under this
    * separate case, so no other command-runner-specific code path (focus routing, mouse hit-testing, keymap recording,
    * ...) mistakes it for the real, interactive command runner.
    */
  case CommandRunnerPeek(runner: CommandRunner)
  case ThemeCreator(state: ThemeCreatorState)
  case ContextualToolbar(state: ContextualToolbarState)
  case ContextMenu(menu: com.serenity.state.models.ContextMenu)
  case CommentLens(state: CommentLensState)
  case MarkdownPreview(bufferId: BufferId, title: String)
  case ModalWorkflow(modal: Modal)
  case Terminal(buffer: String, cursor: Int)
  case Outline(symbols: List[Symbol], activeLocation: Option[Location] = None, scroll: ListScroll = ListScroll())
  case Comments(symbols: List[Symbol], activeLocation: Option[Location] = None, scroll: ListScroll = ListScroll())
  case Diagnostics(issues: List[Diagnostic], activeLocation: Option[Location] = None, scroll: ListScroll = ListScroll())

  /** The toggleable keyboard-shortcuts reference (issue #1247) -- a snapshot of `ShortcutsHelpContent.build`, taken
    * when `AppEventReducer.toggleShortcutsHelp` opens the surface, not re-derived on every render.
    */
  case ShortcutsHelp(groups: List[ShortcutHelpGroup])

  /** The mode/tab corner widget's tab-list summon (issue #1307) -- a snapshot of `TabListContent.build`, taken when
    * `AppEventReducer.toggleTabList` opens the surface, not re-derived on every render (same trade-off as
    * [[ShortcutsHelp]]: staying open across an edit that adds or closes a tab shows the list as of when it opened).
    */
  case TabList(entries: List[TabListEntry], activeBufferId: Option[BufferId])

  /** The always-visible, mouse-interactive tab strip (issue #1074 epic, #1075/#1076) -- distinct from [[TabList]] (that
    * popup stays a keyboard-toggled vertical list). Carries the same `TabListEntry` snapshot data as `TabList`, but is
    * resolved into a `SurfaceComposition` (`TabBarSurfaceComposition`) painted as one horizontal `Distributed` row
    * rather than a bordered panel of rows.
    */
  case TabBar(entries: List[TabListEntry], activeBufferId: Option[BufferId])

  /** The mode/tab corner widget's "recent in this mode" summon (issue #1307) -- a snapshot of
    * `RecentFilesInModeContent.build` for whichever `AppMode` was active when it was opened.
    */
  case RecentFilesInMode(mode: AppMode, paths: List[java.nio.file.Path])

/** How a surface shares the keyboard with the editor beneath it (#1940). */
enum SurfaceFocusPolicy:

  /** Shown without ever taking focus, so every key still reaches what had it. Escape closes it; with `dismissOnMove`,
    * so does any other key, which then carries on as if the peek were not there.
    */
  case Peek

  /** Takes focus; a key it leaves unhandled goes on to the editor pane. */
  case Focusable

  /** Takes focus and keeps every key, handled or not. */
  case Modal

final case class UiSurface(
    id: SurfaceId,
    content: SurfaceContent,
    presentation: SurfacePresentation,
    dismissOnMove: Boolean = false
):

  /** Derived from what the surface shows rather than stored, so a comment lens's policy cannot drift from its mode. */
  def focusPolicy: SurfaceFocusPolicy =
    content match
      case SurfaceContent.CommentLens(lens) if lens.mode == CommentLensMode.ReadOnly => SurfaceFocusPolicy.Peek
      case SurfaceContent.CommandPalette(_) | SurfaceContent.ModalWorkflow(_) | SurfaceContent.ThemeCreator(_) =>
        SurfaceFocusPolicy.Modal
      case _ if isFloatingPeek => SurfaceFocusPolicy.Peek
      case _                   => SurfaceFocusPolicy.Focusable

  /** A floating peek (`PeekStateReducer`), recognised by what it shows: the comment lens and the command runner's
    * cursor peek float above the cursor too, and a peek pinned as a docked panel is no longer one.
    */
  def isFloatingPeek: Boolean =
    (presentation, content) match
      case (
            SurfacePresentation.Floating(_, _),
            SurfaceContent.QuickInfo(_) | SurfaceContent.FilePreview(_, _) | SurfaceContent.SymbolDefinition(_, _) |
            SurfaceContent.DirectoryListing(_, _, _)
          ) =>
        true
      case _ => false

object UiSurface:

  /** Identity of the derived floating status row `AppState.floatingStatusLineSurface` synthesizes each frame -- the
    * one, fixed id every consumer that needs to recognize that specific surface (rather than any floating panel in
    * general) keys off, e.g. `TextOverlayRenderer`'s colour override and its shadow-free painting.
    */
  val StatusLineSurfaceId: SurfaceId = SurfaceId("status-line")

  /** Identity of the derived tab strip `AppState.tabBarSurface` synthesizes each frame -- the one, fixed id
    * `TextOverlayRenderer` (and anything else that needs to recognize this specific surface rather than any floating
    * panel in general) keys off, the same role [[StatusLineSurfaceId]] plays for the status row.
    */
  val TabBarSurfaceId: SurfaceId = SurfaceId("tab-bar")

  def fromPanelContent(id: SurfaceId, content: PanelContent): UiSurface =
    UiSurface(
      id = id,
      content = content.asSurfaceContent,
      presentation = SurfacePresentation.Docked
    )
