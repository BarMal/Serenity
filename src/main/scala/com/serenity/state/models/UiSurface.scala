package com.serenity.state.models

import java.nio.file.Path

import com.serenity.command.{Command, CommandRunner}
import com.serenity.document.RenderedComment
import com.serenity.ui.layout.*
import com.serenity.ui.theme.config.ThemeCreatorState

final case class SurfaceId(value: String)

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

/** Pixel-space click target for an action rendered on the startup page. */
final case class StartupActionBounds(index: Int, xPx: Int, yPx: Int, widthPx: Int, heightPx: Int):
  def contains(pixelX: Int, pixelY: Int): Boolean =
    pixelX >= xPx && pixelX < xPx + widthPx && pixelY >= yPx && pixelY < yPx + heightPx

final case class StartupPage(
    title: String,
    options: List[String] = Nil,
    statusMessage: Option[String] = None,
    selectedIndex: Int = 0,
    actions: List[StartupAction] = Nil
):

  private def legacyActions: List[StartupAction] =
    options.zipWithIndex.map {
      case (label, index) =>
        val (id, command) = index match
          case 0 =>
            "new-session" -> Command.typed(
              "startup.new-session",
              "Start a new session",
              com.serenity.command.CommandIntent.StartupNewSession
            )
          case 1 =>
            "restore-session" -> Command.typed(
              "startup.restore-session",
              "Restore an existing session",
              com.serenity.command.CommandIntent.StartupRestoreSession
            )
          case 2 =>
            "open-file" -> Command.typed(
              "startup.open-file",
              "Open an existing file or directory",
              com.serenity.command.CommandIntent.StartupOpenFile
            )
          case _ =>
            s"option-$index" -> Command.typed(
              "startup.new-session",
              "Start a new session",
              com.serenity.command.CommandIntent.StartupNewSession
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
      .foldLeft((List.empty[Int], 3, Option.empty[StartupActionSection])) {
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
    val actionLines = launchActions.zipWithIndex.foldLeft(List.empty[String]) {
      case (lines, (action, index)) =>
        val sectionHeader =
          if index > 0 && launchActions(index - 1).section != action.section then List("", "Workflows")
          else Nil
        lines ++ sectionHeader :+ action.renderedLabel
    }
    val baseLines = List(title, "Choose a starting point", "") ++ actionLines
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

enum SurfacePlacement:
  case AboveCursor
  case BelowCursor

enum SurfacePresentation:
  case Floating(anchor: Option[CursorPosition], placement: SurfacePlacement)
  case Modal
  case Pinned(position: PanelPosition, size: Int)
  case Expanded(originalPosition: PanelPosition, originalSize: Int)

/** Focused draft state for editing an authored document comment from the above-cursor lens. */
final case class CommentLensState(
    comment: RenderedComment,
    draft: String,
    cursor: Int,
    target: Option[DocumentComment]
):
  def clampedCursor: Int =
    math.max(0, math.min(cursor, draft.length))

enum SurfaceContent:
  case StartPage(page: StartupPage)
  case QuickInfo(text: String)
  case FilePreview(path: Path, content: String)
  case SymbolDefinition(symbol: String, location: Location)
  case CursorInfoBar(text: String)
  case DirectoryListing(path: Path, entries: List[DirEntry], selectedPath: Option[Path] = None)
  case DirectoryTree(tree: DirectoryTreeData, selectedPath: Option[Path] = None)
  case CommandPalette(runner: CommandRunner)
  case CommandPaletteSubmenu(runner: CommandRunner, groupId: String, previewOnly: Boolean)
  case ThemePicker(state: ThemePickerState)
  case ThemeCreator(state: ThemeCreatorState)
  case FileSearch(state: FileSearchState)
  case ContextualToolbar(state: ContextualToolbarState)
  case ContextMenu(menu: com.serenity.state.models.ContextMenu)
  case CommentLens(state: CommentLensState)
  case MarkdownPreview(bufferId: BufferId, title: String)
  case ModalWorkflow(modal: Modal)
  case Terminal(buffer: String, cursor: Int)
  case Outline(symbols: List[Symbol], activeLocation: Option[Location] = None)
  case Comments(symbols: List[Symbol], activeLocation: Option[Location] = None)
  case Diagnostics(issues: List[Diagnostic], activeLocation: Option[Location] = None)

  /** Transient ghost surface used during close-fade-out animation; never persisted in sessions. */
  case GhostOverlay(originalContent: SurfaceContent, cachedRect: LayoutRect)

final case class UiSurface(
    id: SurfaceId,
    content: SurfaceContent,
    presentation: SurfacePresentation,
    dismissOnMove: Boolean = false
)

object UiSurface:

  def fromPanelContent(
    id: SurfaceId,
    content: PanelContent,
    position: PanelPosition,
    size: Int
  ): UiSurface =
    val surfaceContent = content match
      case PanelContent.DirectoryTree(tree, selectedPath) =>
        SurfaceContent.DirectoryTree(tree, selectedPath)
      case PanelContent.Terminal(buffer, cursor) =>
        SurfaceContent.Terminal(buffer, cursor)
      case PanelContent.Outline(symbols, activeLocation) =>
        SurfaceContent.Outline(symbols, activeLocation)
      case PanelContent.Comments(symbols, activeLocation) =>
        SurfaceContent.Comments(symbols, activeLocation)
      case PanelContent.Diagnostics(issues) =>
        SurfaceContent.Diagnostics(issues)
      case PanelContent.MarkdownPreview(bufferId, title) =>
        SurfaceContent.MarkdownPreview(bufferId, title)

    UiSurface(
      id = id,
      content = surfaceContent,
      presentation = SurfacePresentation.Pinned(position, size)
    )
