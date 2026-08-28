package com.serenity.state.manager

import cats.effect.{IO, Ref}
import com.serenity.document.CommentRendering
import com.serenity.keystroke.events.*
import com.serenity.state.components.*
import com.serenity.state.models.*
import com.serenity.ui.layout.*

/** State the event pipeline exposes for selecting, activating, navigating, and resizing pinned/expanded panels. */
private[manager] trait PinnedPanelMouseHitTestingPort:
  def stateRef: Ref[IO, AppState]
  def applyComponentResult(result: ComponentResult, state: AppState): IO[AppState]
  def validateAndUpdateState(newState: AppState, fallbackState: AppState): IO[Unit]

  def updateConfig(
    update: com.serenity.config.AppConfig => com.serenity.config.AppConfig
  ): IO[com.serenity.config.AppConfig]

  def resizePinnedPanel(target: PanelTarget, newSize: Int): IO[Unit]

/** Hit-tests mouse input against pinned/expanded panel rows (directory tree, outline, comments, diagnostics), navigates
  * the active editor to a selected location, and resizes panels and the text-area insets from a drag, independent of
  * every other mouse target.
  */
final private[manager] class PinnedPanelMouseHitTesting(port: PinnedPanelMouseHitTestingPort):
  import port.*

  final private case class PinnedDirectoryMouseHit(
      surface: UiSurface,
      position: PanelPosition,
      tree: DirectoryTreeData,
      row: DirectoryTreeRow
  )

  final private case class PinnedPanelRowHit(
      surface: UiSurface,
      position: PanelPosition,
      rowIndex: Int,
      layoutKind: SurfaceLayoutKind
  )

  private enum TextAreaInsetDrag:
    case Left(value: Double)
    case Right(value: Double)
    case Top(value: Double)
    case Bottom(value: Double)

  def handlePinnedPanelMouseClick(click: MouseClick, state: AppState): IO[Boolean] =
    if click.button != MouseButton.Primary then IO.pure(false)
    else
      handlePinnedPanelMouseSelect(click, state, focusPanel = true).flatMap {
        case false =>
          IO.pure(false)
        case true if click.clickCount < 2 =>
          IO.pure(true)
        case true =>
          stateRef.get.flatMap { selectedState =>
            pinnedDirectoryMouseHitAt(click, selectedState) match
              case Some(hit) =>
                val result = PinnedPanelComponent(hit.position).processEvent(PanelInputEvent.Activate, selectedState)
                applyComponentResult(result, selectedState)
                  .flatMap(validateAndUpdateState(_, selectedState))
                  .as(true)
              case None =>
                IO.pure(true)
          }
      }

  def handlePinnedPanelMouseSelect(
    event: MouseInputEvent,
    state: AppState,
    focusPanel: Boolean
  ): IO[Boolean] =
    pinnedDirectoryMouseHitAt(event, state) match
      case Some(hit) =>
        stateRef.update(selectPinnedDirectoryRow(_, hit, focusPanel)).as(true)
      case None =>
        IO.pure(false)

  def handlePinnedPanelMouseHover(
    event: MouseInputEvent,
    state: AppState
  ): IO[Boolean] =
    handlePinnedPanelMouseSelect(event, state, focusPanel = false).flatMap {
      case true => IO.pure(true)
      case false =>
        pinnedOutlineMouseHitAt(event, state) match
          case Some((surface, symbols, location)) =>
            stateRef.update(selectPinnedOutlineLocation(_, surface, symbols, location)).as(true)
          case None =>
            pinnedCommentsMouseHitAt(event, state) match
              case Some((surface, symbols, location)) =>
                stateRef.update(selectPinnedCommentsLocation(_, surface, symbols, location)).as(true)
              case None =>
                pinnedDiagnosticsMouseHitAt(event, state) match
                  case Some((surface, issues, location)) =>
                    stateRef.update(selectPinnedDiagnosticsLocation(_, surface, issues, location)).as(true)
                  case None =>
                    IO.pure(false)
    }

  def handlePinnedPanelLocationClick(click: MouseClick, state: AppState): IO[Boolean] =
    if click.button != MouseButton.Primary then IO.pure(false)
    else
      pinnedCommentsMouseHitAt(click, state) match
        case Some((_, _, location)) =>
          stateRef
            .update(current => CommentRendering.openLensAtCursor(navigateActiveEditorToLocation(current, location)))
            .as(true)
        case None =>
          pinnedLocationMouseHitAt(click, state) match
            case Some(location) =>
              stateRef.update(current => navigateActiveEditorToLocation(current, location)).as(true)
            case None =>
              IO.pure(false)

  def handlePinnedPanelResizeDrag(drag: MouseDrag, state: AppState): IO[Boolean] =
    state.runtime.viewportSize.flatMap(viewportSize =>
      LayoutEngine.pinnedPanelResizeFromDrag(state, viewportSize, drag.col, drag.row)
    ) match
      case Some(LayoutEngine.PinnedPanelDragResize(position, size)) =>
        resizePinnedPanel(PanelTarget.ByPosition(position), size).as(true)
      case None =>
        IO.pure(false)

  def handleTextAreaResizeDrag(drag: MouseDrag, state: AppState): IO[Boolean] =
    textAreaInsetFromDrag(drag, state) match
      case Some(TextAreaInsetDrag.Left(value)) =>
        updateConfig(_.withTextAreaLeftInset(value)).map(_ => true)
      case Some(TextAreaInsetDrag.Right(value)) =>
        updateConfig(_.withTextAreaRightInset(value)).map(_ => true)
      case Some(TextAreaInsetDrag.Top(value)) =>
        updateConfig(_.withTextAreaTopInset(value)).map(_ => true)
      case Some(TextAreaInsetDrag.Bottom(value)) =>
        updateConfig(_.withTextAreaBottomInset(value)).map(_ => true)
      case None =>
        IO.pure(false)

  private def selectPinnedDirectoryRow(
    state: AppState,
    hit: PinnedDirectoryMouseHit,
    focusPanel: Boolean
  ): AppState =
    val updatedContent = SurfaceContent.DirectoryTree(hit.tree, Some(hit.row.path))
    val updatedSurfaces = state.runtime.uiSurfaces.map {
      case surface if surface.id == hit.surface.id => surface.copy(content = updatedContent)
      case surface                                 => surface
    }
    val nextFocus = if focusPanel then Focus.Surface(hit.surface.id) else state.persisted.focus
    state.copy(
      persisted = state.persisted.copy(focus = nextFocus),
      runtime = state.runtime.copy(uiSurfaces = updatedSurfaces)
    )

  private def selectPinnedOutlineLocation(
    state: AppState,
    surface: UiSurface,
    symbols: List[Symbol],
    location: Location
  ): AppState =
    state.copy(runtime = state.runtime.copy(uiSurfaces = state.runtime.uiSurfaces.map {
      case existing if existing.id == surface.id =>
        existing.copy(content = SurfaceContent.Outline(symbols, Some(location)))
      case existing =>
        existing
    }))

  private def selectPinnedCommentsLocation(
    state: AppState,
    surface: UiSurface,
    symbols: List[Symbol],
    location: Location
  ): AppState =
    state.copy(runtime = state.runtime.copy(uiSurfaces = state.runtime.uiSurfaces.map {
      case existing if existing.id == surface.id =>
        existing.copy(content = SurfaceContent.Comments(symbols, Some(location)))
      case existing =>
        existing
    }))

  private def selectPinnedDiagnosticsLocation(
    state: AppState,
    surface: UiSurface,
    issues: List[Diagnostic],
    location: Location
  ): AppState =
    state.copy(runtime = state.runtime.copy(uiSurfaces = state.runtime.uiSurfaces.map {
      case existing if existing.id == surface.id =>
        existing.copy(content = SurfaceContent.Diagnostics(issues, Some(location)))
      case existing =>
        existing
    }))

  private def pinnedDirectoryMouseHitAt(
    event: MouseInputEvent,
    state: AppState
  ): Option[PinnedDirectoryMouseHit] =
    for
      hit <- pinnedPanelRowHitAt(event, state)
      directoryHit <- hit.surface.content match
        case SurfaceContent.DirectoryTree(tree, _) =>
          DirectoryTreeData.visibleRows(tree).lift(hit.rowIndex).map { row =>
            PinnedDirectoryMouseHit(hit.surface, hit.position, tree, row)
          }
        case _ =>
          None
    yield directoryHit

  private def pinnedOutlineMouseHitAt(
    event: MouseInputEvent,
    state: AppState
  ): Option[(UiSurface, List[Symbol], Location)] =
    for
      hit <- pinnedPanelRowHitAt(event, state)
      locationHit <- hit.surface.content match
        case SurfaceContent.Outline(symbols, _) =>
          hit.layoutKind match
            case SurfaceLayoutKind.Vertical | SurfaceLayoutKind.Square =>
              symbols.lift(hit.rowIndex).map(symbol => (hit.surface, symbols, symbol.location))
            case SurfaceLayoutKind.Horizontal | SurfaceLayoutKind.Compact =>
              None
        case _ =>
          None
    yield locationHit

  private def pinnedCommentsMouseHitAt(
    event: MouseInputEvent,
    state: AppState
  ): Option[(UiSurface, List[Symbol], Location)] =
    for
      hit <- pinnedPanelRowHitAt(event, state)
      locationHit <- hit.surface.content match
        case SurfaceContent.Comments(symbols, _) =>
          hit.layoutKind match
            case SurfaceLayoutKind.Vertical | SurfaceLayoutKind.Square =>
              symbols.lift(hit.rowIndex).map(symbol => (hit.surface, symbols, symbol.location))
            case SurfaceLayoutKind.Horizontal | SurfaceLayoutKind.Compact =>
              None
        case _ =>
          None
    yield locationHit

  private def pinnedDiagnosticsMouseHitAt(
    event: MouseInputEvent,
    state: AppState
  ): Option[(UiSurface, List[Diagnostic], Location)] =
    for
      hit <- pinnedPanelRowHitAt(event, state)
      locationHit <- hit.surface.content match
        case SurfaceContent.Diagnostics(issues, _) =>
          hit.layoutKind match
            case SurfaceLayoutKind.Vertical =>
              issues.lift(hit.rowIndex).map(issue => (hit.surface, issues, issue.location))
            case SurfaceLayoutKind.Square =>
              Option
                .when(hit.rowIndex > 0)(hit.rowIndex - 1)
                .flatMap(issues.lift)
                .map(issue => (hit.surface, issues, issue.location))
            case SurfaceLayoutKind.Horizontal | SurfaceLayoutKind.Compact =>
              None
        case _ =>
          None
    yield locationHit

  private def pinnedLocationMouseHitAt(
    event: MouseInputEvent,
    state: AppState
  ): Option[Location] =
    for
      hit <- pinnedPanelRowHitAt(event, state)
      location <- hit.surface.content match
        case SurfaceContent.Outline(symbols, _) =>
          hit.layoutKind match
            case SurfaceLayoutKind.Vertical | SurfaceLayoutKind.Square =>
              symbols.lift(hit.rowIndex).map(_.location)
            case SurfaceLayoutKind.Horizontal | SurfaceLayoutKind.Compact =>
              None
        case SurfaceContent.Diagnostics(issues, _) =>
          hit.layoutKind match
            case SurfaceLayoutKind.Vertical =>
              issues.lift(hit.rowIndex).map(_.location)
            case SurfaceLayoutKind.Square =>
              Option.when(hit.rowIndex > 0)(hit.rowIndex - 1).flatMap(issues.lift).map(_.location)
            case SurfaceLayoutKind.Horizontal | SurfaceLayoutKind.Compact =>
              None
        case _ =>
          None
    yield location

  private def navigateActiveEditorToLocation(state: AppState, location: Location): AppState =
    state.persisted.layout.activeEditorPaneId match
      case Some(paneId) =>
        state.persisted.layout.editorPanes.get(paneId).flatMap(_.bufferId).flatMap(state.persisted.buffers.get) match
          case Some(buffer) =>
            val line =
              math.max(0, math.min(location.line, math.max(0, buffer.document.content.lineCount - 1)))
            val column =
              math.max(0, math.min(location.column, buffer.document.content.getLine(line).getOrElse("").length))
            val cursor   = CursorPosition(line, column)
            val viewport = CursorViewport.adjustForCursor(buffer, state, cursor)
            val updatedBuffer = buffer.copy(
              editing = buffer.editing.copy(
                cursors = List(cursor),
                selection = None,
                selections = Nil,
                preferredColumn = Some(cursor.column),
                preferredXPx = None,
                multiCursorVerticalStates = Nil
              ),
              viewport = viewport
            )
            state.copy(persisted =
              state.persisted.copy(
                buffers = state.persisted.buffers.updated(buffer.id, updatedBuffer),
                focus = Focus.EditorPane(paneId),
                layout = state.persisted.layout.copy(activeEditorPaneId = Some(paneId))
              )
            )
          case None =>
            state
      case None =>
        state

  private def panelPosition(surface: UiSurface): Option[PanelPosition] =
    surface.presentation match
      case SurfacePresentation.Pinned(position, _)   => Some(position)
      case SurfacePresentation.Expanded(position, _) => Some(position)
      case _                                         => None

  private def pinnedPanelRowHitAt(event: MouseInputEvent, state: AppState): Option[PinnedPanelRowHit] =
    state.runtime.viewportSize.flatMap { viewportSize =>
      val scene = AuthoritativeUiScene.forState(state, viewportSize)
      scene.workspace.reverseIterator
        .flatMap {
          case SceneNode(SceneNodeId.Surface(surfaceId), _, frameRect, _, hitRegions, _) =>
            for
              surface  <- state.surfaceById(surfaceId)
              position <- panelPosition(surface)
              contentRect <- hitRegions.collectFirst {
                case SceneHitRegion(SceneHitKind.Content, rect) if rect.contains(event.col, event.row) => rect
              }
              rowIndex <- pinnedPanelItemRowIndexAt(
                event,
                contentRect,
                scene.editorContract.panelRowSlots(surface.id)
              )
            yield PinnedPanelRowHit(surface, position, rowIndex, SurfaceLayoutKind.classify(frameRect))
          case _ => None
        }
        .collectFirst { case hit => hit }
    }

  private def pinnedPanelItemRowIndexAt(
    event: MouseInputEvent,
    contentRect: LayoutRect,
    rowSlots: List[SurfaceContentRowSlot]
  ): Option[Int] =
    val insideColumns = event.col >= contentRect.x && event.col < contentRect.right
    Option
      .when(insideColumns)(())
      .flatMap(_ =>
        rowSlots.collectFirst {
          case SurfaceContentRowSlot(SurfaceContentRowKind.Item(index), y) if y == event.row =>
            index
        }
      )

  private def textAreaInsetFromDrag(drag: MouseDrag, state: AppState): Option[TextAreaInsetDrag] =
    state.runtime.viewportSize.flatMap { viewportSize =>
      val layout   = LayoutEngine.calculateLayoutWithUI(state, viewportSize)
      val contract = EditorLayoutContract.from(state, viewportSize, layout)
      contract.activePaneLayout.flatMap { _ =>
        val workspaceX     = contract.leftSpacerRect.x
        val workspaceRight = contract.rightSpacerRect.right
        val workspaceWidth = (workspaceRight - workspaceX).max(1)
        val contentTop     = contract.topSpacerRect.y
        val contentBottom  = contract.workspace.editorPanelRect.bottom
        val contentHeight  = (contentBottom - contentTop).max(1)
        val withinWorkspaceY =
          drag.row >= contract.leftSpacerRect.y && drag.row < contract.leftSpacerRect.bottom
        val withinWorkspaceX =
          drag.col >= contract.topSpacerRect.x && drag.col < contract.topSpacerRect.right

        if withinWorkspaceY && drag.col >= contract.leftSpacerRect.x && drag.col < contract.leftSpacerRect.right then
          Some(TextAreaInsetDrag.Left((drag.col - workspaceX).toDouble / workspaceWidth.toDouble))
        else if withinWorkspaceY && drag.col >= contract.rightSpacerRect.x && drag.col < contract.rightSpacerRect.right
        then Some(TextAreaInsetDrag.Right((workspaceRight - drag.col).toDouble / workspaceWidth.toDouble))
        else if withinWorkspaceX &&
            drag.row >= contract.topSpacerRect.y &&
            drag.row < contract.topSpacerRect.bottom
        then Some(TextAreaInsetDrag.Top((drag.row - contentTop).toDouble / contentHeight.toDouble))
        else if withinWorkspaceX &&
            drag.row >= contract.bottomSpacerRect.y &&
            drag.row < contract.bottomSpacerRect.bottom
        then Some(TextAreaInsetDrag.Bottom((contentBottom - drag.row).toDouble / contentHeight.toDouble))
        else None
      }
    }
