package com.serenity.state.manager

import cats.effect.{IO, Ref}
import cats.syntax.all.*
import com.serenity.config.AppConfig
import com.serenity.document.CommentRendering
import com.serenity.keystroke.events.*
import com.serenity.state.components.*
import com.serenity.state.models.*
import com.serenity.state.reducers.{Focused, ReducerResult, Transition}
import com.serenity.ui.layout.*

/** State the event pipeline exposes for selecting, activating, navigating, and resizing pinned/expanded panels, as a
  * capability record rather than a trait -- nothing here breaks a construction-order cycle (#1389), so mockability is
  * the only reason this needs an interface at all, and a record fakes trivially without one (#1017).
  */
final private[manager] case class PinnedPanelMouseHitTestingPort(
    stateRef: Ref[IO, AppState],
    applyComponentResult: (ComponentResult, AppState) => IO[AppState],
    validateAndUpdateState: (AppState, AppState) => IO[Unit],
    updateConfig: (AppConfig => AppConfig) => IO[AppConfig],
    resizePinnedPanel: (PanelTarget, Int) => IO[Unit]
)

/** Hit-tests mouse input against pinned/expanded panel rows (directory tree, outline, comments, diagnostics), navigates
  * the active editor to a selected location, and resizes panels and the text-area insets from a drag, independent of
  * every other mouse target.
  *
  * Row selection and navigation are pure transitions in the companion. Three steps stay IO here because no `AppEffect`
  * expresses them: activating a double-clicked directory row (a `ComponentResult` applied through the pipeline),
  * resizing a panel, and persisting a text-area inset change to the config.
  */
final private[manager] class PinnedPanelMouseHitTesting(port: PinnedPanelMouseHitTestingPort):
  import port.*

  private def commit(transition: Transition[Boolean]): IO[Boolean] =
    MouseTransition.commit(stateRef, applyReducerResult)(transition)

  private def applyReducerResult(result: ReducerResult, fallback: AppState): IO[Unit] =
    applyComponentResult(ComponentResult.reducerResult(result), fallback).void

  def handlePinnedPanelMouseClick(click: MouseClick, state: AppState): IO[Boolean] =
    if click.button != MouseButton.Primary then IO.pure(false)
    else
      commit(PinnedPanelMouseHitTesting.select(click, state, focusPanel = true)).flatTap {
        case true if click.clickCount >= 2 => activateSelectedDirectoryRow(click)
        case _                             => IO.unit
      }

  /** Activation resolves against the committed selection, the same row a keyboard Enter would now activate. */
  private def activateSelectedDirectoryRow(click: MouseClick): IO[Unit] =
    stateRef.get.flatMap { selectedState =>
      PinnedPanelMouseHitTesting.activation(click, selectedState).traverse_ { result =>
        applyComponentResult(result, selectedState).flatMap(validateAndUpdateState(_, selectedState))
      }
    }

  def handlePinnedPanelMouseSelect(
    event: MouseInputEvent,
    state: AppState,
    focusPanel: Boolean
  ): IO[Boolean] =
    commit(PinnedPanelMouseHitTesting.select(event, state, focusPanel))

  def handlePinnedPanelMouseHover(event: MouseInputEvent, state: AppState): IO[Boolean] =
    commit(PinnedPanelMouseHitTesting.hover(event, state))

  def handlePinnedPanelLocationClick(click: MouseClick, state: AppState): IO[Boolean] =
    commit(PinnedPanelMouseHitTesting.locationClick(click, state))

  def handlePinnedPanelResizeDrag(drag: MouseDrag, state: AppState): IO[Boolean] =
    PinnedPanelMouseHitTesting.panelResizeFromDrag(drag, state) match
      case Some(LayoutEngine.PinnedPanelDragResize(position, size)) =>
        resizePinnedPanel(PanelTarget.ByPosition(position), size).as(true)
      case None =>
        IO.pure(false)

  def handleTextAreaResizeDrag(drag: MouseDrag, state: AppState): IO[Boolean] =
    PinnedPanelMouseHitTesting.textAreaInsetFromDrag(drag, state) match
      case Some(inset) => updateConfig(inset.applyTo).as(true)
      case None        => IO.pure(false)

private[manager] object PinnedPanelMouseHitTesting:

  final private case class PinnedDirectoryMouseHit(
      surface: UiSurface,
      position: PanelPosition,
      tree: DirectoryTreeData,
      row: DirectoryTreeRow
  )

  enum TextAreaInsetDrag:
    case Left(value: Double)
    case Right(value: Double)
    case Top(value: Double)
    case Bottom(value: Double)

    def applyTo(config: AppConfig): AppConfig =
      this match
        case Left(value)   => config.withTextAreaLeftInset(value)
        case Right(value)  => config.withTextAreaRightInset(value)
        case Top(value)    => config.withTextAreaTopInset(value)
        case Bottom(value) => config.withTextAreaBottomInset(value)

  def select(event: MouseInputEvent, state: AppState, focusPanel: Boolean): Transition[Boolean] =
    pinnedDirectoryMouseHitAt(event, state) match
      case Some(hit) => Transition.modify(selectPinnedDirectoryRow(_, hit, focusPanel)).as(true)
      case None      => Transition.pure(false)

  /** Hovering highlights the row under the pointer without taking focus from the editor. */
  def hover(event: MouseInputEvent, state: AppState): Transition[Boolean] =
    pinnedDirectoryMouseHitAt(event, state)
      .map(hit => selectPinnedDirectoryRow(_, hit, focusPanel = false))
      .orElse(pinnedOutlineMouseHitAt(event, state).map { (surface, symbols, location) =>
        replaceContent(_, surface.id, SurfaceContent.Outline(symbols, Some(location)))
      })
      .orElse(pinnedCommentsMouseHitAt(event, state).map { (surface, symbols, location) =>
        replaceContent(_, surface.id, SurfaceContent.Comments(symbols, Some(location)))
      })
      .orElse(pinnedDiagnosticsMouseHitAt(event, state).map { (surface, issues, location) =>
        replaceContent(_, surface.id, SurfaceContent.Diagnostics(issues, Some(location)))
      })
      .fold(Transition.pure(false))(highlight => Transition.modify(highlight).as(true))

  /** What a double-click on a directory row does once the click itself has selected that row, resolved against the
    * state carrying that selection. `None` for a single click or a click that no longer lands on a row.
    */
  def activation(click: MouseClick, selectedState: AppState): Option[ComponentResult] =
    Option
      .when(click.clickCount >= 2)(pinnedDirectoryMouseHitAt(click, selectedState))
      .flatten
      .map(hit => PinnedPanelComponent(hit.position).processEvent(PanelInputEvent.Activate, selectedState))

  def locationClick(click: MouseClick, state: AppState): Transition[Boolean] =
    if click.button != MouseButton.Primary then Transition.pure(false)
    else
      pinnedCommentsMouseHitAt(click, state) match
        case Some((_, _, location)) =>
          Transition
            .modify(current => CommentRendering.openLensAtCursor(navigateActiveEditorToLocation(current, location)))
            .as(true)
        case None =>
          pinnedLocationMouseHitAt(click, state) match
            case Some(location) => Transition.modify(navigateActiveEditorToLocation(_, location)).as(true)
            case None           => Transition.pure(false)

  def panelResizeFromDrag(drag: MouseDrag, state: AppState): Option[LayoutEngine.PinnedPanelDragResize] =
    state.runtime.viewportSize.flatMap(viewportSize =>
      LayoutEngine.pinnedPanelResizeFromDrag(state, viewportSize, drag.col, drag.row)
    )

  private def selectPinnedDirectoryRow(
    state: AppState,
    hit: PinnedDirectoryMouseHit,
    focusPanel: Boolean
  ): AppState =
    val withRow   = replaceContent(state, hit.surface.id, SurfaceContent.DirectoryTree(hit.tree, Some(hit.row.path)))
    val nextFocus = if focusPanel then Focus.Surface(hit.surface.id) else withRow.persisted.focus
    if nextFocus == withRow.persisted.focus then withRow
    else withRow.copy(persisted = withRow.persisted.copy(focus = nextFocus))

  /** Leaves `state` untouched (by reference) when the surface already shows `content`, so hovering along a row that is
    * already highlighted does not commit anything.
    */
  private def replaceContent(state: AppState, surfaceId: SurfaceId, content: SurfaceContent): AppState =
    if state.surfaceById(surfaceId).forall(_.content == content) then state
    else
      state.copy(runtime =
        state.runtime.copy(uiSurfaces =
          state.runtime.uiSurfaces.replacedWhere(_.id == surfaceId)(_.copy(content = content))
        )
      )

  /** Resolves hover/click against the directory tree's own `ResolvedSurfaceComposition` (issue #819, slice 4) -- the
    * same composition `PinnedPanelViewModel` paints from, via `SurfaceHitRegion.hitAt`, rather than a generic row-index
    * walk. A hit region is addressed by the filesystem path it represents (mirroring
    * `EditorContextMenuHitTesting.contextMenuHitAt`'s use of `ResolvedSurfaceComposition.hitAt`), so the matching
    * visible row is looked up by path rather than by row index.
    */
  private def pinnedDirectoryMouseHitAt(
    event: MouseInputEvent,
    state: AppState
  ): Option[PinnedDirectoryMouseHit] =
    state.runtime.viewportSize.flatMap { viewportSize =>
      val scene = AuthoritativeUiScene.forState(state, viewportSize)
      scene.workspace.reverseIterator
        .flatMap {
          case SceneNode(SceneNodeId.Surface(surfaceId), _, frameRect, _, _, _) =>
            for
              surface  <- state.surfaceById(surfaceId)
              position <- panelPosition(surface, state)
              tree <- surface.content match
                case SurfaceContent.DirectoryTree(tree, _) => Some(tree)
                case _                                     => None
              selectedPath = surface.content match
                case SurfaceContent.DirectoryTree(_, selectedPath) => selectedPath
                case _                                             => None
              hitRegion <- DirectoryTreeSurfaceComposition
                .forTree(tree, selectedPath, frameRect)
                .hitAt(event.col.toDouble, event.row.toDouble)
              actionId <- hitRegion.actionId
              row      <- DirectoryTreeData.visibleRows(tree).find(_.path.toString == actionId.value)
            yield PinnedDirectoryMouseHit(surface, position, tree, row)
          case _ => None
        }
        .collectFirst { case hit => hit }
    }

  /** Resolves hover/click against the outline's own `ResolvedSurfaceComposition` (issue #819, slice 4) -- the same
    * composition `PinnedPanelViewModel` paints from, via `SurfaceHitRegion.hitAt`, rather than a generic row-index
    * walk. Mirrors `EditorContextMenuHitTesting.contextMenuSelectionAt`'s `focusId` parsing for addressing which symbol
    * was hit; `OutlineSurfaceComposition` only emits a hit region for a row `PanelContentResolver.outlineRowViews`
    * marked addressable, so `Horizontal`/`Compact` summary rows are unreachable here exactly as they were
    * pre-migration.
    */
  private def pinnedOutlineMouseHitAt(
    event: MouseInputEvent,
    state: AppState
  ): Option[(UiSurface, List[Symbol], Location)] =
    state.runtime.viewportSize.flatMap { viewportSize =>
      val scene = AuthoritativeUiScene.forState(state, viewportSize)
      scene.workspace.reverseIterator
        .flatMap {
          case SceneNode(SceneNodeId.Surface(surfaceId), _, frameRect, _, _, _) =>
            for
              surface <- state.surfaceById(surfaceId)
              symbols <- surface.content match
                case SurfaceContent.Outline(symbols, _) => Some(symbols)
                case _                                  => None
              activeLocation = surface.content match
                case SurfaceContent.Outline(_, activeLocation) => activeLocation
                case _                                         => None
              hitRegion <- OutlineSurfaceComposition
                .forOutline(symbols, activeLocation, frameRect)
                .hitAt(event.col.toDouble, event.row.toDouble)
              index  <- hitRegion.focusId.value.stripPrefix("outline-symbol-").toIntOption
              symbol <- symbols.lift(index)
            yield (surface, symbols, symbol.location)
          case _ => None
        }
        .collectFirst { case hit => hit }
    }

  /** Resolves hover/click against the comments panel's own `ResolvedSurfaceComposition` (issue #819, slice 5) -- the
    * same composition `PinnedPanelViewModel` paints from, via `SurfaceHitRegion.hitAt`, rather than a generic row-index
    * walk. Mirrors `pinnedOutlineMouseHitAt`'s `focusId` parsing for addressing which symbol was hit;
    * `CommentsSurfaceComposition` only emits a hit region for a row `PanelContentResolver.commentsRowViews` marked
    * addressable, so `Horizontal`/`Compact` summary rows are unreachable here exactly as they were pre-migration.
    */
  private def pinnedCommentsMouseHitAt(
    event: MouseInputEvent,
    state: AppState
  ): Option[(UiSurface, List[Symbol], Location)] =
    state.runtime.viewportSize.flatMap { viewportSize =>
      val scene = AuthoritativeUiScene.forState(state, viewportSize)
      scene.workspace.reverseIterator
        .flatMap {
          case SceneNode(SceneNodeId.Surface(surfaceId), _, frameRect, _, _, _) =>
            for
              surface <- state.surfaceById(surfaceId)
              symbols <- surface.content match
                case SurfaceContent.Comments(symbols, _) => Some(symbols)
                case _                                   => None
              activeLocation = surface.content match
                case SurfaceContent.Comments(_, activeLocation) => activeLocation
                case _                                          => None
              hitRegion <- CommentsSurfaceComposition
                .forComments(symbols, activeLocation, frameRect)
                .hitAt(event.col.toDouble, event.row.toDouble)
              index  <- hitRegion.focusId.value.stripPrefix("comments-symbol-").toIntOption
              symbol <- symbols.lift(index)
            yield (surface, symbols, symbol.location)
          case _ => None
        }
        .collectFirst { case hit => hit }
    }

  /** Resolves hover/click against the diagnostics panel's own `ResolvedSurfaceComposition` (issue #819, slice 4) -- the
    * same composition `PinnedPanelViewModel` paints from, via `SurfaceHitRegion.hitAt`, rather than a generic row-index
    * walk. `DiagnosticsSurfaceComposition` only emits a hit region for a row `PanelContentResolver.diagnosticsRowViews`
    * marked addressable, so `Horizontal`/`Compact`'s summary rows and `Square`'s leading summary row are unreachable
    * here exactly as they were pre-migration.
    */
  private def pinnedDiagnosticsMouseHitAt(
    event: MouseInputEvent,
    state: AppState
  ): Option[(UiSurface, List[Diagnostic], Location)] =
    state.runtime.viewportSize.flatMap { viewportSize =>
      val scene = AuthoritativeUiScene.forState(state, viewportSize)
      scene.workspace.reverseIterator
        .flatMap {
          case SceneNode(SceneNodeId.Surface(surfaceId), _, frameRect, _, _, _) =>
            for
              surface <- state.surfaceById(surfaceId)
              issues <- surface.content match
                case SurfaceContent.Diagnostics(issues, _) => Some(issues)
                case _                                     => None
              activeLocation = surface.content match
                case SurfaceContent.Diagnostics(_, activeLocation) => activeLocation
                case _                                             => None
              hitRegion <- DiagnosticsSurfaceComposition
                .forDiagnostics(issues, activeLocation, frameRect)
                .hitAt(event.col.toDouble, event.row.toDouble)
              index <- hitRegion.focusId.value.stripPrefix("diagnostics-issue-").toIntOption
              issue <- issues.lift(index)
            yield (surface, issues, issue.location)
          case _ => None
        }
        .collectFirst { case hit => hit }
    }

  /** Both outline's and diagnostics' location-click reuse their own composition-based hit test directly (issue #819,
    * slice 4) rather than a second, parallel row lookup -- they need the exact same location their hover/click handlers
    * resolve.
    */
  private def pinnedLocationMouseHitAt(
    event: MouseInputEvent,
    state: AppState
  ): Option[Location] =
    pinnedOutlineMouseHitAt(event, state)
      .map(_._3)
      .orElse(pinnedDiagnosticsMouseHitAt(event, state).map(_._3))

  private def navigateActiveEditorToLocation(state: AppState, location: Location): AppState =
    state.persisted.layout.activeEditorPaneId match
      case Some(paneId) =>
        Focused.bufferOf(state, paneId) match
          case Some(buffer) =>
            val line =
              math.max(0, math.min(location.line, math.max(0, buffer.document.content.lineCount - 1)))
            val column =
              math.max(0, math.min(location.column, buffer.document.content.getLine(line).getOrElse("").length))
            val cursor   = CursorPosition(line, column)
            val viewport = CursorViewport.adjustForCursor(buffer, state, cursor)
            val updatedBuffer = buffer.copy(
              editing = EditingState(List(cursor)),
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

  private def panelPosition(surface: UiSurface, state: AppState): Option[PanelPosition] =
    surface.presentation match
      case SurfacePresentation.Docked => state.persisted.layout.workspaceTree.flatMap(_.positionForSurface(surface.id))
      case _                          => None

  def textAreaInsetFromDrag(drag: MouseDrag, state: AppState): Option[TextAreaInsetDrag] =
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
