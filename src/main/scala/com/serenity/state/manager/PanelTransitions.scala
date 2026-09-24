package com.serenity.state.manager

import com.serenity.command.PanelKind
import com.serenity.config.CommentDisplayMode
import com.serenity.lsp.config.LanguageId
import com.serenity.state.models.*
import com.serenity.state.reducers.CommandRunnerPanelSelections
import com.serenity.state.undo.HistoryEntry
import com.serenity.ui.layout.{PanelPosition, WorkspaceTree}

/** What pinning a panel kind comes to, decided from the current state. */
private[manager] enum PanelPinPlan:
  case Commit(update: AppState => AppState)

  /** The explorer's first pin needs the working directory listed first, off the pure path. */
  case LoadExplorerRoot(size: Int)

  /** Nothing is pinned; `message` tells the user why. */
  case Report(message: String)

  /** Nothing is pinned; `debugLog` is what the shell logs about it. */
  case Ignore(debugLog: String)

/** Pinned-panel changes by [[PanelKind]] as pure functions of the state -- what `StateManagerPanelEffects` commits. */
private[manager] object PanelTransitions:

  def pinPlan(kind: PanelKind, position: PanelPosition, state: AppState): PanelPinPlan =
    val size = defaultPanelSize(kind, position)
    def upsert(content: SurfaceContent): PanelPinPlan =
      PanelPinPlan.Commit(upsertPanelKind(kind, content, position, size))
    kind match
      case PanelKind.Explorer =>
        newestPanelKindSurface(kind, state).fold(PanelPinPlan.LoadExplorerRoot(size))(surface =>
          upsert(surface.content)
        )
      case PanelKind.Outline =>
        val symbols = PanelSymbolLookup.outlineSymbols(state)
        upsert(SurfaceContent.Outline(symbols, PanelSymbolLookup.currentSymbolActiveLocation(symbols, state)))
      case PanelKind.Comments =>
        // #1551: the pin-to-side command and the `CommentDisplayMode` setting used to disagree about whether comments
        // are visible -- pinning always showed live comment content regardless of the setting.
        if state.persisted.config.surfaceConfig.commentDisplayMode == CommentDisplayMode.Off then
          PanelPinPlan.Report("Comments are hidden -- comment display is turned off in Settings.")
        else
          val symbols = PanelSymbolLookup.commentPanelSymbols(state)
          upsert(SurfaceContent.Comments(symbols, PanelSymbolLookup.currentSymbolActiveLocation(symbols, state)))
      case PanelKind.Diagnostics =>
        upsert(SurfaceContent.Diagnostics(Nil))
      case PanelKind.MarkdownPreview =>
        markdownPreviewContent(state).fold(
          PanelPinPlan.Ignore("[CMD] Markdown preview requested without an active Markdown buffer")
        )(upsert)

  /** `update` applied to the model's state, declared as an undo boundary (#1016 PR4) in the same model when it changed
    * anything, and followed by a refresh of an open command runner's panel selections when `refreshSelections`. A no-op
    * records nothing, mirroring `AppEventReducer.closePaneResult`'s guard for pane close.
    */
  def panelChange(model: Model, update: AppState => AppState, refreshSelections: Boolean): Model =
    val updated = update(model.app)
    val undo =
      if updated == model.app then model.undo
      else UndoRecording.recorded(model.undo, HistoryEntry.PanelChange.capture(model.app), groupable = false)
    model.copy(app = if refreshSelections then withCommandRunnerPanelSelections(updated) else updated, undo = undo)

  def removePanelKind(kind: PanelKind)(state: AppState): AppState =
    val removedIds = state.runtime.uiSurfaces.collect {
      case surface if panelKindOf(surface.content).contains(kind) => surface.id
    }.toSet
    val nextFocus = state.persisted.focus match
      case Focus.Surface(surfaceId) if removedIds.contains(surfaceId) =>
        state.persisted.layout.activeEditorPaneId.map(Focus.EditorPane.apply).getOrElse(state.persisted.focus)
      case _ =>
        state.persisted.focus
    val prunedTree = removedIds.foldLeft(state.persisted.layout.workspaceTree) { (tree, id) =>
      tree.flatMap(_.removeSurface(id)).orElse(tree)
    }
    val maximized = state.persisted.layout.maximizedWorkspaceNodeId.filterNot(nodeId =>
      state.persisted.layout.workspaceTree.flatMap(_.surfaceIdForNode(nodeId)).exists(removedIds.contains)
    )
    state.copy(
      persisted = state.persisted.copy(
        focus = nextFocus,
        layout = state.persisted.layout.copy(
          workspaceTree = prunedTree.orElse(state.persisted.layout.workspaceTree),
          maximizedWorkspaceNodeId = maximized
        )
      ),
      runtime =
        state.runtime.copy(uiSurfaces = state.runtime.uiSurfaces.filterNot(surface => removedIds.contains(surface.id)))
    )

  def upsertPanelKind(
    kind: PanelKind,
    content: SurfaceContent,
    position: PanelPosition,
    size: Int
  )(state: AppState): AppState =
    val matchingSurfaces = state.runtime.uiSurfaces.filter(surface => panelKindOf(surface.content).contains(kind))
    val retainedSurface  = matchingSurfaces.reverse.headOption
    val stateWithoutKind = state.copy(runtime =
      state.runtime.copy(uiSurfaces =
        state.runtime.uiSurfaces.filterNot(surface => panelKindOf(surface.content).contains(kind))
      )
    )
    val droppedIds = matchingSurfaces.filterNot(surface => retainedSurface.exists(_.id == surface.id)).map(_.id).toSet
    val treeWithoutDropped = droppedIds.foldLeft(state.persisted.layout.workspaceTree) { (tree, id) =>
      tree.flatMap(_.removeSurface(id)).orElse(tree)
    }
    val (stateWithId, surface, isNewlyDocked) = retainedSurface match
      case Some(existing) =>
        (
          stateWithoutKind,
          existing.copy(content = content, presentation = SurfacePresentation.Docked, dismissOnMove = false),
          false
        )
      case None =>
        val (allocatedState, surfaceId) = stateWithoutKind.allocateSurfaceId
        (allocatedState, UiSurface(surfaceId, content, SurfacePresentation.Docked, dismissOnMove = false), true)
    val placedTree =
      treeWithoutDropped
        .orElse(stateWithId.persisted.layout.workspaceTree)
        .fold(state.persisted.layout.workspaceTree)(tree =>
          placeInTree(Some(tree), surface.id, position, size, isNewlyDocked, stateWithId)
        )
    val nextFocus = state.persisted.focus match
      case Focus.Surface(surfaceId) if matchingSurfaces.exists(_.id == surfaceId) => Focus.Surface(surface.id)
      case _                                                                      => state.persisted.focus
    stateWithId.copy(
      persisted = stateWithId.persisted.copy(
        focus = nextFocus,
        layout =
          stateWithId.persisted.layout.copy(workspaceTree = placedTree.orElse(state.persisted.layout.workspaceTree))
      ),
      runtime = stateWithId.runtime.copy(uiSurfaces = stateWithId.runtime.uiSurfaces :+ surface)
    )

  /** The retained surface's own dock/move/resize -- collapsed into one tree update alongside the flat-list upsert
    * above, so this path (issue #817) keeps the workspace tree in step with `uiSurfaces` itself rather than relying on
    * a separate reconciliation pass to notice the drift.
    */
  private def placeInTree(
    tree: Option[WorkspaceTree],
    surfaceId: SurfaceId,
    position: PanelPosition,
    size: Int,
    isNewlyDocked: Boolean,
    state: AppState
  ): Option[WorkspaceTree] =
    tree.flatMap { workspaceTree =>
      if isNewlyDocked then
        val (splitId, leafId) = workspaceTree.nextDockIds(surfaceId)
        workspaceTree.dockSized(surfaceId, position, splitId, leafId, size, state.runtime.viewportSize)
      else if workspaceTree.positionForSurface(surfaceId).contains(position) then
        workspaceTree
          .allocationRatio(surfaceId, size, state.runtime.viewportSize)
          .flatMap(workspaceTree.resizeSurface(surfaceId, _))
      else
        val (splitId, _) = workspaceTree.nextDockIds(surfaceId)
        workspaceTree.moveSurface(surfaceId, position, splitId).flatMap { moved =>
          moved.allocationRatio(surfaceId, size, state.runtime.viewportSize).flatMap(moved.resizeSurface(surfaceId, _))
        }
    }

  /** Reorders a docked panel among the others sharing its edge (issue #1310) by rearranging the workspace tree's own
    * nesting for that edge (issue #817: the tree is the sole record of same-edge order, via `WorkspaceTree.dock`'s
    * insertion-order nesting), rather than splicing `uiSurfaces` and leaving a later reconciliation pass to notice.
    */
  def reorderPanelKind(kind: PanelKind, delta: Int)(state: AppState): AppState =
    if delta == 0 then state
    else
      state.persisted.layout.workspaceTree match
        case None => state
        case Some(tree) =>
          def kindOf(surfaceId: SurfaceId): Option[PanelKind] =
            state.runtime.uiSurfaces.find(_.id == surfaceId).flatMap(surface => panelKindOf(surface.content))
          tree.dockedSurfaceIds.find(id => kindOf(id).contains(kind)) match
            case None => state
            case Some(targetId) =>
              tree.positionForSurface(targetId) match
                case None => state
                case Some(targetPosition) =>
                  val sameEdge =
                    tree.dockedSurfaceIds.filter(id => tree.positionForSurface(id).contains(targetPosition))
                  val currentIndex = sameEdge.indexOf(targetId)
                  val targetIndex  = (currentIndex + delta).max(0).min(sameEdge.length - 1)
                  if currentIndex < 0 || currentIndex == targetIndex then state
                  else
                    val desiredOrder  = moveWithinList(sameEdge, currentIndex, targetIndex)
                    val reorderedTree = tree.reorderAt(targetPosition, desiredOrder)
                    state.copy(persisted =
                      state.persisted.copy(layout = state.persisted.layout.copy(workspaceTree = Some(reorderedTree)))
                    )

  private def moveWithinList[A](values: List[A], from: Int, to: Int): List[A] =
    if from == to then values
    else
      values.lift(from) match
        case None => values
        case Some(value) =>
          val withoutValue = values.patch(from, Nil, 1)
          withoutValue.patch(to, List(value), 0)

  def withCommandRunnerPanelSelections(state: AppState): AppState =
    val selections = CommandRunnerPanelSelections.fromState(state)
    val updatedSurfaces = state.runtime.uiSurfaces.map {
      case surface @ UiSurface(_, SurfaceContent.CommandPalette(runner), _, _) =>
        surface.copy(content =
          SurfaceContent.CommandPalette(runner.copy(optionSelections = runner.optionSelections ++ selections))
        )
      case other => other
    }
    state.copy(runtime = state.runtime.copy(uiSurfaces = updatedSurfaces))

  def withMarkdownPreviewWindowBuffer(state: AppState, bufferId: Option[BufferId]): AppState =
    state.copy(runtime = state.runtime.copy(markdownPreviewWindowBuffer = bufferId))

  def markdownPreviewBufferId(state: AppState): Option[BufferId] =
    focusedMarkdownBuffer(state).map(_.id)

  private def markdownPreviewContent(state: AppState): Option[SurfaceContent] =
    focusedMarkdownBuffer(state).map { buffer =>
      val title = buffer.document.filePath
        .flatMap(path => Option(path.getFileName).map(_.toString))
        .getOrElse("Untitled")
      SurfaceContent.MarkdownPreview(buffer.id, title)
    }

  private def focusedMarkdownBuffer(state: AppState): Option[Buffer] =
    state.focusedBufferId
      .flatMap(state.persisted.buffers.get)
      .filter(_.document.language.contains(LanguageId.Markdown))

  private def newestPanelKindSurface(kind: PanelKind, state: AppState): Option[UiSurface] =
    state.runtime.uiSurfaces.reverse.find(surface => panelKindOf(surface.content).contains(kind))

  def panelKindOf(content: SurfaceContent): Option[PanelKind] =
    content match
      case SurfaceContent.DirectoryTree(_, _)   => Some(PanelKind.Explorer)
      case SurfaceContent.Outline(_, _)         => Some(PanelKind.Outline)
      case SurfaceContent.Comments(_, _)        => Some(PanelKind.Comments)
      case SurfaceContent.Diagnostics(_, _)     => Some(PanelKind.Diagnostics)
      case SurfaceContent.MarkdownPreview(_, _) => Some(PanelKind.MarkdownPreview)
      case _                                    => None

  def defaultPanelSize(kind: PanelKind, position: PanelPosition): Int =
    kind match
      case PanelKind.MarkdownPreview => 40
      case PanelKind.Diagnostics =>
        position match
          case PanelPosition.Top | PanelPosition.Bottom => 10
          case PanelPosition.Left | PanelPosition.Right => 30
      case PanelKind.Explorer | PanelKind.Outline | PanelKind.Comments =>
        position match
          case PanelPosition.Top | PanelPosition.Bottom => 10
          case PanelPosition.Left | PanelPosition.Right => 30
