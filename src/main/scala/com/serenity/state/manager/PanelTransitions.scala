package com.serenity.state.manager

import com.serenity.config.CommentDisplayMode
import com.serenity.lsp.config.LanguageId
import com.serenity.state.models.*
import com.serenity.state.undo.HistoryEntry
import com.serenity.ui.layout.{PanelPosition, WorkspaceTree}

/** What pinning a panel comes to, decided from the current state. */
private[manager] enum PanelPinPlan:
  case Commit(update: AppState => AppState)

  /** The explorer's first pin needs the working directory listed first, off the pure path. */
  case LoadExplorerRoot(size: Int)

  /** Nothing is pinned; `message` tells the user why. */
  case Report(message: String)

  /** Nothing is pinned; `debugLog` is what the shell logs about it. */
  case Ignore(debugLog: String)

/** Pinned-panel changes by [[PanelId]] as pure functions of the state -- what `StateManagerPanelEffects` commits. */
private[manager] object PanelTransitions:

  def pinPlan(id: PanelId, position: PanelPosition, state: AppState): PanelPinPlan =
    val size = PanelRegistry.registrationFor(id).defaultSize(position)
    def upsert(content: SurfaceContent): PanelPinPlan =
      PanelPinPlan.Commit(upsertPanel(id, content, position, size))
    id match
      case PanelId.Explorer =>
        newestPanelSurface(id, state).fold(PanelPinPlan.LoadExplorerRoot(size))(surface => upsert(surface.content))
      case PanelId.Outline =>
        upsert(PanelContentSync.outlineContent(state.activeBuffer))
      case PanelId.Comments =>
        // #1551: the pin-to-side command and the `CommentDisplayMode` setting used to disagree about whether comments
        // are visible -- pinning always showed live comment content regardless of the setting.
        if state.persisted.config.surfaceConfig.commentDisplayMode == CommentDisplayMode.Off then
          PanelPinPlan.Report("Comments are hidden -- comment display is turned off in Settings.")
        else upsert(PanelContentSync.commentsContent(state.activeBuffer))
      case PanelId.Diagnostics =>
        upsert(PanelContentSync.diagnosticsContent(state, state.activeBuffer))
      case PanelId.MarkdownPreview =>
        markdownPreviewContent(state).fold(
          PanelPinPlan.Ignore("[CMD] Markdown preview requested without an active Markdown buffer")
        )(upsert)
      case PanelId.ProjectOutput =>
        upsert(ProjectTaskTransitions.terminalContent(state))

  /** `update` applied to the model's state, declared as an undo boundary (#1016 PR4) in the same model when it changed
    * anything. A no-op records nothing, mirroring `AppEventReducer.closePaneResult`'s guard for pane close.
    */
  def panelChange(model: Model, update: AppState => AppState): Model =
    val updated = update(model.app)
    val undo =
      if updated == model.app then model.undo
      else model.undo.pushUndo(HistoryEntry.PanelChange.capture(model.app))
    model.copy(app = updated, undo = undo)

  def removePanel(id: PanelId)(state: AppState): AppState =
    val removedIds = state.runtime.uiSurfaces.collect {
      case surface if PanelId.forContent(surface.content).contains(id) => surface.id
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

  def upsertPanel(
    id: PanelId,
    content: SurfaceContent,
    position: PanelPosition,
    size: Int
  )(state: AppState): AppState =
    val matchingSurfaces = state.runtime.uiSurfaces.filter(surface => PanelId.forContent(surface.content).contains(id))
    val retainedSurface  = matchingSurfaces.reverse.headOption
    val stateWithoutPanel = state.copy(runtime =
      state.runtime.copy(uiSurfaces =
        state.runtime.uiSurfaces.filterNot(surface => PanelId.forContent(surface.content).contains(id))
      )
    )
    val droppedIds = matchingSurfaces.filterNot(surface => retainedSurface.exists(_.id == surface.id)).map(_.id).toSet
    val treeWithoutDropped = droppedIds.foldLeft(state.persisted.layout.workspaceTree) { (tree, id) =>
      tree.flatMap(_.removeSurface(id)).orElse(tree)
    }
    val (stateWithId, surface, isNewlyDocked) = retainedSurface match
      case Some(existing) =>
        (
          stateWithoutPanel,
          existing.copy(content = content, presentation = SurfacePresentation.Docked, dismissOnMove = false),
          false
        )
      case None =>
        (stateWithoutPanel, UiSurface(id.surfaceId, content, SurfacePresentation.Docked, dismissOnMove = false), true)
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
  /** Moves a docked panel to `index` among the panels on its edge, clamped to that edge's length. */
  def movePanelTo(id: PanelId, index: Int)(state: AppState): AppState =
    val reordered = for
      tree     <- state.persisted.layout.workspaceTree
      position <- tree.positionForSurface(id.surfaceId)
      sameEdge = tree.dockedSurfaceIds.filter(tree.positionForSurface(_).contains(position))
      from     = sameEdge.indexOf(id.surfaceId)
      to       = index.max(0).min(sameEdge.length - 1)
      if from >= 0 && from != to
    yield tree.reorderAt(position, moveWithinList(sameEdge, from, to))
    reordered.fold(state)(tree =>
      state.copy(persisted = state.persisted.copy(layout = state.persisted.layout.copy(workspaceTree = Some(tree))))
    )

  private def moveWithinList[A](values: List[A], from: Int, to: Int): List[A] =
    if from == to then values
    else
      values.lift(from) match
        case None => values
        case Some(value) =>
          val withoutValue = values.patch(from, Nil, 1)
          withoutValue.patch(to, List(value), 0)

  def withMarkdownPreviewWindowBuffer(state: AppState, bufferId: Option[BufferId]): AppState =
    state.copy(runtime = state.runtime.copy(markdownPreviewWindowBuffer = bufferId))

  def markdownPreviewBufferId(state: AppState): Option[BufferId] =
    activeMarkdownBuffer(state).map(_.id)

  private def markdownPreviewContent(state: AppState): Option[SurfaceContent] =
    activeMarkdownBuffer(state).map(PanelContentSync.markdownPreviewContent)

  private def activeMarkdownBuffer(state: AppState): Option[Buffer] =
    state.activeBuffer
      .filter(_.document.language.contains(LanguageId.Markdown))

  private def newestPanelSurface(id: PanelId, state: AppState): Option[UiSurface] =
    state.runtime.uiSurfaces.reverse.find(surface => PanelId.forContent(surface.content).contains(id))
