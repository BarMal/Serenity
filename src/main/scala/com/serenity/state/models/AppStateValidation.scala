package com.serenity.state.models

import com.serenity.ui.layout.{SplitAxis, WorkspaceNodeId, WorkspaceTree}

/** Invariant checking and workspace-tree reconciliation for [[AppState]], pulled out of that file to keep it under
  * the architecture ratchet's file-length target. Pure functions over an [[AppState]] snapshot -- no state of their
  * own.
  */
private[models] object AppStateValidation:

  def validationErrors(state: AppState): List[String] =
    val errors = List.newBuilder[String]

    // Focus validation. `Focus` has no "unfocused" case, so a workspace with zero editor panes (e.g. `AppState.empty`,
    // used as the base for constructing test/startup states) necessarily carries an `EditorPane` focus that cannot
    // resolve to anything real. That is not a dangling reference -- there is nothing an empty workspace's focus could
    // legitimately point to -- so it's only flagged once at least one editor pane exists to make the reference stale.
    state.persisted.focus match
      case Focus.EditorPane(paneId)
          if state.persisted.layout.editorPanes.nonEmpty && !state.persisted.layout.editorPanes.contains(paneId) =>
        errors += s"Focus points to non-existent pane: $paneId"
      case Focus.Surface(surfaceId) if state.surfaceById(surfaceId).isEmpty =>
        errors += s"Focus points to non-existent surface: $surfaceId"
      case _ => // Valid focus
    errors ++= orderAndIdentityErrors(state)
    errors ++= documentPositionErrors(state)
    // Buffer-Pane consistency
    state.persisted.layout.editorPanes.foreach { (paneId, pane) =>
      pane.bufferId.foreach { bufferId =>
        if !state.persisted.buffers.contains(bufferId) then
          errors += s"Pane $paneId references non-existent buffer: $bufferId"
      }
    }
    state.persisted.layout.workspaceTree.foreach { tree =>
      val pinnedSurfaceIds = state.runtime.uiSurfaces.collect {
        case UiSurface(id, _, SurfacePresentation.Pinned(_, _), _) => id
      }.toSet
      errors ++= tree.validationErrors(state.persisted.layout.editorPanes.keySet, pinnedSurfaceIds)
      state.persisted.focus match
        case Focus.EditorPane(paneId) if !tree.paneIds.contains(paneId) =>
          errors += s"Focus points outside workspace tree: $paneId"
        case _ =>
      state.persisted.layout.maximizedWorkspaceNodeId.foreach { nodeId =>
        if tree.surfaceIdForNode(nodeId).isEmpty then
          errors += s"Maximised workspace node is not a docked surface: ${nodeId.value}"
      }
    }

    errors.result()

  /** Active-pane coherence, pane/buffer-order coherence (no stale references, no repeats), duplicate surfaces, and
    * next-ID allocation safety.
    */
  private def orderAndIdentityErrors(state: AppState): List[String] =
    val errors = List.newBuilder[String]

    state.persisted.layout.activeEditorPaneId.foreach { paneId =>
      if !state.persisted.layout.editorPanes.contains(paneId) then
        errors += s"Active editor pane does not exist: ${paneId.value}"
    }
    // Pane-order coherence: no stale references, no repeats. (Completeness against `editorPanes` is intentionally not
    // required here -- the workspace tree reconciliation pass is the source of truth for a fully-covering order.)
    val duplicatePaneOrder = duplicates(state.persisted.layout.paneOrder.map(_.value))
    if duplicatePaneOrder.nonEmpty then
      errors += s"Pane order contains duplicate entries: ${duplicatePaneOrder.mkString(", ")}"
    val stalePaneOrder = state.persisted.layout.paneOrder.filterNot(state.persisted.layout.editorPanes.contains)
    if stalePaneOrder.nonEmpty then
      errors += s"Pane order references non-existent panes: ${stalePaneOrder.map(_.value).mkString(", ")}"
    // Buffer-order coherence: no stale references, no repeats.
    val duplicateBufferOrder = duplicates(state.persisted.bufferOrder.map(_.value))
    if duplicateBufferOrder.nonEmpty then
      errors += s"Buffer order contains duplicate entries: ${duplicateBufferOrder.mkString(", ")}"
    val staleBufferOrder = state.persisted.bufferOrder.filterNot(state.persisted.buffers.contains)
    if staleBufferOrder.nonEmpty then
      errors += s"Buffer order references non-existent buffers: ${staleBufferOrder.map(_.value).mkString(", ")}"
    // Duplicate surfaces
    val duplicateSurfaceIds = duplicates(state.runtime.uiSurfaces.map(_.id.value))
    if duplicateSurfaceIds.nonEmpty then errors += s"Duplicate UI surfaces: ${duplicateSurfaceIds.mkString(", ")}"
    // Next-ID allocation must not already be in use, or the next allocation collides with a live object.
    // `nextSurfaceId` is deliberately excluded: unlike buffers/panes, surface IDs are also assigned by hand from
    // fixed string literals (e.g. "context-menu", or a caller-chosen "surface-N" in tests) that never touch the
    // counter, so a numeric coincidence there isn't evidence of a real allocation bug.
    if state.persisted.buffers.contains(state.runtime.nextBufferId) then
      errors += s"Next buffer ID collides with an existing buffer: ${state.runtime.nextBufferId.value}"
    if state.persisted.layout.editorPanes.contains(state.runtime.nextPaneId) then
      errors += s"Next pane ID collides with an existing pane: ${state.runtime.nextPaneId.value}"

    errors.result()

  /** Every cursor, selection endpoint, bookmark and comment position must name a line that actually exists in that
    * buffer's current content. Column is checked only for non-negativity, not against the line's length: this codebase
    * routinely carries a cursor/selection column past end-of-line between an edit and the next clamp
    * (`Rope.lineColumnToOffset` clamps on read rather than rejecting), so a column-vs-line-length check would flag that
    * ordinary, self-correcting slack as a hard commit failure. The line itself identifies *which document position this
    * is*, which is the coordinate that must never dangle.
    */
  private def documentPositionErrors(state: AppState): List[String] =
    val errors = List.newBuilder[String]

    state.persisted.buffers.foreach { (bufferId, buffer) =>
      def outOfBounds(position: CursorPosition): Boolean =
        position.line < 0 || position.line >= buffer.document.content.lineCount || position.column < 0

      val badCursors = buffer.editing.cursors.filter(outOfBounds)
      if badCursors.nonEmpty then
        errors += s"Buffer ${bufferId.value} has out-of-bounds cursor(s): ${badCursors.mkString(", ")}"
      val badSelections = buffer.allSelections.flatMap(s => List(s.anchor, s.focus)).filter(outOfBounds)
      if badSelections.nonEmpty then
        errors += s"Buffer ${bufferId.value} has out-of-bounds selection position(s): ${badSelections.mkString(", ")}"
      val badBookmarks = buffer.annotations.bookmarks.filter(outOfBounds)
      if badBookmarks.nonEmpty then
        errors += s"Buffer ${bufferId.value} has out-of-bounds bookmark(s): ${badBookmarks.mkString(", ")}"
      val badComments =
        buffer.annotations.documentComments.flatMap(c => List(c.anchor, c.focus)).filter(outOfBounds)
      if badComments.nonEmpty then
        errors += s"Buffer ${bufferId.value} has out-of-bounds comment position(s): ${badComments.mkString(", ")}"
    }

    errors.result()

  private def duplicates[A](values: List[A]): List[A] =
    values
      .groupMapReduce(identity)(_ => 1)(_ + _)
      .collect { case (value, count) if count > 1 => value }
      .toList
      .sortBy(_.toString)

  def validated(state: AppState): Either[List[String], AppState] =
    val reconciled = reconcileWorkspaceTree(state)
    if reconciled.isValid then Right(reconciled) else Left(reconciled.validationErrors)

  private def reconcileWorkspaceTree(state: AppState): AppState =
    state.persisted.layout.workspaceTree match
      case None                                                      => state
      case Some(tree) if workspaceTreeAlreadyReconciled(state, tree) => state
      case Some(tree) =>
        val paneIds = state.persisted.layout.editorPanes.keySet
        val prunedPanes = tree.paneIds
          .filterNot(paneIds.contains)
          .foldLeft(Option(tree)) {
            case (Some(currentTree), paneId) => currentTree.remove(paneId)
            case (None, _)                   => None
          }
        val paneReconciledTree = state.persisted.layout.paneOrder
          .filter(paneIds.contains)
          .foldLeft(prunedPanes) {
            case (Some(currentTree), paneId) if !currentTree.paneIds.contains(paneId) =>
              val splitId = WorkspaceNodeId(s"reconcile-pane-${paneId.value}")
              currentTree
                .split(
                  currentTree.paneIds.lastOption.getOrElse(paneId),
                  paneId,
                  SplitAxis.fromLegacy(state.persisted.layout.splitDirection),
                  splitId,
                  WorkspaceNodeId(s"reconcile-pane-leaf-${paneId.value}")
                )
            case (currentTree, _) => currentTree
          }
          .getOrElse(tree)
        val pinned = state.runtime.uiSurfaces.collect {
          case UiSurface(id, _, SurfacePresentation.Pinned(position, _), _) => id -> position
        }
        val pinnedIds = pinned.map(_._1).toSet
        val prunedTree = paneReconciledTree.dockedSurfaceIds
          .filterNot(pinnedIds.contains)
          .foldLeft(paneReconciledTree) {
            case (currentTree, surfaceId) =>
              currentTree.removeSurface(surfaceId).getOrElse(currentTree)
          }
        val reconciledTree = pinned.zipWithIndex.foldLeft(prunedTree) {
          case (currentTree, ((surfaceId, position), index)) =>
            if currentTree.dockedSurfaceIds.contains(surfaceId) then
              if currentTree.positionForSurface(surfaceId).contains(position) then currentTree
              else
                currentTree
                  .moveSurface(surfaceId, position, WorkspaceNodeId(s"reconcile-dock-$index-${surfaceId.value}"))
                  .getOrElse(currentTree)
            else
              val splitId = WorkspaceNodeId(s"dock-${surfaceId.value}")
              val leafId  = WorkspaceNodeId(s"dock-leaf-${surfaceId.value}")
              currentTree.dock(surfaceId, position, splitId, leafId).getOrElse(currentTree)
        }
        val orderedTree = pinned
          .groupBy(_._2)
          .foldLeft(reconciledTree) {
            case (currentTree, (position, surfacesAtPosition)) =>
              val desiredOrder = surfacesAtPosition.map(_._1)
              val currentOrder = currentTree.dockedSurfaceIds.filter { surfaceId =>
                currentTree.positionForSurface(surfaceId).contains(position)
              }
              if currentOrder == desiredOrder then currentTree
              else
                desiredOrder.zipWithIndex.foldLeft(currentTree) {
                  case (tree, (surfaceId, index)) =>
                    tree
                      .moveSurface(
                        surfaceId,
                        position,
                        WorkspaceNodeId(s"reconcile-order-${position.toString.toLowerCase}-$index-${surfaceId.value}")
                      )
                      .getOrElse(tree)
                }
          }
        state.copy(persisted =
          state.persisted.copy(layout =
            state.persisted.layout.copy(workspaceTree = Some(orderedTree), paneOrder = orderedTree.paneIds)
          )
        )

  // Cheap pre-check for the common case where no pane or pinned-surface change requires rebuilding the tree,
  // so events that don't touch panes/docking (e.g. command-palette navigation) skip the full reconciliation pass.
  private def workspaceTreeAlreadyReconciled(state: AppState, tree: WorkspaceTree): Boolean =
    tree.dockedSurfaceIds.isEmpty &&
      !state.runtime.uiSurfaces.exists {
        case UiSurface(_, _, SurfacePresentation.Pinned(_, _), _) => true
        case _                                                    => false
      } &&
      tree.paneIds.toSet == state.persisted.layout.editorPanes.keySet &&
      state.persisted.layout.paneOrder.filter(state.persisted.layout.editorPanes.keySet.contains) == tree.paneIds
