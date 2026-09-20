package com.serenity.state.core

import com.serenity.config.DefaultDocumentMode
import com.serenity.lsp.config.LanguageId
import com.serenity.richtext.RichTextDocument
import com.serenity.state.models.*
import com.serenity.ui.layout.{SplitAxis, WorkspaceNode, WorkspaceNodeId, WorkspaceTree}

object EditorState:

  def openNewTab(state: AppState)(using com.serenity.rope.Balance): AppState =
    val (withBuffer, newBufferId) = createNewEmptyBuffer(state)
    focusBuffer(
      rebalancePanes(insertBufferInOrder(withBuffer, newBufferId), Some(newBufferId)),
      newBufferId
    )

  def createNewEmptyBuffer(state: AppState)(using com.serenity.rope.Balance): (AppState, BufferId) =
    val bufferId = state.runtime.nextBufferId
    val buffer   = newEmptyBuffer(bufferId, state.persisted.config.defaultDocumentMode)
    (
      state.copy(
        persisted = state.persisted.copy(buffers = state.persisted.buffers + (bufferId -> buffer)),
        runtime = state.runtime.copy(nextBufferId = BufferId(bufferId.value + 1))
      ),
      bufferId
    )

  private def newEmptyBuffer(bufferId: BufferId, mode: DefaultDocumentMode)(using com.serenity.rope.Balance): Buffer =
    val buffer = Buffer.newEmpty(bufferId)
    mode match
      case DefaultDocumentMode.PlainText =>
        buffer
      case DefaultDocumentMode.Markdown =>
        buffer.copy(document = buffer.document.copy(language = Some(LanguageId.Markdown)))
      case DefaultDocumentMode.RichText =>
        buffer.copy(richText = buffer.richText.copy(richTextDocument = Some(RichTextDocument.fromPlainText(""))))

  def insertBufferInOrder(state: AppState, newBufferId: BufferId): AppState =
    state.focusedBufferId match
      case Some(currentBufferId) =>
        val currentIndex = state.persisted.bufferOrder.indexOf(currentBufferId)
        if currentIndex == -1 then
          state.copy(persisted = state.persisted.copy(bufferOrder = state.persisted.bufferOrder :+ newBufferId))
        else
          val (before, after) = state.persisted.bufferOrder.splitAt(currentIndex + 1)
          state.copy(persisted = state.persisted.copy(bufferOrder = before ++ List(newBufferId) ++ after))
      case None =>
        state.copy(persisted = state.persisted.copy(bufferOrder = state.persisted.bufferOrder :+ newBufferId))

  def rebalancePanes(state: AppState, focusedBufferId: Option[BufferId] = None): AppState =
    assignBuffersToPanes(state, focusedBufferId)

  def focusBuffer(state: AppState, bufferId: BufferId): AppState =
    state.persisted.layout.editorPanes.find(_._2.bufferId.contains(bufferId)) match
      case Some((paneId, _)) =>
        state.copy(
          persisted = state.persisted.copy(
            focus = Focus.EditorPane(paneId),
            layout = state.persisted.layout.copy(activeEditorPaneId = Some(paneId))
          )
        )
      case None =>
        state

  def navigateToNextBuffer(state: AppState): AppState =
    navigateBuffer(state, _.nextBufferInOrder)

  def navigateToPreviousBuffer(state: AppState): AppState =
    navigateBuffer(state, _.previousBufferInOrder)

  /** Closes the currently focused tab -- `Ctrl+W`'s keyboard path, which only ever knows the focused buffer.
    * Generalised by [[closeBuffer]] (issue #1078), which a tab-bar mouse close click can also target by an arbitrary
    * `BufferId`, whether or not it is focused.
    */
  def closeFocusedTab(state: AppState): AppState =
    state.focusedBufferId match
      case Some(bufferId) => closeBuffer(state, bufferId)
      case None           => state

  /** Closes `bufferId` wherever it is -- the focused tab, or (issue #1078: a tab-bar close-affordance click) any other
    * open tab -- the single close-by-id path both the keyboard's focused-only `CloseTab` and the mouse's per-tab close
    * share. Closing the focused buffer reassigns focus/panes exactly as `closeFocusedTab` always did (falling back to
    * the next remaining buffer, or leaving the pane empty once none remain); closing a buffer that is not focused only
    * drops it from `bufferOrder`/`buffers` (and clears it from any pane showing it, via [[removeBuffer]]), leaving
    * which buffer is focused untouched.
    */
  def closeBuffer(state: AppState, bufferId: BufferId): AppState =
    val withoutBuffer = removeBuffer(state, bufferId)
    if state.focusedBufferId.contains(bufferId) then
      nextRemainingBuffer(state, bufferId) match
        case Some(nextBufferId) => focusBuffer(rebalancePanes(withoutBuffer, Some(nextBufferId)), nextBufferId)
        case None               => withoutBuffer
    else withoutBuffer

  def removeBuffer(state: AppState, bufferId: BufferId): AppState =
    val updatedPanes = state.persisted.layout.editorPanes.view.mapValues { pane =>
      if pane.bufferId.contains(bufferId) then pane.copy(bufferId = None) else pane
    }.toMap

    state.copy(
      persisted = state.persisted.copy(
        buffers = state.persisted.buffers - bufferId,
        bufferOrder = state.persisted.bufferOrder.filterNot(_ == bufferId),
        layout = state.persisted.layout.copy(editorPanes = updatedPanes)
      )
    )

  /** Moves `from` to sit immediately before `to` within `bufferOrder` -- issue #1079's drag-to-reorder. Reordering the
    * tab strip is exactly reordering `bufferOrder`: `TabListContent.build` derives the tab list from it one-to-one, so
    * there is no separate "tab order" to update, and which buffer each pane shows (and which is focused) is untouched.
    * "Insert before the drop target" is the only convention this can express without a finer, sub-tab-cell drop
    * position (deferred per the issue's own scope note) -- dropping `from` on the tab immediately after its current
    * position, or on itself, is therefore already a no-op, satisfying "a drag that ends where it began leaves order
    * unchanged". `from`/`to` no longer being open tabs is a no-op too, guarding a drag tick against a tab that closed
    * mid-gesture.
    */
  def reorderBuffer(state: AppState, from: BufferId, to: BufferId): AppState =
    val order = state.persisted.bufferOrder
    if from == to || !order.contains(from) || !order.contains(to) then state
    else
      val without         = order.filterNot(_ == from)
      val (before, after) = without.splitAt(without.indexOf(to))
      state.copy(persisted = state.persisted.copy(bufferOrder = before ++ (from :: after)))

  def removePane(state: AppState, paneId: PaneId): AppState =
    state.persisted.layout.editorPanes.get(paneId) match
      case None =>
        state
      case Some(pane) if state.persisted.layout.editorPanes.size == 1 =>
        val retainedTree =
          state.persisted.layout.workspaceTree.orElse(
            Some(WorkspaceTree(WorkspaceNode.Leaf(WorkspaceNodeId(s"editor-${paneId.value}"), paneId)))
          )
        state.copy(
          persisted = state.persisted.copy(
            layout = state.persisted.layout.copy(
              editorPanes = Map(paneId -> pane.copy(bufferId = None)),
              activeEditorPaneId = Some(paneId),
              workspaceTree = retainedTree
            ),
            focus = Focus.EditorPane(paneId)
          )
        )
      case Some(_) =>
        val previousOrder = state.persisted.layout.orderedPaneIds
        val removedIndex  = previousOrder.indexOf(paneId)
        val updatedPanes  = state.persisted.layout.editorPanes - paneId
        val updatedTree   = state.persisted.layout.workspaceTree.flatMap(_.remove(paneId))
        val updatedOrder  = updatedTree.map(_.paneIds).getOrElse(previousOrder.filterNot(_ == paneId))
        val nextActivePaneId =
          if state.persisted.layout.activeEditorPaneId.contains(paneId) then
            updatedOrder.lift(removedIndex).orElse(updatedOrder.lastOption)
          else state.persisted.layout.activeEditorPaneId.filter(updatedPanes.contains).orElse(updatedOrder.headOption)
        val nextFocus =
          state.persisted.focus match
            case Focus.EditorPane(`paneId`) =>
              nextActivePaneId.map(Focus.EditorPane.apply).getOrElse(state.persisted.focus)
            case _ => state.persisted.focus

        state.copy(
          persisted = state.persisted.copy(
            layout = state.persisted.layout.copy(
              editorPanes = updatedPanes,
              activeEditorPaneId = nextActivePaneId,
              workspaceTree = updatedTree
            ),
            focus = nextFocus
          ),
          runtime =
            state.runtime.copy(focusHistory = state.runtime.focusHistory.filterNot(_ == Focus.EditorPane(paneId)))
        )

  /** Splits the currently focused (or active, if focus is elsewhere -- e.g. a surface) editor pane along `axis`,
    * carrying its buffer into the new pane so both show the same file, mirroring every mainstream editor's split
    * behaviour. The new pane becomes focused/active, matching `insertPane`'s own convention.
    */
  def splitFocusedPane(state: AppState, axis: SplitAxis): AppState =
    focusedPaneId(state) match
      case None => state
      case Some(paneId) =>
        val newPaneId = state.runtime.nextPaneId
        val newPane =
          state.persisted.layout.editorPanes.get(paneId).flatMap(_.bufferId) match
            case Some(bufferId) => EditorPane.withBuffer(newPaneId, bufferId)
            case None           => EditorPane.empty(newPaneId)

        state.persisted.layout.workspaceTree
          .flatMap(
            _.split(
              paneId,
              newPaneId,
              axis,
              WorkspaceNodeId(s"split-${paneId.value}-${newPaneId.value}"),
              WorkspaceNodeId(s"editor-${newPaneId.value}")
            )
          )
          .fold(state) { tree =>
            state.copy(
              persisted = state.persisted.copy(
                layout = state.persisted.layout.copy(
                  editorPanes = state.persisted.layout.editorPanes.updated(newPaneId, newPane),
                  activeEditorPaneId = Some(newPaneId),
                  workspaceTree = Some(tree)
                ),
                focus = Focus.EditorPane(newPaneId)
              ),
              runtime = state.runtime.copy(nextPaneId = PaneId(newPaneId.value + 1))
            )
          }

  /** Removes the currently focused (or active, if focus is elsewhere -- e.g. a surface) editor pane. A no-op if there
    * is no pane to target.
    */
  def removeFocusedPane(state: AppState): AppState =
    focusedPaneId(state) match
      case Some(paneId) => removePane(state, paneId)
      case None         => state

  /** The pane a global, focus-independent pane action (split, close, directional-focus-move) should target: the focused
    * editor pane, or -- when focus is elsewhere, e.g. a surface -- the last active one.
    */
  private def focusedPaneId(state: AppState): Option[PaneId] =
    state.persisted.focus match
      case Focus.EditorPane(paneId) => Some(paneId)
      case _                        => state.persisted.layout.activeEditorPaneId

  private def assignBuffersToPanes(state: AppState, focusedBufferId: Option[BufferId]): AppState =
    val targetFocusedBuffer = focusedBufferId.orElse(state.focusedBufferId)
    targetFocusedBuffer match
      case Some(focusedBufferId) =>
        val stateWithPane = ensureEditorPane(state)
        stateWithPane.persisted.layout.editorPanes.find(_._2.bufferId.contains(focusedBufferId)) match
          case Some((paneId, _)) =>
            stateWithPane.copy(
              persisted = stateWithPane.persisted.copy(
                layout = stateWithPane.persisted.layout.copy(activeEditorPaneId = Some(paneId)),
                focus = Focus.EditorPane(paneId)
              )
            )
          case None =>
            val targetPaneId =
              stateWithPane.persisted.focus match
                case Focus.EditorPane(paneId) if stateWithPane.persisted.layout.editorPanes.contains(paneId) =>
                  Some(paneId)
                case _ =>
                  stateWithPane.persisted.layout.activeEditorPaneId.filter(
                    stateWithPane.persisted.layout.editorPanes.contains
                  )
            targetPaneId
              .flatMap(stateWithPane.persisted.layout.editorPanes.get)
              .map { pane =>
                val updatedPane = pane.copy(bufferId = Some(focusedBufferId))
                stateWithPane.copy(
                  persisted = stateWithPane.persisted.copy(
                    layout = stateWithPane.persisted.layout.copy(
                      editorPanes = stateWithPane.persisted.layout.editorPanes.updated(pane.id, updatedPane),
                      activeEditorPaneId = Some(pane.id)
                    ),
                    focus = Focus.EditorPane(pane.id)
                  )
                )
              }
              .getOrElse(stateWithPane)

      case None =>
        state

  private def ensureEditorPane(state: AppState): AppState =
    if state.persisted.layout.editorPanes.nonEmpty then state
    else
      val paneId = state.runtime.nextPaneId
      val tree   = WorkspaceTree(WorkspaceNode.Leaf(WorkspaceNodeId(s"editor-${paneId.value}"), paneId))
      state.copy(
        persisted = state.persisted.copy(
          layout = state.persisted.layout.copy(
            editorPanes = Map(paneId -> EditorPane.empty(paneId)),
            activeEditorPaneId = Some(paneId),
            workspaceTree = Some(tree)
          ),
          focus = Focus.EditorPane(paneId)
        ),
        runtime = state.runtime.copy(nextPaneId = PaneId(paneId.value + 1))
      )

  private def navigateBuffer(
    state: AppState,
    nextBuffer: AppState => BufferId => Option[BufferId]
  ): AppState =
    if state.persisted.bufferOrder.isEmpty then state
    else
      state.focusedBufferId match
        case Some(currentBufferId) =>
          nextBuffer(state)(currentBufferId) match
            case Some(bufferId) =>
              focusBuffer(rebalancePanes(state, Some(bufferId)), bufferId)
            case None =>
              state
        case None =>
          state.persisted.bufferOrder.headOption match
            case Some(firstBufferId) => focusBuffer(state, firstBufferId)
            case None                => state

  private def nextRemainingBuffer(state: AppState, removedBufferId: BufferId): Option[BufferId] =
    val remainingBuffers = state.persisted.bufferOrder.filterNot(_ == removedBufferId)
    if remainingBuffers.isEmpty then None
    else
      val removedIndex = state.persisted.bufferOrder.indexOf(removedBufferId)
      if removedIndex == -1 then remainingBuffers.headOption
      else remainingBuffers.lift(math.min(removedIndex, remainingBuffers.size - 1)).orElse(remainingBuffers.lastOption)
