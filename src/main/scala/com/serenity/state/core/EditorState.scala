package com.serenity.state.core

import com.serenity.config.DefaultDocumentMode
import com.serenity.lsp.config.LanguageId
import com.serenity.richtext.RichTextDocument
import com.serenity.state.models.*
import com.serenity.ui.layout.{WorkspaceNode, WorkspaceNodeId, WorkspaceTree}

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

  def closeFocusedTab(state: AppState): AppState =
    state.persisted.focus match
      case Focus.EditorPane(paneId) =>
        state.persisted.layout.editorPanes.get(paneId) match
          case Some(pane) =>
            pane.bufferId match
              case Some(bufferId) =>
                val withoutBuffer  = removeBuffer(state, bufferId)
                val fallbackBuffer = nextRemainingBuffer(state, bufferId)
                val rebalancedState =
                  fallbackBuffer match
                    case Some(nextBufferId) =>
                      focusBuffer(rebalancePanes(withoutBuffer, Some(nextBufferId)), nextBufferId)
                    case None =>
                      withoutBuffer

                rebalancedState
              case None =>
                state
          case None =>
            state
      case _ =>
        state

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

  def removePane(state: AppState, paneId: PaneId): AppState =
    state.persisted.layout.editorPanes.get(paneId) match
      case None =>
        state
      case Some(pane) if state.persisted.layout.editorPanes.size == 1 =>
        val retainedTree =
          state.persisted.layout.effectiveWorkspaceTree.orElse(
            Some(WorkspaceTree(WorkspaceNode.Leaf(WorkspaceNodeId(s"editor-${paneId.value}"), paneId)))
          )
        state.copy(
          persisted = state.persisted.copy(
            layout = state.persisted.layout.copy(
              editorPanes = Map(paneId -> pane.copy(bufferId = None)),
              activeEditorPaneId = Some(paneId),
              paneOrder = List(paneId),
              workspaceTree = retainedTree
            ),
            focus = Focus.EditorPane(paneId)
          )
        )
      case Some(_) =>
        val previousOrder = state.persisted.layout.orderedPaneIds
        val removedIndex  = previousOrder.indexOf(paneId)
        val updatedPanes  = state.persisted.layout.editorPanes - paneId
        val updatedTree   = state.persisted.layout.effectiveWorkspaceTree.flatMap(_.remove(paneId))
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
              paneOrder = updatedOrder,
              activeEditorPaneId = nextActivePaneId,
              workspaceTree = updatedTree
            ),
            focus = nextFocus
          ),
          runtime =
            state.runtime.copy(focusHistory = state.runtime.focusHistory.filterNot(_ == Focus.EditorPane(paneId)))
        )

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
            paneOrder = List(paneId),
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
