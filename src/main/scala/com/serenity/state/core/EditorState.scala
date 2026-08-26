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
    val bufferId = state.nextBufferId
    val buffer   = newEmptyBuffer(bufferId, state.config.defaultDocumentMode)
    (
      state.copy(
        buffers = state.buffers + (bufferId -> buffer),
        nextBufferId = BufferId(bufferId.value + 1)
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
        val currentIndex = state.bufferOrder.indexOf(currentBufferId)
        if currentIndex == -1 then state.copy(bufferOrder = state.bufferOrder :+ newBufferId)
        else
          val (before, after) = state.bufferOrder.splitAt(currentIndex + 1)
          state.copy(bufferOrder = before ++ List(newBufferId) ++ after)
      case None =>
        state.copy(bufferOrder = state.bufferOrder :+ newBufferId)

  def rebalancePanes(state: AppState, focusedBufferId: Option[BufferId] = None): AppState =
    assignBuffersToPanes(state, focusedBufferId)

  def focusBuffer(state: AppState, bufferId: BufferId): AppState =
    state.layout.editorPanes.find(_._2.bufferId.contains(bufferId)) match
      case Some((paneId, _)) =>
        state.copy(
          focus = Focus.EditorPane(paneId),
          layout = state.layout.copy(activeEditorPaneId = Some(paneId))
        )
      case None =>
        state

  def navigateToNextBuffer(state: AppState): AppState =
    navigateBuffer(state, _.nextBufferInOrder)

  def navigateToPreviousBuffer(state: AppState): AppState =
    navigateBuffer(state, _.previousBufferInOrder)

  def closeFocusedTab(state: AppState): AppState =
    state.focus match
      case Focus.EditorPane(paneId) =>
        state.layout.editorPanes.get(paneId) match
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
    val updatedPanes = state.layout.editorPanes.view.mapValues { pane =>
      if pane.bufferId.contains(bufferId) then pane.copy(bufferId = None) else pane
    }.toMap

    state.copy(
      buffers = state.buffers - bufferId,
      bufferOrder = state.bufferOrder.filterNot(_ == bufferId),
      layout = state.layout.copy(editorPanes = updatedPanes)
    )

  def removePane(state: AppState, paneId: PaneId): AppState =
    state.layout.editorPanes.get(paneId) match
      case None =>
        state
      case Some(pane) if state.layout.editorPanes.size == 1 =>
        val retainedTree =
          state.layout.effectiveWorkspaceTree.orElse(
            Some(WorkspaceTree(WorkspaceNode.Leaf(WorkspaceNodeId(s"editor-${paneId.value}"), paneId)))
          )
        state.copy(
          layout = state.layout.copy(
            editorPanes = Map(paneId -> pane.copy(bufferId = None)),
            activeEditorPaneId = Some(paneId),
            paneOrder = List(paneId),
            workspaceTree = retainedTree
          ),
          focus = Focus.EditorPane(paneId)
        )
      case Some(_) =>
        val previousOrder = state.layout.orderedPaneIds
        val removedIndex  = previousOrder.indexOf(paneId)
        val updatedPanes  = state.layout.editorPanes - paneId
        val updatedTree   = state.layout.effectiveWorkspaceTree.flatMap(_.remove(paneId))
        val updatedOrder  = updatedTree.map(_.paneIds).getOrElse(previousOrder.filterNot(_ == paneId))
        val nextActivePaneId =
          if state.layout.activeEditorPaneId.contains(paneId) then
            updatedOrder.lift(removedIndex).orElse(updatedOrder.lastOption)
          else state.layout.activeEditorPaneId.filter(updatedPanes.contains).orElse(updatedOrder.headOption)
        val nextFocus =
          state.focus match
            case Focus.EditorPane(`paneId`) => nextActivePaneId.map(Focus.EditorPane.apply).getOrElse(state.focus)
            case _                          => state.focus

        state.copy(
          layout = state.layout.copy(
            editorPanes = updatedPanes,
            paneOrder = updatedOrder,
            activeEditorPaneId = nextActivePaneId,
            workspaceTree = updatedTree
          ),
          focus = nextFocus,
          focusHistory = state.focusHistory.filterNot(_ == Focus.EditorPane(paneId))
        )

  private def assignBuffersToPanes(state: AppState, focusedBufferId: Option[BufferId]): AppState =
    val targetFocusedBuffer = focusedBufferId.orElse(state.focusedBufferId)
    targetFocusedBuffer match
      case Some(focusedBufferId) =>
        val stateWithPane = ensureEditorPane(state)
        stateWithPane.layout.editorPanes.find(_._2.bufferId.contains(focusedBufferId)) match
          case Some((paneId, _)) =>
            stateWithPane.copy(
              layout = stateWithPane.layout.copy(activeEditorPaneId = Some(paneId)),
              focus = Focus.EditorPane(paneId)
            )
          case None =>
            val targetPaneId =
              stateWithPane.focus match
                case Focus.EditorPane(paneId) if stateWithPane.layout.editorPanes.contains(paneId) => Some(paneId)
                case _ => stateWithPane.layout.activeEditorPaneId.filter(stateWithPane.layout.editorPanes.contains)
            targetPaneId
              .flatMap(stateWithPane.layout.editorPanes.get)
              .map { pane =>
                val updatedPane = pane.copy(bufferId = Some(focusedBufferId))
                stateWithPane.copy(
                  layout = stateWithPane.layout.copy(
                    editorPanes = stateWithPane.layout.editorPanes.updated(pane.id, updatedPane),
                    activeEditorPaneId = Some(pane.id)
                  ),
                  focus = Focus.EditorPane(pane.id)
                )
              }
              .getOrElse(stateWithPane)

      case None =>
        state

  private def ensureEditorPane(state: AppState): AppState =
    if state.layout.editorPanes.nonEmpty then state
    else
      val paneId = state.nextPaneId
      val tree   = WorkspaceTree(WorkspaceNode.Leaf(WorkspaceNodeId(s"editor-${paneId.value}"), paneId))
      state.copy(
        layout = state.layout.copy(
          editorPanes = Map(paneId -> EditorPane.empty(paneId)),
          activeEditorPaneId = Some(paneId),
          paneOrder = List(paneId),
          workspaceTree = Some(tree)
        ),
        focus = Focus.EditorPane(paneId),
        nextPaneId = PaneId(paneId.value + 1)
      )

  private def navigateBuffer(
    state: AppState,
    nextBuffer: AppState => BufferId => Option[BufferId]
  ): AppState =
    if state.bufferOrder.isEmpty then state
    else
      state.focusedBufferId match
        case Some(currentBufferId) =>
          nextBuffer(state)(currentBufferId) match
            case Some(bufferId) =>
              focusBuffer(rebalancePanes(state, Some(bufferId)), bufferId)
            case None =>
              state
        case None =>
          state.bufferOrder.headOption match
            case Some(firstBufferId) => focusBuffer(state, firstBufferId)
            case None                => state

  private def nextRemainingBuffer(state: AppState, removedBufferId: BufferId): Option[BufferId] =
    val remainingBuffers = state.bufferOrder.filterNot(_ == removedBufferId)
    if remainingBuffers.isEmpty then None
    else
      val removedIndex = state.bufferOrder.indexOf(removedBufferId)
      if removedIndex == -1 then remainingBuffers.headOption
      else remainingBuffers.lift(math.min(removedIndex, remainingBuffers.size - 1)).orElse(remainingBuffers.lastOption)
