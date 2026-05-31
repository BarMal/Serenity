package com.serenity.state.core

import com.serenity.state.models.*
import com.serenity.ui.layout.{LayoutEngine, ViewportSize}

object EditorState:

  def openNewTab(state: AppState)(using com.serenity.rope.Balance): AppState =
    val (withBuffer, newBufferId) = createNewEmptyBuffer(state)
    focusBuffer(
      rebalancePanes(insertBufferInOrder(withBuffer, newBufferId), Some(newBufferId)),
      newBufferId
    )

  def createNewEmptyBuffer(state: AppState)(using com.serenity.rope.Balance): (AppState, BufferId) =
    val bufferId = state.nextBufferId
    val buffer   = Buffer.newEmpty(bufferId)
    (
      state.copy(
        buffers = state.buffers + (bufferId -> buffer),
        nextBufferId = BufferId(bufferId.value + 1)
      ),
      bufferId
    )

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

  private def assignBuffersToPanes(state: AppState, focusedBufferId: Option[BufferId]): AppState =
    val viewportSize        = state.viewportSize.getOrElse(ViewportSize(80, 24))
    val layout              = LayoutEngine.calculateLayout(state, viewportSize)
    val maxPossiblePanes    = math.max(1, layout.editorPanelRect.width / state.config.minimumPaneWidth)
    val targetFocusedBuffer = focusedBufferId.orElse(state.focusedBufferId)
    updatePaneAssignments(state, maxPossiblePanes, targetFocusedBuffer)

  private def updatePaneAssignments(
    state: AppState,
    maxVisiblePanes: Int,
    targetFocusedBuffer: Option[BufferId]
  ): AppState =
    targetFocusedBuffer match
      case Some(focusedBufferId) =>
        val focusedIndex = state.bufferOrder.indexOf(focusedBufferId)
        val startIndex =
          if focusedIndex == -1 then 0
          else math.max(0, focusedIndex - maxVisiblePanes / 2)
        val visibleBuffers = state.bufferOrder.slice(startIndex, startIndex + maxVisiblePanes)

        val neededPanes  = visibleBuffers.size
        val currentPanes = state.layout.editorPanes
        val paneIds      = state.layout.orderedPaneIds

        val updatedState =
          if paneIds.size < neededPanes then
            val newPaneIds =
              (paneIds.size until neededPanes).map(i => PaneId(state.nextPaneId.value + i - paneIds.size)).toList
            val additionalPanes = newPaneIds.map(id => id -> EditorPane.empty(id)).toMap
            val newNextPaneId = PaneId(
              math.max(state.nextPaneId.value, state.nextPaneId.value + neededPanes - paneIds.size)
            )
            state.copy(
              layout = state.layout.copy(
                editorPanes = currentPanes ++ additionalPanes,
                paneOrder = state.layout.paneOrder ++ newPaneIds
              ),
              nextPaneId = newNextPaneId
            )
          else state

        val finalPanes      = updatedState.layout.orderedPaneIds
        val paneAssignments = finalPanes.take(visibleBuffers.size).zip(visibleBuffers).toMap

        val assignedPanes = finalPanes.map { paneId =>
          paneAssignments.get(paneId) match
            case Some(bufferId) => paneId -> EditorPane.withBuffer(paneId, bufferId)
            case None           => paneId -> EditorPane.empty(paneId)
        }.toMap

        val finalState = updatedState.copy(
          layout = updatedState.layout.copy(editorPanes = assignedPanes)
        )

        assignedPanes.find(_._2.bufferId.contains(focusedBufferId)) match
          case Some((paneId, _)) =>
            finalState.copy(
              layout = finalState.layout.copy(activeEditorPaneId = Some(paneId)),
              focus = Focus.EditorPane(paneId)
            )
          case None =>
            finalState

      case None =>
        state

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
