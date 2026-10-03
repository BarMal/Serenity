package com.serenity.state.manager

import cats.effect.IO
import com.serenity.state.models.*
import com.serenity.ui.layout.*

final private[manager] class StateManagerEditorCapability(
    modelCommit: ModelCommit,
    operations: StateManagerOperationBoundary
):

  def getModel: IO[Model] = modelCommit.model

  def getCurrentState: IO[AppState] = modelCommit.currentState

  def updateStateValidated(update: AppState => AppState): IO[Unit] =
    operations.dispatch(modelCommit.currentState.flatMap(state => modelCommit.commitState(update(state), state)))

  def createPane(bufferId: Option[BufferId] = None): IO[PaneId] =
    modelCommit.currentState.flatMap { state =>
      val (newState, paneId) = EditorTransitions.paneInserted(
        state,
        state.persisted.layout.orderedPaneIds.lastOption,
        bufferId,
        SplitAxis.Horizontal
      )
      modelCommit.commitState(newState, state).as(paneId)
    }

  def switchToPane(paneId: PaneId): IO[Unit] =
    modelCommit.currentState.flatMap { state =>
      EditorTransitions.paneSwitched(state, paneId).fold(IO.unit)(modelCommit.commitState(_, state))
    }

  def getTabOrder(): IO[List[PaneId]] =
    modelCommit.currentState.map(_.persisted.layout.orderedPaneIds)
