package com.serenity.state.components

import com.serenity.keystroke.events.FileEvent
import com.serenity.state.models.{AppState, PaneId}
import com.serenity.state.reducers.{FileEventReducer, ReducerResult}

class FileComponent:

  def reduce(event: FileEvent, currentState: AppState): ReducerResult =
    FileEventReducer.reduce(event, currentState)

  def reduceForPane(event: FileEvent, paneId: PaneId, currentState: AppState): ReducerResult =
    FileEventReducer.reduceForPane(event, paneId, currentState)
