package com.serenity.state.reducers

import com.serenity.keystroke.events.*
import com.serenity.state.models.*

object FileEventReducer:

  def reduce(event: FileEvent, state: AppState): ReducerResult =
    reduceWithLookup(event, state, focusedBuffer)

  def reduceForPane(event: FileEvent, paneId: PaneId, state: AppState): ReducerResult =
    reduceWithLookup(event, state, bufferForPane(paneId, _))

  private def reduceWithLookup(
    event: FileEvent,
    state: AppState,
    bufferLookup: AppState => Option[Buffer]
  ): ReducerResult =
    event match
      case SaveFile =>
        bufferLookup(state) match
          case Some(buffer) if buffer.document.filePath.isDefined =>
            ReducerResult.withEffect(state, AppEffect.SaveBuffer(buffer.id))
          case _ =>
            ReducerResult.noEffects(state)

      case SaveFileAs(path) =>
        bufferLookup(state) match
          case Some(buffer) =>
            ReducerResult.withEffect(state, AppEffect.SaveBufferAs(buffer.id, path))
          case None =>
            ReducerResult.noEffects(state)

      case SaveAsFile =>
        ReducerResult.withEffect(state, AppEffect.RequestSaveAs())

      case OpenFile | OpenFileBrowser =>
        ReducerResult.withEffect(state, AppEffect.RequestOpenFile())

      case LoadFile(path) =>
        ReducerResult.withEffect(state, AppEffect.DirectLoadFile(path))

  private def focusedBuffer(state: AppState): Option[Buffer] =
    state.persisted.focus match
      case Focus.EditorPane(paneId) =>
        bufferForPane(paneId, state)
      case _ =>
        None

  private def bufferForPane(paneId: PaneId, state: AppState): Option[Buffer] =
    for
      pane     <- state.persisted.layout.editorPanes.get(paneId)
      bufferId <- pane.bufferId
      buffer   <- state.persisted.buffers.get(bufferId)
    yield buffer
