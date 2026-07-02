package com.serenity.state.manager

import java.nio.file.Path

import cats.effect.IO
import com.serenity.state.models.*

private[manager] trait StateManagerFileFacadeBehavior extends StateManagerViewportBehavior:
  this: StateManager =>

  def setBufferFilePath(bufferId: BufferId, filePath: String): IO[Unit] =
    stateRef.update { state =>
      state.buffers.get(bufferId) match
        case Some(buffer) =>
          state.copy(buffers = state.buffers + (bufferId -> buffer.copy(filePath = Some(Path.of(filePath)))))
        case None =>
          state
    }

  def openFile(filePath: Path): IO[Unit] =
    directLoadFileEffect(filePath)

  def saveBuffer(bufferId: BufferId): IO[Unit] =
    saveBufferEffect(bufferId)

  def saveBufferAs(bufferId: BufferId, filePath: String): IO[Unit] =
    saveBufferAsEffect(bufferId, Path.of(filePath))

  def markBufferSaved(bufferId: BufferId): IO[Unit] =
    stateRef.update { state =>
      state.buffers.get(bufferId) match
        case Some(buffer) =>
          state.copy(buffers = state.buffers + (bufferId -> buffer.copy(isDirty = false)))
        case None =>
          state
    }

  def checkUnsavedChanges(bufferId: Option[BufferId] = None): IO[Boolean] =
    stateRef.get.map { state =>
      bufferId match
        case Some(id) => state.buffers.get(id).exists(_.hasUnsavedChanges)
        case None     => state.buffers.values.exists(_.hasUnsavedChanges)
    }

  def forceCloseBuffer(bufferId: BufferId): IO[Unit] =
    closeBuffer(bufferId)

  def getRecentFiles: IO[List[java.nio.file.Path]] =
    stateRef.get.map(_.recentFiles)
