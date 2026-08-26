package com.serenity.state.manager

import java.nio.file.Path

import cats.effect.{IO, Ref}
import com.serenity.state.models.*

final private[manager] class StateManagerFileFacade(
    stateRef: Ref[IO, AppState],
    loadFile: Path => IO[Unit],
    save: BufferId => IO[Unit],
    saveAs: (BufferId, Path) => IO[Unit],
    close: BufferId => IO[Unit]
):

  def setBufferFilePath(bufferId: BufferId, filePath: String): IO[Unit] =
    stateRef.update { state =>
      state.buffers.get(bufferId) match
        case Some(buffer) =>
          state.copy(buffers =
            state.buffers + (bufferId -> buffer.copy(document = buffer.document.copy(filePath = Some(Path.of(filePath)))))
          )
        case None =>
          state
    }

  def openFile(filePath: Path): IO[Unit] =
    loadFile(filePath)

  def saveBuffer(bufferId: BufferId): IO[Unit] =
    save(bufferId)

  def saveBufferAs(bufferId: BufferId, filePath: Path): IO[Unit] =
    saveAs(bufferId, filePath)

  def markBufferSaved(bufferId: BufferId): IO[Unit] =
    stateRef.update { state =>
      state.buffers.get(bufferId) match
        case Some(buffer) =>
          state.copy(buffers =
            state.buffers + (bufferId -> buffer.copy(document = buffer.document.copy(isDirty = false)))
          )
        case None =>
          state
    }

  def checkUnsavedChanges(bufferId: Option[BufferId]): IO[Boolean] =
    stateRef.get.map { state =>
      bufferId match
        case Some(id) => state.buffers.get(id).exists(_.hasUnsavedChanges)
        case None     => state.buffers.values.exists(_.hasUnsavedChanges)
    }

  def forceCloseBuffer(bufferId: BufferId): IO[Unit] =
    close(bufferId)

  def getRecentFiles: IO[List[Path]] =
    stateRef.get.map(_.recentFiles)

final private[manager] class StateManagerFileCapability(
    stateRef: Ref[IO, AppState],
    dependencies: FileCapabilityPort
)(using balance: com.serenity.rope.Balance):

  import dependencies.*

  private lazy val fileFacade = new StateManagerFileFacade(
    stateRef,
    directLoadFileEffect,
    saveBufferEffect,
    saveBufferAsEffect,
    closeBuffer
  )

  def setBufferFilePath(bufferId: BufferId, filePath: String): IO[Unit] =
    fileFacade.setBufferFilePath(bufferId, filePath)

  def openFile(filePath: Path): IO[Unit] =
    fileFacade.openFile(filePath)

  def saveBuffer(bufferId: BufferId): IO[Unit] =
    fileFacade.saveBuffer(bufferId)

  def saveBufferAs(bufferId: BufferId, filePath: String): IO[Unit] =
    fileFacade.saveBufferAs(bufferId, Path.of(filePath))

  def markBufferSaved(bufferId: BufferId): IO[Unit] =
    fileFacade.markBufferSaved(bufferId)

  def checkUnsavedChanges(bufferId: Option[BufferId] = None): IO[Boolean] =
    fileFacade.checkUnsavedChanges(bufferId)

  def forceCloseBuffer(bufferId: BufferId): IO[Unit] =
    fileFacade.forceCloseBuffer(bufferId)

  def getRecentFiles: IO[List[java.nio.file.Path]] =
    fileFacade.getRecentFiles
