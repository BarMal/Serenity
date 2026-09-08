package com.serenity.state.manager

import java.nio.file.Path

import cats.effect.{IO, Ref}
import com.serenity.state.models.*

final private[manager] class StateManagerFileFacade(
    stateRef: Ref[IO, AppState],
    loadFile: Path => IO[Unit],
    save: BufferId => IO[Unit],
    saveAs: (BufferId, Path) => IO[Unit]
):

  def setBufferFilePath(bufferId: BufferId, filePath: Path): IO[Unit] =
    stateRef.update { state =>
      state.persisted.buffers.get(bufferId) match
        case Some(buffer) =>
          state.copy(persisted =
            state.persisted.copy(buffers =
              state.persisted.buffers + (bufferId -> buffer.copy(document =
                buffer.document.copy(filePath = Some(filePath))
              ))
            )
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
      state.persisted.buffers.get(bufferId) match
        case Some(buffer) =>
          state.copy(persisted =
            state.persisted.copy(buffers =
              state.persisted.buffers + (bufferId -> buffer.copy(document = buffer.document.copy(isDirty = false)))
            )
          )
        case None =>
          state
    }

  def checkUnsavedChanges(bufferId: Option[BufferId]): IO[Boolean] =
    stateRef.get.map { state =>
      bufferId match
        case Some(id) => state.persisted.buffers.get(id).exists(_.hasUnsavedChanges)
        case None     => state.persisted.buffers.values.exists(_.hasUnsavedChanges)
    }

  def getRecentFiles: IO[List[Path]] =
    stateRef.get.map(_.persisted.recentFiles)

final private[manager] class StateManagerFileCapability(
    stateRef: Ref[IO, AppState],
    effects: StateManagerEffectHandlers
):

  private lazy val fileFacade = new StateManagerFileFacade(
    stateRef,
    effects.directLoadFileEffect,
    effects.saveBufferEffect,
    effects.saveBufferAsEffect
  )

  private def setBufferFilePath(bufferId: BufferId, filePath: Path): IO[Unit] =
    fileFacade.setBufferFilePath(bufferId, filePath)

  private def openFile(filePath: Path): IO[Unit] =
    fileFacade.openFile(filePath)

  private def saveBuffer(bufferId: BufferId): IO[Unit] =
    fileFacade.saveBuffer(bufferId)

  private def saveBufferAs(bufferId: BufferId, filePath: Path): IO[Unit] =
    fileFacade.saveBufferAs(bufferId, filePath)

  private def markBufferSaved(bufferId: BufferId): IO[Unit] =
    fileFacade.markBufferSaved(bufferId)

  private def checkUnsavedChanges(bufferId: Option[BufferId]): IO[Boolean] =
    fileFacade.checkUnsavedChanges(bufferId)

  private def getRecentFiles: IO[List[java.nio.file.Path]] =
    fileFacade.getRecentFiles

  val fileOpener: FileOpener = FileOpener(openFile = openFile)

  val fileService: FileService = FileService(
    setBufferFilePath = setBufferFilePath,
    saveBuffer = saveBuffer,
    saveBufferAs = saveBufferAs,
    markBufferSaved = markBufferSaved,
    checkUnsavedChanges = checkUnsavedChanges,
    getRecentFiles = getRecentFiles
  )
