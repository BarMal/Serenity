package com.serenity.state.manager

import java.nio.file.{Files, Path}

import cats.effect.IO
import com.serenity.state.models.*
import com.serenity.state.reducers.{ModalStateReducer, PanelStateReducer, PeekStateReducer}
import com.serenity.ui.layout.*

private[manager] trait StateManagerSurfaceFacadeBehavior extends StateManagerEditorFacadeBehavior:
  this: StateManager =>

  def showPeek(content: PeekContent, at: CursorPosition): IO[Unit] =
    stateRef.get.flatMap(state => validateAndUpdateState(PeekStateReducer.show(content, at, state).state, state))

  def dismissPeek(): IO[Unit] =
    stateRef.get.flatMap(state => validateAndUpdateState(PeekStateReducer.dismiss(state).state, state))

  def peekToPin(position: PanelPosition): IO[Unit] =
    stateRef.get.flatMap { state =>
      validateAndUpdateState(PanelStateReducer.pinPeekOverlay(position, state).state, state)
        .flatMap(_ => applyAnimationHooks(state))
    }

  def pinPanel(content: PanelContent, position: PanelPosition, size: Int): IO[Unit] =
    stateRef.get.flatMap { state =>
      validateAndUpdateState(PanelStateReducer.pin(content, position, size, state).state, state)
        .flatMap(_ => applyAnimationHooks(state))
    }

  def unpinPanel(position: PanelPosition): IO[Unit] =
    stateRef.get.flatMap { state =>
      validateAndUpdateState(PanelStateReducer.unpin(position, state).state, state)
        .flatMap(_ => applyAnimationHooks(state))
    }

  def expandPinnedPanel(position: PanelPosition): IO[Unit] =
    stateRef.get.flatMap { state =>
      validateAndUpdateState(PanelStateReducer.expand(position, state).state, state)
        .flatMap(_ => applyAnimationHooks(state))
    }

  def collapseExpandedPanel(): IO[Unit] =
    stateRef.get.flatMap { state =>
      validateAndUpdateState(PanelStateReducer.collapseExpandedPanel(state).state, state)
        .flatMap(_ => applyAnimationHooks(state))
    }

  def showModal(modal: Modal): IO[Unit] =
    stateRef.get.flatMap(state => validateAndUpdateState(ModalStateReducer.show(modal, state).state, state))

  def dismissModal(): IO[Unit] =
    stateRef.get.flatMap(state => validateAndUpdateState(ModalStateReducer.dismiss(state).state, state))

  def switchToPinnedPanel(position: PanelPosition): IO[Unit] =
    stateRef.get.flatMap(state => validateAndUpdateState(PanelStateReducer.focus(position, state).state, state))

  def loadDirectoryTree(path: String, files: List[String]): IO[Unit] =
    val rootPath = Path.of(path)
    val entries = files.map { name =>
      val isDir = name.endsWith("/")
      DirEntry(rootPath.resolve(name), name, isDirectory = isDir)
    }
    val tree    = DirectoryTreeData(rootPath, entries = Map(rootPath -> entries))
    val content = PanelContent.DirectoryTree(tree, selectedPath = None)
    stateRef.get.flatMap { state =>
      val maybeExistingExplorer = state.pinnedSurfaces.reverse.find { surface =>
        surface.content match
          case SurfaceContent.DirectoryTree(_, _) =>
            surface.presentation match
              case SurfacePresentation.Pinned(PanelPosition.Left, _) => true
              case _                                                 => false
          case _ =>
            false
      }
      val updated =
        maybeExistingExplorer match
          case Some(surface) =>
            val nextSurface = surface.copy(
              content = SurfaceContent.DirectoryTree(tree, selectedPath = None),
              presentation = SurfacePresentation.Pinned(PanelPosition.Left, 30)
            )
            state.copy(uiSurfaces = state.uiSurfaces.filterNot(_.id == surface.id) :+ nextSurface)
          case None =>
            PanelStateReducer.pin(content, PanelPosition.Left, 30, state).state
      validateAndUpdateState(updated, state).flatMap(_ => applyAnimationHooks(state))
    }

  def selectFileInExplorer(filePath: String): IO[Unit] =
    val targetPath = Path.of(filePath)
    stateRef.get.flatMap { state =>
      val updated = state.pinnedSurfaces.reverse
        .find { surface =>
          surface.presentation match
            case SurfacePresentation.Pinned(PanelPosition.Left, _) => true
            case _                                                 => false
        }
        .flatMap { surface =>
          surface.content match
            case SurfaceContent.DirectoryTree(tree, _) =>
              val newContent = SurfaceContent.DirectoryTree(tree, Some(targetPath))
              val newSurface = surface.copy(content = newContent)
              Some(state.copy(uiSurfaces = state.uiSurfaces.filterNot(_.id == surface.id) :+ newSurface))
            case _ => None
        }
        .getOrElse(state)
      validateAndUpdateState(updated, state)
    }

  def resizePinnedPanel(position: PanelPosition, newSize: Int): IO[Unit] =
    stateRef.get.flatMap(state =>
      validateAndUpdateState(PanelStateReducer.resize(position, newSize, state).state, state)
    )

  def dragFileToDirectory(sourceFile: String, targetDir: String): IO[Unit] =
    val src    = Path.of(sourceFile)
    val dst    = Path.of(targetDir).resolve(src.getFileName)
    val srcDir = src.getParent
    IO.blocking(Files.move(src, dst))
      .flatMap { _ =>
        stateRef.update { state =>
          state.pinnedSurfaces.foldLeft(state) { (currentState, surface) =>
            surface.content match
              case SurfaceContent.DirectoryTree(tree, selectedPath) if tree.entries.contains(srcDir) =>
                val updatedSurface = surface.copy(
                  content = SurfaceContent.DirectoryTree(
                    tree.copy(entries = tree.entries.updated(srcDir, tree.entries(srcDir).filterNot(_.path == src))),
                    selectedPath
                  )
                )
                currentState.copy(
                  uiSurfaces = currentState.uiSurfaces.filterNot(_.id == surface.id) :+ updatedSurface
                )
              case _ =>
                currentState
          }
        }
      }
      .handleErrorWith(ex => logger.error(ex)(s"[FILE] Failed to move $sourceFile to $targetDir"))
