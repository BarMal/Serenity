package com.serenity.state.manager

import java.nio.file.{Files, Path}

import cats.effect.IO
import com.serenity.state.models.*
import com.serenity.state.reducers.{ModalStateReducer, PanelStateReducer, PeekStateReducer}
import com.serenity.ui.layout.*

final private[manager] class StateManagerSurfaceCapability(
    stateRef: cats.effect.Ref[IO, AppState],
    logger: org.typelevel.log4cats.Logger[IO],
    operations: StateManagerOperationBoundary
):

  private def validateAndUpdateState(newState: AppState, fallbackState: AppState): IO[Unit] =
    operations.validateAndUpdateState(newState, fallbackState)

  private def applyAnimationHooks(previousState: AppState): IO[Unit] =
    operations.enqueueAnimationHooks(previousState)

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

  // A target that resolves to no panel (a `ByPosition` side holding nothing pinned, or an `ById` surface that isn't
  // a pinned panel) is a deliberate no-op: the reducer returns the unchanged state we handed it, so callers asking
  // to unpin/expand/focus/resize a panel that isn't there just see nothing happen, the same explicit policy as
  // "target doesn't apply, ignore the request" used elsewhere in this façade (e.g. `checkUnsavedChanges` and
  // `saveBufferAs` no-op when the bufferId doesn't resolve to a buffer).
  /** Updates the existing pinned Terminal panel's content in place rather than pinning a new surface every call, so a
    * project task's periodic output refresh (`StateManagerEffectHandlers.runProjectTask`) doesn't leave behind a fresh
    * panel -- and the user's own workspace-tree focus on it -- every 100ms for the task's whole lifetime (issue #1294).
    */
  def pinOrUpdateTerminalPanel(text: String, position: PanelPosition, size: Int): IO[Unit] =
    val content = PanelContent.Terminal(text, text.length)
    stateRef.get.flatMap { state =>
      val existing = state.pinnedSurfaces.reverse.find { surface =>
        surface.content match
          case SurfaceContent.Terminal(_, _) =>
            surface.presentation match
              case SurfacePresentation.Pinned(_, _) => true
              case _                                => false
          case _ => false
      }
      val updated = existing match
        case Some(surface @ UiSurface(_, _, SurfacePresentation.Pinned(_, _), _)) =>
          val nextSurface = surface.copy(content = SurfaceContent.Terminal(text, text.length))
          state.copy(runtime =
            state.runtime.copy(uiSurfaces = state.runtime.uiSurfaces.filterNot(_.id == surface.id) :+ nextSurface)
          )
        case _ =>
          PanelStateReducer.pin(content, position, size, state).state
      validateAndUpdateState(updated, state).flatMap(_ => applyAnimationHooks(state))
    }

  def unpinPanel(target: PanelTarget): IO[Unit] =
    stateRef.get.flatMap { state =>
      val result = target match
        case PanelTarget.ById(surfaceId)      => PanelStateReducer.unpin(surfaceId, state)
        case PanelTarget.ByPosition(position) => PanelStateReducer.unpin(position, state)
      validateAndUpdateState(result.state, state)
        .flatMap(_ => applyAnimationHooks(state))
    }

  def movePinnedPanel(surfaceId: SurfaceId, position: PanelPosition): IO[Unit] =
    stateRef.get.flatMap { state =>
      validateAndUpdateState(PanelStateReducer.move(surfaceId, position, state).state, state)
        .flatMap(_ => applyAnimationHooks(state))
    }

  def expandPinnedPanel(target: PanelTarget): IO[Unit] =
    stateRef.get.flatMap { state =>
      val result = target match
        case PanelTarget.ById(surfaceId)      => PanelStateReducer.expand(surfaceId, state)
        case PanelTarget.ByPosition(position) => PanelStateReducer.expand(position, state)
      validateAndUpdateState(result.state, state)
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

  def switchToPinnedPanel(target: PanelTarget): IO[Unit] =
    stateRef.get.flatMap { state =>
      val result = target match
        case PanelTarget.ById(surfaceId)      => PanelStateReducer.focus(surfaceId, state)
        case PanelTarget.ByPosition(position) => PanelStateReducer.focus(position, state)
      validateAndUpdateState(result.state, state)
    }

  def loadDirectoryTree(rootPath: Path, files: List[String]): IO[Unit] =
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
              case SurfacePresentation.Pinned(_, _) => true
              case _                                => false
          case _ =>
            false
      }
      val updated =
        maybeExistingExplorer match
          case Some(surface @ UiSurface(_, _, SurfacePresentation.Pinned(position, size), _)) =>
            val nextSurface = surface.copy(
              content = SurfaceContent.DirectoryTree(tree, selectedPath = None),
              presentation = SurfacePresentation.Pinned(position, size)
            )
            state.copy(runtime =
              state.runtime.copy(uiSurfaces = state.runtime.uiSurfaces.filterNot(_.id == surface.id) :+ nextSurface)
            )
          case Some(_) =>
            state
          case None =>
            PanelStateReducer.pin(content, PanelPosition.Left, 30, state).state
      validateAndUpdateState(updated, state).flatMap(_ => applyAnimationHooks(state))
    }

  def selectFileInExplorer(targetPath: Path): IO[Unit] =
    stateRef.get.flatMap { state =>
      val updated = state.pinnedSurfaces.reverse
        .find { surface =>
          surface.content match
            case SurfaceContent.DirectoryTree(_, _) =>
              surface.presentation match
                case SurfacePresentation.Pinned(_, _) => true
                case _                                => false
            case _ =>
              false
        }
        .flatMap { surface =>
          surface.content match
            case SurfaceContent.DirectoryTree(tree, _) =>
              val newContent = SurfaceContent.DirectoryTree(tree, Some(targetPath))
              val newSurface = surface.copy(content = newContent)
              Some(
                state.copy(runtime =
                  state.runtime.copy(uiSurfaces = state.runtime.uiSurfaces.filterNot(_.id == surface.id) :+ newSurface)
                )
              )
            case _ => None
        }
        .getOrElse(state)
      validateAndUpdateState(updated, state)
    }

  def resizePinnedPanel(target: PanelTarget, newSize: Int): IO[Unit] =
    stateRef.get.flatMap { state =>
      val result = target match
        case PanelTarget.ById(surfaceId)      => PanelStateReducer.resize(surfaceId, newSize, state)
        case PanelTarget.ByPosition(position) => PanelStateReducer.resize(position, newSize, state)
      validateAndUpdateState(result.state, state)
    }

  def dragFileToDirectory(src: Path, targetDir: Path): IO[Unit] =
    val dst    = targetDir.resolve(src.getFileName)
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
                  runtime = currentState.runtime.copy(
                    uiSurfaces = currentState.runtime.uiSurfaces.filterNot(_.id == surface.id) :+ updatedSurface
                  )
                )
              case _ =>
                currentState
          }
        }
      }
      .handleErrorWith(ex => logger.error(ex)(s"[FILE] Failed to move $src to $targetDir"))
