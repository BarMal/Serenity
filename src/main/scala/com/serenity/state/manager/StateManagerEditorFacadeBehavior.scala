package com.serenity.state.manager

import java.nio.file.Path

import cats.effect.IO
import com.serenity.lsp.LspEffect
import com.serenity.rope.Rope
import com.serenity.state.core.EditorState
import com.serenity.state.models.*

private[manager] trait StateManagerEditorFacadeBehavior extends StateManagerEventPipelineBehavior:
  this: StateManager =>

  def getCurrentState: IO[AppState] = stateRef.get

  def getCurrentFocus: IO[Focus] = stateRef.get.map(_.focus)

  def switchFocus(newFocus: Focus): IO[Unit] =
    stateRef.update(_.copy(focus = newFocus))

  def updateState(update: AppState => AppState): IO[Unit] =
    stateRef.update(update)

  def advanceAnimationFrames(): IO[Unit] =
    for
      state <- stateRef.get
      updatedBuffers = state.buffers.view.mapValues { buffer =>
        buffer.copy(animations = buffer.animations.advanceAnimations())
      }.toMap
      _ <- stateRef.set(state.copy(buffers = updatedBuffers))
    yield ()

  def advanceAnimationsOnTick(): IO[Boolean] =
    stateRef.get.flatMap { state =>
      val hasBufferAnimations  = state.buffers.values.exists(_.animations.hasActiveAnimations)
      val hasThemeTransition   = state.themeTransition.isDefined
      val hasSurfaceAnimations = state.surfaceAnimations.nonEmpty
      if !hasBufferAnimations && !hasThemeTransition && !hasSurfaceAnimations then IO.pure(false)
      else
        val updatedBuffers = state.buffers.view.mapValues { buffer =>
          buffer.copy(animations = buffer.animations.advanceAllAnimations())
        }.toMap
        val updatedTransition        = state.themeTransition.map(_.advance).filterNot(_.isComplete)
        val stateWithAdvancedBuffers = state.copy(buffers = updatedBuffers, themeTransition = updatedTransition)
        val newState                 = advanceSurfaceAnimations(stateWithAdvancedBuffers)
        val stillActive =
          newState.buffers.values.exists(_.animations.hasActiveAnimations) ||
            newState.themeTransition.isDefined ||
            newState.surfaceAnimations.nonEmpty
        stateRef.set(newState).as(stillActive)
    }

  def getActiveBuffer: IO[Option[Buffer]] =
    for
      state      <- stateRef.get
      activePane <- getActivePane
      buffer = activePane.flatMap(pane => pane.bufferId.flatMap(state.buffers.get))
    yield buffer

  def getActivePane: IO[Option[EditorPane]] =
    stateRef.get.map(state => state.layout.activeEditorPaneId.flatMap(state.layout.editorPanes.get))

  def createBuffer(content: String, filePath: Option[Path] = None): IO[BufferId] =
    stateRef.modify { state =>
      val bufferId = state.nextBufferId
      val buffer   = Buffer.fromString(bufferId, content)(using balance).copy(filePath = filePath)
      val newState = state.copy(
        buffers = state.buffers + (bufferId -> buffer),
        bufferOrder = state.bufferOrder :+ bufferId,
        nextBufferId = BufferId(bufferId.value + 1)
      )
      (newState, bufferId)
    }

  def createNewEmptyBuffer(): IO[BufferId] =
    stateRef.modify(state => EditorState.createNewEmptyBuffer(state)(using balance))

  def updateBuffer(bufferId: BufferId, content: String): IO[Unit] =
    stateRef.update { state =>
      state.buffers.get(bufferId) match
        case Some(buffer) =>
          val updatedBuffer = buffer.copy(
            content = Rope(content)(using balance),
            isDirty = true
          )
          state.copy(buffers = state.buffers + (bufferId -> updatedBuffer))
        case None => state
    }

  def closeBuffer(bufferId: BufferId): IO[Unit] =
    stateRef
      .modify { state =>
        val closingBuffer = state.buffers.get(bufferId)
        val newState      = EditorState.removeBuffer(state, bufferId)
        (newState, closingBuffer)
      }
      .flatMap {
        case Some(buffer) =>
          (buffer.filePath, buffer.language) match
            case (Some(path), Some(languageId)) =>
              lspQueue.offer(LspEffect.FileClosed(path.toUri.toString, languageId))
            case _ =>
              IO.unit
        case None =>
          IO.unit
      }

  def createPane(bufferId: Option[BufferId] = None): IO[PaneId] =
    stateRef.modify { state =>
      val paneId = state.nextPaneId
      val pane = bufferId match
        case Some(id) => EditorPane.withBuffer(paneId, id)
        case None     => EditorPane.empty(paneId)

      val newState = state.copy(
        layout = state.layout.copy(
          editorPanes = state.layout.editorPanes + (paneId -> pane),
          activeEditorPaneId = Some(paneId),
          paneOrder = state.layout.paneOrder :+ paneId
        ),
        focus = Focus.EditorPane(paneId),
        nextPaneId = PaneId(paneId.value + 1)
      )
      (newState, paneId)
    }

  def switchToPane(paneId: PaneId): IO[Unit] =
    stateRef.update { state =>
      if state.layout.editorPanes.contains(paneId) then
        state.copy(
          layout = state.layout.copy(activeEditorPaneId = Some(paneId)),
          focus = Focus.EditorPane(paneId)
        )
      else state
    }

  def closePane(paneId: PaneId): IO[Unit] =
    stateRef.update { state =>
      val updatedState = EditorState.removePane(state, paneId)
      if updatedState.layout.activeEditorPaneId.isDefined then updatedState
      else ensureCommandRunnerSurface(updatedState)
    }

  def setBufferForPane(paneId: PaneId, bufferId: BufferId): IO[Unit] =
    stateRef.update { state =>
      state.layout.editorPanes.get(paneId) match
        case Some(pane) =>
          val updatedPane = pane.copy(bufferId = Some(bufferId))
          state.copy(
            layout = state.layout.copy(
              editorPanes = state.layout.editorPanes + (paneId -> updatedPane)
            )
          )
        case None => state
    }

  def setCursorPosition(paneId: PaneId, line: Int, column: Int): IO[Unit] =
    stateRef.update { state =>
      state.layout.editorPanes.get(paneId) match
        case Some(pane) =>
          pane.bufferId.flatMap(state.buffers.get) match
            case Some(buffer) =>
              val newCursor = CursorPosition(line, column)
              val updatedBuffer = buffer.copy(
                cursors = List(newCursor),
                preferredColumn = Some(column),
                preferredXPx = None,
                multiCursorVerticalStates = Nil
              )
              state.copy(buffers = state.buffers + (buffer.id -> updatedBuffer))
            case None => state
        case None => state
    }

  def setViewport(paneId: PaneId, viewport: Viewport): IO[Unit] =
    stateRef.update { state =>
      state.layout.editorPanes.get(paneId) match
        case Some(pane) =>
          pane.bufferId.flatMap(state.buffers.get) match
            case Some(buffer) =>
              val updatedBuffer = buffer.copy(viewport = viewport)
              state.copy(buffers = state.buffers + (buffer.id -> updatedBuffer))
            case None => state
        case None => state
    }

  def setPaneProperties(paneId: PaneId, update: EditorPane => EditorPane): IO[Unit] =
    stateRef.update { state =>
      state.layout.editorPanes.get(paneId) match
        case Some(pane) =>
          state.copy(
            layout = state.layout.copy(
              editorPanes = state.layout.editorPanes + (paneId -> update(pane))
            )
          )
        case None => state
    }

  def createPaneAfter(afterPaneId: PaneId, bufferId: Option[BufferId] = None): IO[PaneId] =
    stateRef.modify { state =>
      val paneId = state.nextPaneId
      val pane = bufferId match
        case Some(id) => EditorPane.withBuffer(paneId, id)
        case None     => EditorPane.empty(paneId)

      val insertIdx = state.layout.paneOrder.indexOf(afterPaneId) match
        case -1  => state.layout.paneOrder.size
        case idx => idx + 1

      val newState = state.copy(
        layout = state.layout.copy(
          editorPanes = state.layout.editorPanes + (paneId -> pane),
          activeEditorPaneId = Some(paneId),
          paneOrder = state.layout.paneOrder.patch(insertIdx, List(paneId), 0)
        ),
        focus = Focus.EditorPane(paneId),
        nextPaneId = PaneId(paneId.value + 1)
      )
      (newState, paneId)
    }

  def getTabOrder(): IO[List[PaneId]] =
    stateRef.get.map(_.layout.orderedPaneIds)

  def splitPaneHorizontal(paneId: PaneId, bufferId: Option[BufferId] = None): IO[PaneId] =
    createPaneAfter(paneId, bufferId)
