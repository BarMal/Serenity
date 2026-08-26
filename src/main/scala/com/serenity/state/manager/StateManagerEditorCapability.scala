package com.serenity.state.manager

import java.nio.file.Path

import cats.effect.IO
import com.serenity.animation.CharacterKey
import com.serenity.keystroke.events.Direction
import com.serenity.lsp.LspEffect
import com.serenity.rope.Rope
import com.serenity.state.core.EditorState
import com.serenity.state.models.*
import com.serenity.ui.layout.*

final private[manager] class StateManagerEditorCapability(
    dependencies: EditorCapabilityPort
)(using balance: com.serenity.rope.Balance):

  import dependencies.*
  def getCurrentState: IO[AppState] = stateRef.get

  def getBufferAnimations: IO[Map[BufferId, com.serenity.animation.AnimationState]] = bufferAnimationsRef.get

  def getCurrentFocus: IO[Focus] = stateRef.get.map(_.focus)

  def switchFocus(newFocus: Focus): IO[Unit] =
    stateRef.update(_.copy(focus = newFocus))

  def updateState(update: AppState => AppState): IO[Unit] =
    stateRef.update(update)

  def updateBufferAnimations(
    update: Map[BufferId, com.serenity.animation.AnimationState] => Map[BufferId, com.serenity.animation.AnimationState]
  ): IO[Unit] =
    bufferAnimationsRef.update(update)

  def advanceAnimationFrames(): IO[Unit] =
    bufferAnimationsRef.update(
      _.view
        .mapValues { animations =>
          val advanced = animations.advanceAnimations()
          if advanced eq animations then animations else advanced
        }
        .toMap
    )

  def advanceAnimationsOnTick(): IO[Boolean] =
    for
      state            <- stateRef.get
      bufferAnimations <- bufferAnimationsRef.get
      hasBufferAnimations  = state.buffers.keys.exists(id => bufferAnimations.get(id).exists(_.hasActiveAnimations))
      hasThemeTransition   = state.themeTransition.isDefined
      hasSurfaceAnimations = state.surfaceAnimations.nonEmpty
      hasWindowSitter      = state.windowSitter.isActive
      stillActive <-
        if !hasBufferAnimations && !hasThemeTransition && !hasSurfaceAnimations && !hasWindowSitter then IO.pure(false)
        else
          val updatedTransition = state.themeTransition.map(_.advance).filterNot(_.isComplete)
          val stateWithAdvancedBuffers = state.copy(
            themeTransition = updatedTransition,
            windowSitter = state.windowSitter.advance
          )
          val newState = advanceSurfaceAnimations(stateWithAdvancedBuffers)
          for
            updatedBufferAnimations <- bufferAnimationsRef.updateAndGet(_.map {
              case (id, animations) =>
                val advanced = newState.buffers.get(id) match
                  case Some(buffer) => animations.advanceAllAnimations(isWithinViewport(buffer.viewport))
                  case None         => animations
                id -> advanced
            })
            _ <- stateRef.set(newState)
          yield newState.buffers.keys.exists(id => updatedBufferAnimations.get(id).exists(_.hasActiveAnimations)) ||
            newState.themeTransition.isDefined ||
            newState.surfaceAnimations.nonEmpty ||
            newState.windowSitter.isActive
    yield stillActive

  /** A cell outside the buffer's currently visible viewport isn't rendered, so there's no need to pay its
    * interpolation/allocation cost on every tick -- it simply resumes advancing once scrolled back into view.
    */
  private def isWithinViewport(viewport: Viewport)(key: CharacterKey): Boolean =
    key.line >= viewport.topLine && key.line < viewport.topLine + viewport.visibleLines &&
      key.column >= viewport.leftColumn && key.column < viewport.leftColumn + viewport.visibleColumns

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
      val buffer =
        if content.isEmpty && filePath.isEmpty then Buffer.newEmpty(bufferId)(using balance)
        else Buffer.fromString(bufferId, content)(using balance).copy(filePath = filePath)
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
    stateRef
      .modify { state =>
        state.buffers.get(bufferId) match
          case Some(buffer) =>
            val updatedBuffer = buffer.copy(
              content = Rope(content)(using balance),
              isDirty = true,
              isNewEmpty = false
            )
            val lspTarget =
              if buffer.content.collect() == content then None
              else
                for
                  path       <- updatedBuffer.filePath
                  languageId <- updatedBuffer.language
                yield (path.toUri.toString, languageId, content)
            (state.copy(buffers = state.buffers + (bufferId -> updatedBuffer)), lspTarget)
          case None => (state, None)
      }
      .flatMap(_.fold(IO.unit) {
        case (uri, languageId, text) =>
          lspQueue.enqueueDocumentChange(uri, languageId, text)
      })

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
              lspQueue.enqueue(LspEffect.FileClosed(path.toUri.toString, languageId))
            case _ =>
              IO.unit
        case None =>
          IO.unit
      }

  def createPane(bufferId: Option[BufferId] = None): IO[PaneId] =
    stateRef.modify { state =>
      insertPane(
        state,
        state.layout.orderedPaneIds.lastOption,
        bufferId,
        SplitAxis.Horizontal
      )
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
      val updated = EditorState.removePane(state, paneId)
      if state.layout.editorPanes.size == 1 && state.layout.editorPanes.contains(paneId) then
        ensureCommandRunnerSurface(updated)
      else updated
    }

  def setBufferForPane(paneId: PaneId, bufferId: BufferId): IO[Unit] =
    stateRef.update { state =>
      state.layout.editorPanes.get(paneId) match
        case Some(pane) =>
          val updatedPane = pane.copy(bufferId = Some(bufferId))
          val stateWithBuffer = state.copy(
            layout = state.layout.copy(
              editorPanes = state.layout.editorPanes + (paneId -> updatedPane)
            )
          )
          LayoutEngine.syncViewportDimensions(
            stateWithBuffer,
            stateWithBuffer.viewportSize.getOrElse(ViewportSize(80, 24))
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
    stateRef.modify(state => insertPane(state, Some(afterPaneId), bufferId, SplitAxis.Horizontal))

  def getTabOrder(): IO[List[PaneId]] =
    stateRef.get.map(_.layout.orderedPaneIds)

  def splitPaneHorizontal(paneId: PaneId, bufferId: Option[BufferId] = None): IO[PaneId] =
    splitPane(paneId, bufferId, SplitAxis.Horizontal)

  def splitPaneVertical(paneId: PaneId, bufferId: Option[BufferId] = None): IO[PaneId] =
    splitPane(paneId, bufferId, SplitAxis.Vertical)

  def resizePaneSplit(splitId: WorkspaceNodeId, ratio: Double): IO[Unit] =
    stateRef.update { state =>
      state.layout.effectiveWorkspaceTree
        .flatMap(_.resize(splitId, ratio))
        .map(tree => state.copy(layout = state.layout.copy(workspaceTree = Some(tree), paneOrder = tree.paneIds)))
        .getOrElse(state)
    }

  def focusPaneInDirection(direction: Direction): IO[Unit] =
    stateRef.update { state =>
      val currentPaneId =
        state.focus match
          case Focus.EditorPane(paneId) => Some(paneId)
          case _                        => state.layout.activeEditorPaneId
      val viewportSize = state.viewportSize.getOrElse(ViewportSize(80, 24))
      val layout       = LayoutEngine.calculateLayoutWithUI(state, viewportSize)
      currentPaneId
        .flatMap(LayoutEngine.directionalPaneNeighbor(state, layout, _, direction))
        .map { paneId =>
          state.copy(
            layout = state.layout.copy(activeEditorPaneId = Some(paneId)),
            focus = Focus.EditorPane(paneId)
          )
        }
        .getOrElse(state)
    }

  private def splitPane(
    paneId: PaneId,
    bufferId: Option[BufferId],
    splitAxis: SplitAxis
  ): IO[PaneId] =
    stateRef.modify(state => insertPane(state, Some(paneId), bufferId, splitAxis))

  private def insertPane(
    state: AppState,
    requestedAfter: Option[PaneId],
    bufferId: Option[BufferId],
    splitAxis: SplitAxis
  ): (AppState, PaneId) =
    val paneId = state.nextPaneId
    val pane = bufferId match
      case Some(id) => EditorPane.withBuffer(paneId, id)
      case None     => EditorPane.empty(paneId)
    val targetPaneId =
      requestedAfter
        .filter(state.layout.editorPanes.contains)
        .orElse(state.layout.orderedPaneIds.lastOption)

    val updatedTree =
      targetPaneId match
        case Some(target) =>
          state.layout.effectiveWorkspaceTree.flatMap(
            _.split(
              target,
              paneId,
              splitAxis,
              WorkspaceNodeId(s"split-${target.value}-${paneId.value}"),
              WorkspaceNodeId(s"editor-${paneId.value}")
            )
          )
        case None =>
          Some(WorkspaceTree(WorkspaceNode.Leaf(WorkspaceNodeId(s"editor-${paneId.value}"), paneId)))

    updatedTree match
      case Some(tree) =>
        val updatedState = state.copy(
          layout = state.layout.copy(
            editorPanes = state.layout.editorPanes.updated(paneId, pane),
            activeEditorPaneId = Some(paneId),
            paneOrder = tree.paneIds,
            workspaceTree = Some(tree)
          ),
          focus = Focus.EditorPane(paneId),
          nextPaneId = PaneId(paneId.value + 1)
        )
        (updatedState, paneId)
      case None =>
        (state, paneId)
