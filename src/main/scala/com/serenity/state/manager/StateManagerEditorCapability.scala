package com.serenity.state.manager

import java.nio.file.Path

import scala.util.Random

import cats.effect.IO
import com.serenity.animation.CharacterKey
import com.serenity.config.VisualFlairLevel
import com.serenity.rope.Rope
import com.serenity.state.core.EditorState
import com.serenity.state.models.*
import com.serenity.ui.layout.*

final private[manager] class StateManagerEditorCapability(
    stateRef: cats.effect.Ref[IO, AppState],
    lspQueue: LspEffectQueue,
    bufferAnimationsRef: cats.effect.Ref[IO, Map[BufferId, com.serenity.animation.AnimationState]],
    animations: AnimationChoreography,
    // Seeds the companion sprite's pseudo-random idle-to-action rolls (see `CompanionSpriteState`'s transition
    // policy). A single mutable source threaded through every tick, same as a real hardware RNG would be -- the pure
    // transition logic itself never touches unseeded randomness directly, only what this IO-boundary constructor
    // passes it. Tests construct this class with a seeded `Random` for a deterministic trace.
    companionSpriteRandom: Random = new Random()
)(using balance: com.serenity.rope.Balance):

  def getCurrentState: IO[AppState] = stateRef.get

  def getBufferAnimations: IO[Map[BufferId, com.serenity.animation.AnimationState]] = bufferAnimationsRef.get

  def getCurrentFocus: IO[Focus] = stateRef.get.map(_.persisted.focus)

  def switchFocus(newFocus: Focus): IO[Unit] =
    stateRef.update(state => state.copy(persisted = state.persisted.copy(focus = newFocus)))

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
      hasBufferAnimations = state.persisted.buffers.keys.exists(id =>
        bufferAnimations.get(id).exists(_.hasActiveAnimations)
      )
      hasThemeTransition   = state.runtime.themeTransition.isDefined
      hasSurfaceAnimations = state.runtime.surfaceAnimations.nonEmpty
      hasWindowSitter      = state.runtime.windowSitter.isActive
      flairLevel           = state.persisted.config.visualFlairLevel
      hasCompanionSprite   = state.persisted.config.companionSpriteConfig.enabled && flairLevel != VisualFlairLevel.Off
      stillActive <-
        if !hasBufferAnimations && !hasThemeTransition && !hasSurfaceAnimations && !hasWindowSitter &&
          !hasCompanionSprite
        then IO.pure(false)
        else
          val updatedTransition = state.runtime.themeTransition.map(_.advance).filterNot(_.isComplete)
          val advancedCompanionSprite =
            if hasCompanionSprite then
              state.runtime.companionSprite.tick(companionSpriteRandom, reducedRate = flairLevel == VisualFlairLevel.Reduced)
            else state.runtime.companionSprite
          val stateWithAdvancedBuffers = state.copy(
            runtime = state.runtime.copy(
              themeTransition = updatedTransition,
              windowSitter = state.runtime.windowSitter.advance,
              companionSprite = advancedCompanionSprite
            )
          )
          val newState = animations.advanceSurfaceAnimations(stateWithAdvancedBuffers)
          for
            updatedBufferAnimations <- bufferAnimationsRef.updateAndGet(_.map {
              case (id, animations) =>
                val advanced = newState.persisted.buffers.get(id) match
                  case Some(buffer) => animations.advanceAllAnimations(isWithinViewport(buffer.viewport))
                  case None         => animations
                id -> advanced
            })
            _ <- stateRef.set(newState)
          yield newState.persisted.buffers.keys
            .exists(id => updatedBufferAnimations.get(id).exists(_.hasActiveAnimations)) ||
            newState.runtime.themeTransition.isDefined ||
            newState.runtime.surfaceAnimations.nonEmpty ||
            newState.runtime.windowSitter.isActive ||
            hasCompanionSprite
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
      buffer = activePane.flatMap(pane => pane.bufferId.flatMap(state.persisted.buffers.get))
    yield buffer

  def getActivePane: IO[Option[EditorPane]] =
    stateRef.get.map(state => state.persisted.layout.activeEditorPaneId.flatMap(state.persisted.layout.editorPanes.get))

  def createBuffer(content: String, filePath: Option[Path] = None): IO[BufferId] =
    stateRef.modify { state =>
      val bufferId = state.runtime.nextBufferId
      val buffer =
        if content.isEmpty && filePath.isEmpty then Buffer.newEmpty(bufferId)(using balance)
        else
          val fresh = Buffer.fromString(bufferId, content)(using balance)
          fresh.copy(document = fresh.document.copy(filePath = filePath))
      val newState = state.copy(
        persisted = state.persisted.copy(
          buffers = state.persisted.buffers + (bufferId -> buffer),
          bufferOrder = state.persisted.bufferOrder :+ bufferId
        ),
        runtime = state.runtime.copy(nextBufferId = BufferId(bufferId.value + 1))
      )
      (newState, bufferId)
    }

  def createNewEmptyBuffer(): IO[BufferId] =
    stateRef.modify(state => EditorState.createNewEmptyBuffer(state)(using balance))

  def updateBuffer(bufferId: BufferId, content: String): IO[Unit] =
    stateRef
      .modify { state =>
        state.persisted.buffers.get(bufferId) match
          case Some(buffer) =>
            val updatedBuffer = buffer.copy(
              document = buffer.document.copy(
                content = Rope(content)(using balance),
                isDirty = true,
                isNewEmpty = false
              )
            )
            val lspTarget =
              if buffer.document.content.collect() == content then None
              else
                for
                  path       <- updatedBuffer.document.filePath
                  languageId <- updatedBuffer.document.language
                yield (path.toUri.toString, languageId, content)
            (
              state.copy(persisted =
                state.persisted.copy(buffers = state.persisted.buffers + (bufferId -> updatedBuffer))
              ),
              lspTarget
            )
          case None => (state, None)
      }
      .flatMap(_.fold(IO.unit) {
        case (uri, languageId, text) =>
          lspQueue.enqueueDocumentChange(uri, languageId, text)
      })

  def createPane(bufferId: Option[BufferId] = None): IO[PaneId] =
    stateRef.modify { state =>
      insertPane(
        state,
        state.persisted.layout.orderedPaneIds.lastOption,
        bufferId,
        SplitAxis.Horizontal
      )
    }

  def switchToPane(paneId: PaneId): IO[Unit] =
    stateRef.update { state =>
      if state.persisted.layout.editorPanes.contains(paneId) then
        state.copy(
          persisted = state.persisted.copy(
            layout = state.persisted.layout.copy(activeEditorPaneId = Some(paneId)),
            focus = Focus.EditorPane(paneId)
          )
        )
      else state
    }

  def getTabOrder(): IO[List[PaneId]] =
    stateRef.get.map(_.persisted.layout.orderedPaneIds)

  private def insertPane(
    state: AppState,
    requestedAfter: Option[PaneId],
    bufferId: Option[BufferId],
    splitAxis: SplitAxis
  ): (AppState, PaneId) =
    val paneId = state.runtime.nextPaneId
    val pane = bufferId match
      case Some(id) => EditorPane.withBuffer(paneId, id)
      case None     => EditorPane.empty(paneId)
    val targetPaneId =
      requestedAfter
        .filter(state.persisted.layout.editorPanes.contains)
        .orElse(state.persisted.layout.orderedPaneIds.lastOption)

    val updatedTree =
      targetPaneId match
        case Some(target) =>
          state.persisted.layout.effectiveWorkspaceTree.flatMap(
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
          persisted = state.persisted.copy(
            layout = state.persisted.layout.copy(
              editorPanes = state.persisted.layout.editorPanes.updated(paneId, pane),
              activeEditorPaneId = Some(paneId),
              paneOrder = tree.paneIds,
              workspaceTree = Some(tree)
            ),
            focus = Focus.EditorPane(paneId)
          ),
          runtime = state.runtime.copy(nextPaneId = PaneId(paneId.value + 1))
        )
        (updatedState, paneId)
      case None =>
        (state, paneId)
