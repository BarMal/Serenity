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
    operations: StateManagerOperationBoundary,
    // Seeds the companion sprite's pseudo-random idle-to-action rolls (see `CompanionSpriteState`'s transition
    // policy). A single mutable source threaded through every tick, same as a real hardware RNG would be -- the pure
    // transition logic itself never touches unseeded randomness directly, only what this IO-boundary constructor
    // passes it. Tests construct this class with a seeded `Random` for a deterministic trace.
    companionSpriteRandom: Random = new Random()
)(using balance: com.serenity.rope.Balance):

  def getCurrentState: IO[AppState] = stateRef.get

  private def validateAndUpdateState(newState: AppState, fallbackState: AppState): IO[Unit] =
    operations.validateAndUpdateState(newState, fallbackState)

  def getBufferAnimations: IO[Map[BufferId, com.serenity.animation.AnimationState]] = bufferAnimationsRef.get

  val focusManager: FocusManager = FocusManager(switchFocus = switchFocus)

  private def switchFocus(newFocus: Focus): IO[Unit] =
    stateRef.update(state => state.copy(persisted = state.persisted.copy(focus = newFocus)))

  def updateState(update: AppState => AppState): IO[Unit] =
    stateRef.update(update)

  def updateBufferAnimations(
    update: Map[BufferId, com.serenity.animation.AnimationState] => Map[BufferId, com.serenity.animation.AnimationState]
  ): IO[Unit] =
    bufferAnimationsRef.update(update)

  val animationTicker: AnimationTicker = AnimationTicker(advanceAnimationsOnTick = advanceAnimationsOnTick())

  private def advanceAnimationsOnTick(): IO[Boolean] =
    for
      state            <- stateRef.get
      bufferAnimations <- bufferAnimationsRef.get
      hasBufferAnimations = state.persisted.buffers.keys.exists(id =>
        bufferAnimations.get(id).exists(_.hasActiveAnimations)
      )
      hasThemeTransition   = state.runtime.themeTransition.isDefined
      hasSurfaceAnimations = state.runtime.surfaceAnimations.nonEmpty
      hasColumnTransitions = state.runtime.columnTransitions.nonEmpty
      hasTypingActivity    = state.runtime.typingActivity.isActive
      flairLevel           = state.persisted.config.visualFlairLevel
      hasCompanionSprite   = state.persisted.config.companionSpriteConfig.enabled && flairLevel != VisualFlairLevel.Off
      stillActive <-
        if !hasBufferAnimations && !hasThemeTransition && !hasSurfaceAnimations && !hasColumnTransitions &&
            !hasCompanionSprite && !hasTypingActivity
        then IO.pure(false)
        else
          // `stateRef.modify`, not a `set` built from the `state` read above: this tick runs on the render loop's own
          // fiber, concurrently with `AppRuntime.inputEventPhase`'s fiber applying a keystroke (or the quit
          // transition) to the same ref (#1564). A `set` computed from a stale snapshot would silently overwrite
          // whatever the input fiber committed in the meantime -- observed as a dropped keystroke, or the whole
          // runtime wedged because the quit signal it clobbered never landed. Recomputing from `current` inside
          // `modify` keeps this tick's write atomic with that concurrent one instead of blindly replacing it.
          for
            newState <- stateRef.modify { current =>
              val updatedTransition = current.runtime.themeTransition.map(_.advance).filterNot(_.isComplete)
              val advancedCompanionSprite =
                if hasCompanionSprite then
                  current.runtime.companionSprite
                    .tick(companionSpriteRandom, reducedRate = flairLevel == VisualFlairLevel.Reduced)
                else current.runtime.companionSprite
              val updatedColumnTransitions =
                current.runtime.columnTransitions.view.mapValues(_.advance).toMap.filterNot(_._2.isComplete)
              val stateWithAdvancedBuffers = current.copy(
                runtime = current.runtime.copy(
                  themeTransition = updatedTransition,
                  typingActivity = current.runtime.typingActivity.advance,
                  companionSprite = advancedCompanionSprite,
                  columnTransitions = updatedColumnTransitions
                )
              )
              val next = animations.advanceSurfaceAnimations(stateWithAdvancedBuffers)
              (next, next)
            }
            updatedBufferAnimations <- bufferAnimationsRef.updateAndGet(_.map {
              case (id, animations) =>
                val advanced = newState.persisted.buffers.get(id) match
                  case Some(buffer) => animations.advanceAllAnimations(isWithinViewport(buffer.viewport))
                  case None         => animations
                id -> advanced
            })
          yield newState.persisted.buffers.keys
            .exists(id => updatedBufferAnimations.get(id).exists(_.hasActiveAnimations)) ||
            newState.runtime.themeTransition.isDefined ||
            newState.runtime.surfaceAnimations.nonEmpty ||
            newState.runtime.columnTransitions.nonEmpty ||
            newState.runtime.typingActivity.isActive ||
            hasCompanionSprite
    yield stillActive

  /** A cell outside the buffer's currently visible viewport isn't rendered, so there's no need to pay its
    * interpolation/allocation cost on every tick -- it simply resumes advancing once scrolled back into view.
    */
  private def isWithinViewport(viewport: Viewport)(key: CharacterKey): Boolean =
    key.line >= viewport.topLine && key.line < viewport.topLine + viewport.visibleLines &&
      key.column >= viewport.leftColumn && key.column < viewport.leftColumn + viewport.visibleColumns

  val bufferManager: BufferManager = BufferManager(
    createBuffer = createBuffer,
    createNewEmptyBuffer = createNewEmptyBuffer(),
    updateBuffer = updateBuffer
  )

  private def createBuffer(content: String, filePath: Option[Path]): IO[BufferId] =
    // Advancing `nextBufferId` can never by itself violate an invariant (it never touches `buffers`/`bufferOrder`),
    // so it's split out as its own unchecked step -- the same shape `directLoadFileEffect` already uses. That means a
    // drifted `nextBufferId` that collides with a live buffer id is consumed here before the structural add below is
    // attempted, so a rejection falls back to a state that has already moved past the stale id instead of reverting
    // straight back to the collision.
    stateRef
      .modify { state =>
        val bufferId = state.runtime.nextBufferId
        (state.copy(runtime = state.runtime.copy(nextBufferId = BufferId(bufferId.value + 1))), bufferId)
      }
      .flatMap { bufferId =>
        val buffer =
          if content.isEmpty && filePath.isEmpty then Buffer.newEmpty(bufferId)(using balance)
          else
            val fresh = Buffer.fromString(bufferId, content)(using balance)
            fresh.copy(document = fresh.document.copy(filePath = filePath))
        stateRef.get.flatMap { state =>
          val newState = state.copy(persisted =
            state.persisted.copy(
              buffers = state.persisted.buffers + (bufferId -> buffer),
              bufferOrder = state.persisted.bufferOrder :+ bufferId
            )
          )
          validateAndUpdateState(newState, state).as(bufferId)
        }
      }

  private def createNewEmptyBuffer(): IO[BufferId] =
    stateRef.get.flatMap { state =>
      val (newState, bufferId) = EditorState.createNewEmptyBuffer(state)(using balance)
      validateAndUpdateState(newState, state).as(bufferId)
    }

  private def updateBuffer(bufferId: BufferId, content: String): IO[Unit] =
    stateRef.get.flatMap { state =>
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
          val newState = state.copy(persisted =
            state.persisted.copy(buffers = state.persisted.buffers + (bufferId -> updatedBuffer))
          )
          validateAndUpdateState(newState, state) >> stateRef.get.flatMap { committed =>
            if committed.persisted.buffers.get(bufferId).contains(updatedBuffer) then
              lspTarget.fold(IO.unit) {
                case (uri, languageId, text) => lspQueue.enqueueDocumentChange(uri, languageId, text)
              }
            else IO.unit
          }
        case None => IO.unit
    }

  def createPane(bufferId: Option[BufferId] = None): IO[PaneId] =
    stateRef.get.flatMap { state =>
      val (newState, paneId) = insertPane(
        state,
        state.persisted.layout.orderedPaneIds.lastOption,
        bufferId,
        SplitAxis.Horizontal
      )
      validateAndUpdateState(newState, state).as(paneId)
    }

  def switchToPane(paneId: PaneId): IO[Unit] =
    stateRef.get.flatMap { state =>
      if state.persisted.layout.editorPanes.contains(paneId) then
        val newState = state.copy(
          persisted = state.persisted.copy(
            layout = state.persisted.layout.copy(activeEditorPaneId = Some(paneId)),
            focus = Focus.EditorPane(paneId)
          )
        )
        validateAndUpdateState(newState, state)
      else IO.unit
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
          state.persisted.layout.workspaceTree.flatMap(
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
              workspaceTree = Some(tree)
            ),
            focus = Focus.EditorPane(paneId)
          ),
          runtime = state.runtime.copy(nextPaneId = PaneId(paneId.value + 1))
        )
        (updatedState, paneId)
      case None =>
        (state, paneId)
