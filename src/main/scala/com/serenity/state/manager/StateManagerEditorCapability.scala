package com.serenity.state.manager

import java.nio.file.Path

import scala.util.Random

import cats.effect.IO
import com.serenity.animation.CharacterKey
import com.serenity.config.VisualFlairLevel
import com.serenity.state.core.EditorState
import com.serenity.state.models.*
import com.serenity.ui.layout.*

final private[manager] class StateManagerEditorCapability(
    modelCommit: ModelCommit,
    lspQueue: LspEffectQueue,
    animations: AnimationChoreography,
    operations: StateManagerOperationBoundary,
    // Seeds the companion sprite's pseudo-random idle-to-action rolls (see `CompanionSpriteState`'s transition
    // policy). A single mutable source threaded through every tick, same as a real hardware RNG would be -- the pure
    // transition logic itself never touches unseeded randomness directly, only what this IO-boundary constructor
    // passes it. Tests construct this class with a seeded `Random` for a deterministic trace.
    companionSpriteRandom: Random = new Random()
)(using balance: com.serenity.rope.Balance):

  def getModel: IO[Model] = modelCommit.model

  def getCurrentState: IO[AppState] = modelCommit.currentState

  def getBufferAnimations: IO[Map[BufferId, com.serenity.animation.AnimationState]] =
    modelCommit.model.map(_.bufferAnimations)

  val focusManager: FocusManager = FocusManager(switchFocus = switchFocus)

  private def switchFocus(newFocus: Focus): IO[Unit] =
    modelCommit.currentState.flatMap(state =>
      modelCommit.commitState(EditorTransitions.focused(state, newFocus), state)
    )

  def updateState(update: AppState => AppState): IO[Unit] =
    modelCommit.updateUnvalidated(model => model.copy(app = update(model.app))).void

  def updateStateValidated(update: AppState => AppState): IO[Unit] =
    operations.dispatch(modelCommit.currentState.flatMap(state => modelCommit.commitState(update(state), state)))

  def updateBufferAnimations(
    update: Map[BufferId, com.serenity.animation.AnimationState] => Map[BufferId, com.serenity.animation.AnimationState]
  ): IO[Unit] =
    modelCommit.updateBufferAnimations(update)

  val animationTicker: AnimationTicker = AnimationTicker(advanceAnimationsOnTick = advanceAnimationsOnTick())

  private def advanceAnimationsOnTick(): IO[Boolean] =
    for
      model <- modelCommit.model
      state            = model.app
      bufferAnimations = model.bufferAnimations
      hasBufferAnimations = state.persisted.buffers.keys.exists(id =>
        bufferAnimations.get(id).exists(_.hasActiveAnimations)
      )
      hasThemeTransition   = state.runtime.themeTransition.isDefined
      hasSurfaceAnimations = state.runtime.surfaceAnimations.nonEmpty
      hasColumnTransitions = state.runtime.columnTransitions.nonEmpty
      hasPanelGeometry     = state.runtime.panelGeometry.nonEmpty
      hasCursorGlide       = state.persisted.buffers.values.exists(hasInFlightGlide)
      hasSelectionGeometry = state.persisted.buffers.values.exists(hasInFlightSelectionGeometry)
      hasTypingActivity    = state.runtime.typingActivity.isActive
      flairLevel           = state.persisted.config.visualFlairLevel
      hasCompanionSprite   = state.persisted.config.companionSpriteConfig.enabled && flairLevel != VisualFlairLevel.Off
      stillActive <-
        if !hasBufferAnimations && !hasThemeTransition && !hasSurfaceAnimations && !hasColumnTransitions &&
            !hasPanelGeometry && !hasCursorGlide && !hasSelectionGeometry && !hasCompanionSprite &&
            !hasTypingActivity
        then IO.pure(false)
        else
          // A dispatch in flight would commit a state built from its own earlier snapshot over this tick's write
          // (#1564), and waiting for it would stall the render loop behind its I/O -- so skip this tick and report
          // still-active so the next frame retries.
          IO(companionSpriteRandom.nextLong()).flatMap { companionSpriteSeed =>
            operations
              .runIfDispatcherIdle(advanceOneTick(hasCompanionSprite, flairLevel, companionSpriteSeed))
              .map(_.getOrElse(true))
          }
    yield stillActive

  private def advanceOneTick(
    hasCompanionSprite: Boolean,
    flairLevel: VisualFlairLevel,
    companionSpriteSeed: Long
  ): IO[Boolean] =
    // An atomic update rather than a `set`: writers outside the dispatcher (`updateState`, the buffer/panel records)
    // still exist, and this keeps the tick atomic with them. It may retry, so everything it reads is passed in.
    modelCommit
      .updateUnvalidated(advanceModel(_, hasCompanionSprite, flairLevel, companionSpriteSeed))
      .map { next =>
        val newState = next.app
        newState.persisted.buffers.keys.exists(id => next.bufferAnimations.get(id).exists(_.hasActiveAnimations)) ||
        newState.runtime.themeTransition.isDefined ||
        newState.runtime.surfaceAnimations.nonEmpty ||
        newState.runtime.columnTransitions.nonEmpty ||
        newState.runtime.panelGeometry.nonEmpty ||
        newState.persisted.buffers.values.exists(hasInFlightGlide) ||
        newState.persisted.buffers.values.exists(hasInFlightSelectionGeometry) ||
        newState.runtime.typingActivity.isActive ||
        hasCompanionSprite
      }

  private def advanceModel(
    current: Model,
    hasCompanionSprite: Boolean,
    flairLevel: VisualFlairLevel,
    companionSpriteSeed: Long
  ): Model =
    val state             = current.app
    val updatedTransition = state.runtime.themeTransition.map(_.advance).filterNot(_.isComplete)
    val advancedCompanionSprite =
      if hasCompanionSprite then
        state.runtime.companionSprite
          .tick(new Random(companionSpriteSeed), reducedRate = flairLevel == VisualFlairLevel.Reduced)
      else state.runtime.companionSprite
    val updatedColumnTransitions =
      state.runtime.columnTransitions.view.mapValues(_.advance).toMap.filterNot(_._2.isComplete)
    val stateWithAdvancedBuffers = state.copy(
      persisted = state.persisted.copy(
        buffers = state.persisted.buffers.view
          .mapValues(advanceCursorGlides andThen advanceSelectionGeometries)
          .toMap
      ),
      runtime = state.runtime.copy(
        themeTransition = updatedTransition,
        typingActivity = state.runtime.typingActivity.advance,
        companionSprite = advancedCompanionSprite,
        columnTransitions = updatedColumnTransitions
      )
    )
    val newState = animations.advancePanelGeometry(animations.advanceSurfaceAnimations(stateWithAdvancedBuffers))
    val advancedBufferAnimations = current.bufferAnimations.map {
      case (id, bufferAnimations) =>
        val advanced = newState.persisted.buffers.get(id) match
          case Some(buffer) => bufferAnimations.advanceAllAnimations(isWithinViewport(buffer.viewport))
          case None         => bufferAnimations
        id -> advanced
    }
    current.copy(app = newState, bufferAnimations = advancedBufferAnimations)

  /** Caret-glide (issue #1085 phase 2): whether `buffer` has any cursor with a glide still mid-flight -- checked before
    * paying the cost of advancing every buffer's cursors on a tick that has nothing else to do either.
    */
  private def hasInFlightGlide(buffer: Buffer): Boolean =
    buffer.editing.cursors.exists(_.glide.exists(!_.isComplete))

  /** Selection grow/settle (issue #1085 phase 3): whether `buffer` has any cursor with a selection geometry still
    * mid-flight -- the same early-out `hasInFlightGlide` gives caret glide.
    */
  private def hasInFlightSelectionGeometry(buffer: Buffer): Boolean =
    buffer.editing.cursors.exists(_.selectionGeometry.exists(!_.isComplete))

  /** Advances every cursor's `glide` by one tick, dropping it once complete -- the per-cursor equivalent of
    * `Runtime.columnTransitions`'/`Runtime.panelGeometry`'s tick-driven advance, except this state lives on `Cursor`
    * inside `Buffer.editing.cursors` (`#1577`) rather than a top-level `Runtime` map, so it advances by rebuilding each
    * buffer's cursor list instead of updating a `Runtime` field.
    */
  private def advanceCursorGlides(buffer: Buffer): Buffer =
    val advancedCursors = buffer.editing.cursors.map { cursor =>
      cursor.glide match
        case Some(glide) =>
          val advanced = glide.advance
          if advanced.isComplete then cursor.copy(glide = None) else cursor.copy(glide = Some(advanced))
        case None => cursor
    }
    if advancedCursors == buffer.editing.cursors then buffer else buffer.withCursorList(advancedCursors)

  /** Selection grow/settle (issue #1085 phase 3): advances every cursor's `selectionGeometry` by one tick, dropping it
    * once every line's tween completes -- the per-cursor equivalent of `advanceCursorGlides` just above.
    */
  private def advanceSelectionGeometries(buffer: Buffer): Buffer =
    val advancedCursors = buffer.editing.cursors.map { cursor =>
      cursor.selectionGeometry match
        case Some(geometry) =>
          val advanced = geometry.advance
          if advanced.isComplete then cursor.copy(selectionGeometry = None)
          else cursor.copy(selectionGeometry = Some(advanced))
        case None => cursor
    }
    if advancedCursors == buffer.editing.cursors then buffer else buffer.withCursorList(advancedCursors)

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
    modelCommit.currentState.flatMap { state =>
      val creation = EditorTransitions.bufferCreated(state, content, filePath)
      modelCommit.commitState(creation.created, creation.idAdvanced).as(creation.bufferId)
    }

  private def createNewEmptyBuffer(): IO[BufferId] =
    modelCommit.currentState.flatMap { state =>
      val (newState, bufferId) = EditorState.createNewEmptyBuffer(state)(using balance)
      modelCommit.commitState(newState, state).as(bufferId)
    }

  private def updateBuffer(bufferId: BufferId, content: String): IO[Unit] =
    modelCommit.currentState.flatMap { state =>
      EditorTransitions.bufferContentReplaced(state, bufferId, content) match
        case Some(replacement) =>
          modelCommit.commitState(replacement.state, state) >> modelCommit.currentState.flatMap { committed =>
            if committed.persisted.buffers.get(bufferId).contains(replacement.buffer) then
              replacement.documentChange.fold(IO.unit) {
                case (uri, languageId, text) => lspQueue.enqueueDocumentChange(uri, languageId, text)
              }
            else IO.unit
          }
        case None => IO.unit
    }

  def createPane(bufferId: Option[BufferId] = None): IO[PaneId] =
    modelCommit.currentState.flatMap { state =>
      val (newState, paneId) = EditorTransitions.paneInserted(
        state,
        state.persisted.layout.orderedPaneIds.lastOption,
        bufferId,
        SplitAxis.Horizontal
      )
      modelCommit.commitState(newState, state).as(paneId)
    }

  def switchToPane(paneId: PaneId): IO[Unit] =
    modelCommit.currentState.flatMap { state =>
      EditorTransitions.paneSwitched(state, paneId).fold(IO.unit)(modelCommit.commitState(_, state))
    }

  def getTabOrder(): IO[List[PaneId]] =
    modelCommit.currentState.map(_.persisted.layout.orderedPaneIds)
