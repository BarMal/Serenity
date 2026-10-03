package com.serenity.state.manager

import cats.effect.IO
import com.serenity.animation.CharacterKey
import com.serenity.state.models.*
import com.serenity.ui.layout.*

final private[manager] class StateManagerEditorCapability(
    modelCommit: ModelCommit,
    operations: StateManagerOperationBoundary
):

  def getModel: IO[Model] = modelCommit.model

  def getCurrentState: IO[AppState] = modelCommit.currentState

  def updateStateValidated(update: AppState => AppState): IO[Unit] =
    operations.dispatch(modelCommit.currentState.flatMap(state => modelCommit.commitState(update(state), state)))

  val animationTicker: AnimationTicker = AnimationTicker(advanceAnimationsOnTick = advanceAnimationsOnTick())

  private def advanceAnimationsOnTick(): IO[Boolean] =
    for
      model <- modelCommit.model
      state            = model.app
      bufferAnimations = model.bufferAnimations
      hasBufferAnimations = state.persisted.buffers.keys.exists(id =>
        bufferAnimations.get(id).exists(_.hasActiveAnimations)
      )
      hasTypingActivity = state.typingHidesFloatingStatusLine
      stillActive <-
        if !hasBufferAnimations && !hasTypingActivity
        then IO.pure(false)
        else
          // A dispatch in flight would commit a state built from its own earlier snapshot over this tick's write
          // (#1564), and waiting for it would stall the render loop behind its I/O -- so skip this tick and report
          // still-active so the next frame retries.
          for
            now      <- IO.monotonic
            advanced <- operations.runIfDispatcherIdle(advanceOneTick(now.toNanos))
          yield advanced.getOrElse(true)
    yield stillActive

  private def advanceOneTick(nowNanos: Long): IO[Boolean] =
    // An atomic, validated update rather than a `set`: writers outside the dispatcher (`updateState`, the
    // buffer/panel records) still exist, and this keeps the tick atomic with them. It may retry, so everything it
    // reads is passed in.
    modelCommit
      .advanceTick(advanceModel(_, nowNanos))
      .map { next =>
        val newState = next.app
        newState.persisted.buffers.keys.exists(id => next.bufferAnimations.get(id).exists(_.hasActiveAnimations)) ||
        newState.typingHidesFloatingStatusLine
      }

  private def advanceModel(current: Model, nowNanos: Long): Model =
    val state = current.app
    val newState =
      state.copy(runtime = state.runtime.copy(typingActivity = state.runtime.typingActivity.advance(nowNanos)))
    val advancedBufferAnimations = current.bufferAnimations.map {
      case (id, bufferAnimations) =>
        val advanced = newState.persisted.buffers.get(id) match
          case Some(buffer) => bufferAnimations.advanceAllAnimations(isWithinViewport(buffer.viewport))
          case None         => bufferAnimations
        id -> advanced
    }
    current.copy(app = newState, bufferAnimations = advancedBufferAnimations)

  /** A cell outside the buffer's currently visible viewport isn't rendered, so there's no need to pay its
    * interpolation/allocation cost on every tick -- it simply resumes advancing once scrolled back into view.
    */
  private def isWithinViewport(viewport: Viewport)(key: CharacterKey): Boolean =
    key.line >= viewport.topLine && key.line < viewport.topLine + viewport.visibleLines &&
      key.column >= viewport.leftColumn && key.column < viewport.leftColumn + viewport.visibleColumns

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
