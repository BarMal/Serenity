package com.serenity.state.manager

import com.serenity.animation.*
import com.serenity.config.AppConfigMotionOps.*
import com.serenity.state.models.*

/** The buffer sweep animation used for tab-cycling, seeded into a buffer's `bufferAnimations`. */
private[manager] object AnimationChoreography:

  def withPaneFlowAnimation(state: AppState, sweep: SweepDirection)(
    bufferAnimations: Map[BufferId, AnimationState]
  ): Map[BufferId, AnimationState] =
    val sweptBuffer = for
      config <- state.persisted.config.scaledUiAnimation
      paneId <- state.persisted.layout.activeEditorPaneId
      pane   <- state.persisted.layout.editorPanes.get(paneId)
      buffId <- pane.bufferId
      buffer <- state.persisted.buffers.get(buffId)
      cells = VisibleBufferAnimationCells.fromBuffer(
        buffer,
        state.persisted.config.surfaceConfig.wordWrapEnabled,
        state.persisted.theme.background,
        state.persisted.theme.foreground
      )
      if cells.nonEmpty
    yield
      val animated = FlowAnimationBuilder.build(cells, FlowDirection.ByColumn, sweep, config.steps)
      buffId -> animated.view.mapValues(_.copy(owner = AnimationOwner.UiTransitions)).toMap
    sweptBuffer.fold(bufferAnimations) { (buffId, uiAnimations) =>
      val animations = bufferAnimations.getOrElse(buffId, AnimationState.empty)
      bufferAnimations.updated(
        buffId,
        animations.clear(AnimationOwner.UiTransitions).mergeUiTransitionAnimations(uiAnimations)
      )
    }
