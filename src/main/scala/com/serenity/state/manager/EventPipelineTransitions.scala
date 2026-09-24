package com.serenity.state.manager

import com.serenity.animation.SweepDirection
import com.serenity.keystroke.events.ResizeEvent
import com.serenity.state.models.AppState
import com.serenity.state.reducers.{AppEventReducer, ReducerResult, SystemEventReducer}

/** The event pipeline's own steps around a reducer, as pure functions, so each lands in the event's single validated
  * commit instead of a write of its own after it (#1183, #1697).
  */
private[manager] object EventPipelineTransitions:

  def resized(event: ResizeEvent, state: AppState): ReducerResult =
    val reduced = SystemEventReducer.reduce(event, state)
    reduced.copy(state = AppEventReducer.rebalancePanes(reduced.state, reduced.state.focusedBufferId))

  // Resolving the frozen cursor anchor to a screen position needs LayoutEngine, which reducers may not touch
  // (ArchitectureChecks.ForbiddenImports), so the pipeline does it right after the reduce that may have set it.
  def withCursorPeekAnchorResolved(result: ReducerResult): ReducerResult =
    result.copy(state = CursorPeekAnchorResolution.resolve(result.state))

  /** `result`'s state with its model-only effects (animations, undo bookkeeping) folded in. */
  def committed(model: Model, result: ReducerResult): Model =
    ModelCommit.applyModelEffects(model.copy(app = result.state), result.effects)

  def withPaneFlow(model: Model, sweep: SweepDirection): Model =
    model.copy(bufferAnimations = AnimationChoreography.withPaneFlowAnimation(model.app, sweep)(model.bufferAnimations))

  def commandRunnerFocusNormalized(state: AppState): AppState =
    if state.hasCommandRunnerDomain && !state.isCommandRunnerDomainFocus() then
      state.preferredCommandRunnerFocus.fold(state)(focus =>
        state.copy(persisted = state.persisted.copy(focus = focus))
      )
    else state
