package com.serenity.state.manager

import cats.effect.IO
import com.serenity.animation.{AnimationOwner, AnimationState}
import com.serenity.state.models.BufferId
import com.serenity.state.reducers.AnimationEffect

/** Interprets `AnimationEffect`s into the buffer-animation side table. `Buffer` carries no animation state (`#1001`) --
  * this is where the presentation layer that actually owns `AnimationState` applies a reducer-computed change. The
  * event pipeline folds a reducer result's animation effects into the same model write as its state
  * (`ModelCommit.applyModelEffects`); `interpret` covers an effect that reaches the effect interpreter on its own. The
  * resulting state is read back via `StateManager.getBufferAnimations`.
  */
final private[manager] class AnimationEffectHandler(
    updateBufferAnimations: (Map[BufferId, AnimationState] => Map[BufferId, AnimationState]) => IO[Unit]
):

  def interpret(effect: AnimationEffect): IO[Unit] =
    updateBufferAnimations(AnimationEffectHandler.applied(_, effect))

private[manager] object AnimationEffectHandler:

  def applied(animations: Map[BufferId, AnimationState], effect: AnimationEffect): Map[BufferId, AnimationState] =
    effect match
      case AnimationEffect.RemapThroughEdits(bufferId, before, after, edits) =>
        animations.get(bufferId) match
          case Some(state) => animations.updated(bufferId, state.remapThroughEdits(before, after, edits))
          case None        => animations
      case AnimationEffect.Merge(bufferId, delta) =>
        val current = animations.getOrElse(bufferId, AnimationState.empty)
        animations.updated(bufferId, current.mergeAnimations(delta))
      case AnimationEffect.ClearAll(bufferId) =>
        animations - bufferId
      case AnimationEffect.ClearOwner(bufferId, owner) =>
        animations.get(bufferId) match
          case Some(state) => animations.updated(bufferId, state.clear(owner))
          case None        => animations
      case AnimationEffect.RestartUiTransitions(bufferId, cells) =>
        val current = animations.getOrElse(bufferId, AnimationState.empty)
        animations.updated(bufferId, current.clear(AnimationOwner.UiTransitions).mergeUiTransitionAnimations(cells))
