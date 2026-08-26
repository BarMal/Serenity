package com.serenity.state.manager

import cats.effect.{IO, Ref}
import com.serenity.animation.AnimationState
import com.serenity.state.models.BufferId
import com.serenity.state.reducers.AnimationEffect

/** Interprets `AnimationEffect`s into the buffer-animation side table. `Buffer` carries no animation state (`#1001`) --
  * this is where the presentation layer that actually owns `AnimationState` applies a reducer-computed change. Not yet
  * wired to any reducer emission site or read by any painter; landed here so the side table has a real, exercised owner
  * before either side is migrated onto it.
  */
final private[manager] class AnimationEffectHandler(bufferAnimationsRef: Ref[IO, Map[BufferId, AnimationState]]):

  def interpret(effect: AnimationEffect): IO[Unit] =
    effect match
      case AnimationEffect.RemapThroughEdits(bufferId, before, after, edits) =>
        bufferAnimationsRef.update { animations =>
          animations.get(bufferId) match
            case Some(state) => animations.updated(bufferId, state.remapThroughEdits(before, after, edits))
            case None        => animations
        }
      case AnimationEffect.Merge(bufferId, delta) =>
        bufferAnimationsRef.update { animations =>
          val current = animations.getOrElse(bufferId, AnimationState.empty)
          animations.updated(bufferId, current.mergeAnimations(delta))
        }
      case AnimationEffect.ClearAll(bufferId) =>
        bufferAnimationsRef.update(_ - bufferId)
      case AnimationEffect.ClearOwner(bufferId, owner) =>
        bufferAnimationsRef.update { animations =>
          animations.get(bufferId) match
            case Some(state) => animations.updated(bufferId, state.clear(owner))
            case None        => animations
        }
