package com.serenity.testkit

import java.awt.Color

import com.serenity.animation.{AnimatedCell, AnimationOwner, AnimationState, CharacterKey, EasingCurve, Tween}
import com.serenity.state.models.{AppState, BufferId}

/** An in-flight buffer animation on `state`'s first buffer, for specs that need the render loop to see animation. */
object ActiveAnimationFixtures:

  def bufferAnimations(state: AppState): Map[BufferId, AnimationState] =
    state.persisted.buffers.keys.headOption.toList.map { bufferId =>
      bufferId -> AnimationState(
        Map(
          CharacterKey(0, 0) ->
            AnimatedCell(
              Some('a'),
              Some(Tween(Color.BLACK, Color.WHITE, EasingCurve.Linear, steps = 1000)),
              None,
              AnimationOwner.EditorText
            )
        )
      )
    }.toMap
