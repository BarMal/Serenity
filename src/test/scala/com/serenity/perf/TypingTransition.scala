package com.serenity.perf

import cats.effect.unsafe.IORuntime
import com.serenity.keystroke.events.InsertChar
import com.serenity.state.manager.StateManager
import com.serenity.state.models.AppState

private[perf] object TypingTransition:

  /** One keystroke into a buffer the previous keystroke already modified. The first keystroke after a save also flips
    * the dirty marker of the tab and header, which presents the whole window once; typing a frame after frame never
    * does, so a benchmark alternating clean and modified would time that exception every frame.
    */
  def of(stateManager: StateManager)(using IORuntime): (AppState, AppState) =
    stateManager.applyEvent(InsertChar('x')).unsafeRunSync()
    val before = stateManager.getCurrentState.unsafeRunSync()
    stateManager.applyEvent(InsertChar('y')).unsafeRunSync()
    (before, stateManager.getCurrentState.unsafeRunSync())
