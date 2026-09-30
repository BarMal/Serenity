package com.serenity

import com.serenity.config.AppMode
import com.serenity.state.models.AppState

/** Puts a state into a given app mode, for specs whose subject belongs to one mode's command family. */
object TestAppModes:

  def prose(state: AppState): AppState = inMode(AppMode.Prose)(state)

  def inMode(mode: AppMode)(state: AppState): AppState =
    state.copy(persisted = state.persisted.copy(config = state.persisted.config.withAppMode(mode)))
