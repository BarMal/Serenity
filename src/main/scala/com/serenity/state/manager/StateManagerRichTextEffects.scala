package com.serenity.state.manager

import cats.effect.IO
import cats.syntax.all.*
import com.serenity.command.RichTextIntent
import com.serenity.state.models.*
import com.serenity.state.reducers.{AppEffect, RichTextReducer}

/** Commits the pure [[RichTextReducer]]'s result for a rich-text formatting command. */
final private[manager] class StateManagerRichTextEffects(
    currentState: IO[AppState],
    commitState: (AppState, AppState) => IO[Unit],
    interpretEffect: AppEffect => IO[Unit]
):

  private[manager] def interpret(intent: RichTextIntent): IO[Unit] =
    currentState.flatMap { current =>
      val result = RichTextReducer.reduce(intent, current)
      commitState(result.state, current) >> result.effects.traverse_(interpretEffect)
    }
