package com.serenity.state.manager

import cats.effect.{IO, Ref}
import cats.syntax.all.*
import com.serenity.command.RichTextIntent
import com.serenity.state.models.*
import com.serenity.state.reducers.{AppEffect, RichTextReducer}

/** Commits the pure [[RichTextReducer]]'s result for a rich-text formatting command. */
final private[manager] class StateManagerRichTextEffects(
    stateRef: Ref[IO, AppState],
    validateAndUpdateState: (AppState, AppState) => IO[Unit],
    interpretEffect: AppEffect => IO[Unit]
):

  private[manager] def interpret(intent: RichTextIntent): IO[Unit] =
    stateRef.get.flatMap { current =>
      val result = RichTextReducer.reduce(intent, current)
      validateAndUpdateState(result.state, current) >> result.effects.traverse_(interpretEffect)
    }
