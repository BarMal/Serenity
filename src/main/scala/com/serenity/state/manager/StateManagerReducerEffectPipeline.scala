package com.serenity.state.manager

import cats.effect.IO
import cats.syntax.foldable.*
import com.serenity.state.models.AppState
import com.serenity.state.reducers.{AppEffect, ReducerResult}

/** Commits a reducer result and interprets its effects through injected boundaries. */
final private[manager] class StateManagerReducerEffectPipeline(
    dependencies: StateManagerReducerEffectPipeline.Dependencies
):

  def apply(result: ReducerResult, fallbackState: AppState): IO[Unit] =
    dependencies.validateAndUpdateState(result.state, fallbackState) >>
      result.effects.traverse_(dependencies.interpretEffect)

private[manager] object StateManagerReducerEffectPipeline:

  final case class Dependencies(
      validateAndUpdateState: (AppState, AppState) => IO[Unit],
      interpretEffect: AppEffect => IO[Unit]
  )
