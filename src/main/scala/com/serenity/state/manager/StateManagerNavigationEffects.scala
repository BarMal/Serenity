package com.serenity.state.manager

import cats.effect.IO
import cats.syntax.all.*
import com.serenity.command.{CommentsIntent, NavigationIntent}
import com.serenity.state.models.*
import com.serenity.state.reducers.AppEffect

/** Commits the pure [[NavigationTransitions]] result for a navigation or comment command. */
final private[manager] class StateManagerNavigationEffects(
    currentState: IO[AppState],
    logger: org.typelevel.log4cats.Logger[IO],
    commitState: (AppState, AppState) => IO[Unit],
    interpretEffect: AppEffect => IO[Unit]
):

  private[manager] def interpretComments(intent: CommentsIntent): IO[Unit] =
    commit(NavigationTransitions.comments(intent, _))

  private[manager] def interpretNavigation(intent: NavigationIntent): IO[Unit] =
    commit(NavigationTransitions.navigation(intent, _))

  private def commit(transition: AppState => NavigationOutcome): IO[Unit] =
    currentState.flatMap { current =>
      transition(current) match
        case NavigationOutcome.Applied(result) =>
          commitState(result.state, current) >> result.effects.traverse_(interpretEffect)
        case NavigationOutcome.Ignored(debugLog) =>
          debugLog.traverse_(logger.debug(_))
    }
