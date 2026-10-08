package com.serenity.state.manager

import cats.effect.IO
import cats.syntax.all.*
import com.serenity.command.{CommentsIntent, NavigationIntent, PlaceholderIntent}
import com.serenity.state.models.*
import com.serenity.state.reducers.AppEffect
import com.serenity.ui.layout.WrappedLineCache

/** Commits the pure [[NavigationTransitions]] result for a navigation or comment command. */
final private[manager] class StateManagerNavigationEffects(
    currentState: IO[AppState],
    logger: org.typelevel.log4cats.Logger[IO],
    commitState: (AppState, AppState) => IO[Unit],
    interpretEffect: AppEffect => IO[Unit],
    wrapCache: WrappedLineCache = WrappedLineCache.Uncached
):

  private[manager] def interpretComments(intent: CommentsIntent): IO[Unit] =
    IO.realTimeInstant.flatMap(now => commit(NavigationTransitions.comments(intent, _, now, wrapCache = wrapCache)))

  private[manager] def interpretPlaceholders(intent: PlaceholderIntent): IO[Unit] =
    commit(NavigationTransitions.placeholders(intent, _, wrapCache = wrapCache))

  private[manager] def interpretNavigation(intent: NavigationIntent): IO[Unit] =
    commit(NavigationTransitions.navigation(intent, _, wrapCache = wrapCache))

  private def commit(transition: AppState => NavigationOutcome): IO[Unit] =
    currentState.flatMap { current =>
      transition(current) match
        case NavigationOutcome.Applied(result) =>
          commitState(result.state, current) >> result.effects.traverse_(interpretEffect)
        case NavigationOutcome.Ignored(debugLog) =>
          debugLog.traverse_(logger.debug(_))
    }
