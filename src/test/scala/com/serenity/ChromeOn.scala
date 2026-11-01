package com.serenity

import cats.effect.IO
import com.serenity.config.{AppConfig, DockedCornerStyle}
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.models.AppState
import com.serenity.testkit.SharedDictionary
import org.typelevel.log4cats.{Logger, LoggerFactory}

/** The states and managers specs about line numbers, pane headers, the status bar and the tab strip start from:
  * [[AppState.initial]] and [[AppState.empty]] on the conventional layout, since the app's own default is the minimal
  * one with none of them.
  *
  * Docked chrome is flush here: those specs pin where each element sits against the window, and the inset that
  * `floating` adds has specs of its own (`FloatingChromeInsetSpec`).
  */
object ChromeOn:

  val config: AppConfig = AppConfig.conventional.withDockedCornerStyle(DockedCornerStyle.Flush)

  def initial(using Balance): AppState = AppState.initial(config)

  def stateManager(logger: Logger[IO])(using Balance, LoggerFactory[IO]): IO[StateManager] =
    StateManager.apply(logger, initialConfig = config, dictionaryCache = SharedDictionary.default)

  def empty: AppState = AppState.empty(config)
