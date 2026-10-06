package com.serenity.state.manager

import cats.effect.IO
import cats.syntax.all.*
import com.serenity.state.models.AppState
import com.serenity.ui.theme.appearance.OsAppearanceDetector
import org.typelevel.log4cats.Logger

/** Re-reads the OS appearance and switches to the matching configured theme, when `theme.follow_system` is on.
  *
  * The OS is not queried at all while following is off, so an opted-out user never pays for the process calls.
  */
final private[manager] class SystemAppearanceFollower(
    detector: OsAppearanceDetector,
    currentState: IO[AppState],
    switchTheme: String => IO[Unit],
    logger: Logger[IO]
):

  def follow: IO[Unit] =
    currentState
      .flatMap { state =>
        val config = state.persisted.config.themeFollowConfig
        IO.whenA(config.followSystem)(
          detector.detect.flatMap(
            config.themeFor(_).filterNot(_ == state.persisted.theme.name).traverse_(switchTheme)
          )
        )
      }
      .handleErrorWith(error => logger.error(error)("[THEME] Following the system appearance failed"))
