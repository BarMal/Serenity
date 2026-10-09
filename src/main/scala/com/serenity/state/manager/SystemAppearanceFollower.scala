package com.serenity.state.manager

import scala.concurrent.duration.FiniteDuration

import cats.effect.IO
import cats.syntax.all.*
import com.serenity.state.models.AppState
import com.serenity.ui.theme.appearance.{OsAppearance, OsAppearanceDetector}
import org.typelevel.log4cats.Logger

/** Re-reads the OS appearance and switches to the matching configured theme, when `theme.follow_system` is on.
  *
  * The OS is not queried at all while following is off, so an opted-out user never pays for the process calls. A theme
  * being previewed from the settings palette is left alone: the preview is the person's to accept or abandon.
  */
final private[manager] class SystemAppearanceFollower(
    detector: OsAppearanceDetector,
    currentState: IO[AppState],
    switchTheme: String => IO[Unit],
    logger: Logger[IO]
):

  def follow: IO[Unit] = followUsing(detector.detect)

  /** For before the first frame, where a slow detector must not hold startup: an OS that has not answered within
    * `bound` is treated as not having said, so the configured theme stays.
    */
  def followWithin(bound: FiniteDuration): IO[Unit] =
    followUsing(
      detector.detect.timeoutTo(
        bound,
        logger
          .warn(s"[THEME] The OS appearance was not detected within ${bound.toMillis} ms; keeping the theme")
          .as(OsAppearance.Unknown)
      )
    )

  private def followUsing(detect: IO[OsAppearance]): IO[Unit] =
    currentState
      .flatMap { state =>
        val config = state.persisted.config.themeFollowConfig
        IO.whenA(config.followSystem && state.runtime.pendingSetting.isEmpty)(
          detect.flatMap(
            config.themeFor(_).filterNot(_ == state.persisted.theme.name).traverse_(switchTheme)
          )
        )
      }
      .handleErrorWith(error => logger.error(error)("[THEME] Following the system appearance failed"))
