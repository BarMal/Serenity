package com.serenity.app

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, LinkOption, Path, Paths}

import cats.effect.IO
import com.serenity.config.{AppConfig, ConfigError, ConfigManager, ConfigNotice, LayoutPin, LayoutPreset}
import org.slf4j.LoggerFactory

/** The one-time question of what to do about chrome the config file pins away from the layout in force.
  *
  * Earlier builds wrote every setting when they made a config file, so such files pin the chrome the layout now
  * supplies, and the layout does nothing for their owners. The question is asked on the first launch that finds the
  * marker absent and the file readable, whatever its answer, and is never asked again: the marker is left when the
  * question is posed, not when it is answered, because dismissing it is an answer (keep the file as it is). It lives
  * beside the other launch markers rather than in the config file, which would otherwise gain a key nobody set, or the
  * session, which can be reset without the question being new again.
  */
object LayoutOverrideReview:

  private val logger = LoggerFactory.getLogger("com.serenity.app.LayoutOverrideReview")

  def defaultMarker: Path =
    Paths.get(System.getProperty("user.home"), ".serenity", "layout-override-reviewed")

  /** `plan` with the question added when this launch is the one to ask it, and the notice to show when the file could
    * not be read to find out. Safe mode never read the user's file, and a file that did not load has nothing to ask
    * about nor an answer to record.
    */
  def planned(
    plan: StartupRecovery.Plan,
    configLoaded: Boolean,
    config: AppConfig
  ): IO[(StartupRecovery.Plan, Option[String])] =
    if plan.safeMode || !configLoaded then IO.pure((plan, None))
    else
      review(defaultMarker, ConfigManager.defaultConfigPath, config).map {
        case Right(pins) => (plan.copy(layoutPins = pins), None)
        case Left(error) => (plan, Some(ConfigNotice.unreadableForReview(ConfigManager.defaultConfigPath, error)))
      }

  /** The chrome to ask about, none once the question has been posed or when the file cannot be read. */
  def due(marker: Path, configPath: Path, config: AppConfig): IO[List[LayoutPin]] =
    review(marker, configPath, config).map(_.getOrElse(Nil))

  /** The chrome to ask about, or why the file could not be read. The marker is left only once the file has been read,
    * so an unreadable one is asked about at the next launch, and not at all when the marker cannot be left: a question
    * that would come back on every launch is worse than one never asked.
    */
  def review(marker: Path, configPath: Path, config: AppConfig): IO[Either[ConfigError, List[LayoutPin]]] =
    IO.blocking(Files.exists(marker, LinkOption.NOFOLLOW_LINKS)).handleError(_ => true).flatMap {
      case true => IO.pure(Right(Nil))
      case false =>
        ConfigManager.settingKeysIO(configPath).flatMap {
          case Left(error) =>
            val message = s"[CONFIG] Layout review skipped: ${error.message}"
            IO(error.cause.fold(logger.error(message))(logger.error(message, _))).as(Left(error))
          case Right(keys) =>
            leaveMarker(marker)
              .as(Right(LayoutPreset.pins(config, keys, config.uiLayout)))
              .handleError(_ => Right(Nil))
        }
    }

  private def leaveMarker(marker: Path): IO[Unit] =
    IO.realTimeInstant.flatMap(at =>
      IO.blocking {
        Files.createDirectories(marker.getParent)
        val _ = Files.write(marker, s"asked $at\n".getBytes(StandardCharsets.UTF_8))
      }
    )
