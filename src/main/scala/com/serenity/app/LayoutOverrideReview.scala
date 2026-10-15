package com.serenity.app

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, LinkOption, Path, Paths}

import cats.effect.IO
import com.serenity.config.{AppConfig, ConfigManager, LayoutPin, LayoutPreset}

/** The one-time question of what to do about chrome the config file pins away from the layout in force.
  *
  * Earlier builds wrote every setting when they made a config file, so such files pin the chrome the layout now
  * supplies, and the layout does nothing for their owners. The question is asked on the first launch that finds the
  * marker absent, whatever its answer, and is never asked again: the marker is left when the question is posed, not
  * when it is answered, because dismissing it is an answer (keep the file as it is). It lives beside the other launch
  * markers rather than in the config file, which would otherwise gain a key nobody set, or the session, which can be
  * reset without the question being new again.
  */
object LayoutOverrideReview:

  def defaultMarker: Path =
    Paths.get(System.getProperty("user.home"), ".serenity", "layout-override-reviewed")

  /** `plan` with the question added when this launch is the one to ask it. Safe mode never read the user's file, and a
    * file that did not load has nothing to ask about nor an answer to record.
    */
  def planned(plan: StartupRecovery.Plan, configLoaded: Boolean, config: AppConfig): IO[StartupRecovery.Plan] =
    if plan.safeMode || !configLoaded then IO.pure(plan)
    else due(defaultMarker, ConfigManager.defaultConfigPath, config).map(pins => plan.copy(layoutPins = pins))

  /** The chrome to ask about, none once the question has been posed. Leaves the marker either way, and asks nothing
    * when it cannot: a question that would come back on every launch is worse than one never asked.
    */
  def due(marker: Path, configPath: Path, config: AppConfig): IO[List[LayoutPin]] =
    IO.blocking(Files.exists(marker, LinkOption.NOFOLLOW_LINKS)).handleError(_ => true).flatMap {
      case true => IO.pure(Nil)
      case false =>
        (leaveMarker(marker) *> pinsOf(configPath, config)).handleError(_ => Nil)
    }

  private def pinsOf(configPath: Path, config: AppConfig): IO[List[LayoutPin]] =
    ConfigManager.settingKeysIO(configPath).map(LayoutPreset.pins(config, _, config.uiLayout))

  private def leaveMarker(marker: Path): IO[Unit] =
    IO.realTimeInstant.flatMap(at =>
      IO.blocking {
        Files.createDirectories(marker.getParent)
        val _ = Files.write(marker, s"asked $at\n".getBytes(StandardCharsets.UTF_8))
      }
    )
