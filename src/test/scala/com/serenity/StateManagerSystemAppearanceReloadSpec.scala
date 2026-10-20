package com.serenity

import java.nio.file.{Files, Path}

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import cats.syntax.all.*
import com.serenity.config.AppConfigOps.*
import com.serenity.config.{AppConfig, ConfigManager, ThemeFollowConfig}
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.testkit.AwaitCondition.awaitValue
import com.serenity.testkit.SharedDictionary
import com.serenity.ui.theme.appearance.{OsAppearance, OsAppearanceDetector}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory

/** The config file is reloaded on a window focus regain too (#2132), alongside the follower. Whatever order the two
  * land in, the theme ends up the one the file's `theme.*` settings and the OS agree on.
  */
class StateManagerSystemAppearanceReloadSpec extends AnyFlatSpec with Matchers:

  given Balance                                  = Balance.default
  given org.typelevel.log4cats.LoggerFactory[IO] = Slf4jFactory.create[IO]

  private val followingDark = AppConfig.default.withThemeFollowConfig(ThemeFollowConfig(followSystem = true))

  final private class Os(appearance: Ref[IO, OsAppearance], reads: Ref[IO, Int]) extends OsAppearanceDetector:
    def detect: IO[OsAppearance]          = reads.update(_ + 1) >> appearance.get
    def set(next: OsAppearance): IO[Unit] = appearance.set(next)
    def detections: IO[Int]               = reads.get

  private def os(initial: OsAppearance): Os =
    (Ref.of[IO, OsAppearance](initial), Ref.of[IO, Int](0)).mapN(new Os(_, _)).unsafeRunSync()

  private def editorOn(configPath: Path, detector: OsAppearanceDetector): StateManager =
    StateManager
      .apply(
        org.typelevel.log4cats.noop.NoOpLogger.impl[IO],
        sessionRootOverride = Some(TestTemp.directory("appearance-reload-session")),
        initialConfig = followingDark,
        configPersistencePath = Some(configPath),
        dictionaryCache = SharedDictionary.cacheFor(followingDark),
        appearanceDetector = detector
      )
      .unsafeRunSync()

  private def configFile: Path = TestTemp.directory("appearance-reload").resolve("config.conf")

  private def editFile(path: Path, config: AppConfig): Unit =
    Files.writeString(path, ConfigManager.configToString(config)): Unit

  private def reloadFromWatcher(manager: StateManager): Unit =
    (manager.fileService.configWatch.traverse_(_.reload) >> manager.runtimeLifecycle.awaitEffects).unsafeRunSync()

  private def themeName(manager: StateManager): IO[String] = manager.getCurrentState.map(_.persisted.theme.name)

  "A config reload that rewrites theme.* while follow_system is on" should
    "leave the theme the OS asks for in the slot the file now names" in {
      val path    = configFile
      val system  = os(OsAppearance.Dark)
      val manager = editorOn(path, system)
      manager.followSystemAppearance.unsafeRunSync()
      themeName(manager).unsafeRunSync() shouldBe "dark"

      editFile(path, followingDark.withThemeFollowConfig(ThemeFollowConfig(followSystem = true, darkTheme = "light")))
      reloadFromWatcher(manager)

      awaitValue(themeName(manager))(_ == "light").unsafeRunSync() shouldBe "light"
    }

  it should "follow the new slot on the next focus regain, as before" in {
    val path    = configFile
    val system  = os(OsAppearance.Dark)
    val manager = editorOn(path, system)
    manager.followSystemAppearance.unsafeRunSync()

    editFile(path, followingDark.withThemeFollowConfig(ThemeFollowConfig(followSystem = true, darkTheme = "light")))
    reloadFromWatcher(manager)
    manager.followSystemAppearance.unsafeRunSync()

    awaitValue(themeName(manager))(_ == "light").unsafeRunSync() shouldBe "light"
  }

  "A config reload that turns follow_system off" should "keep the theme and stop asking the OS" in {
    val path    = configFile
    val system  = os(OsAppearance.Dark)
    val manager = editorOn(path, system)
    manager.followSystemAppearance.unsafeRunSync()
    val readsBefore = system.detections.unsafeRunSync()

    editFile(path, AppConfig.default.withThemeFollowConfig(ThemeFollowConfig(followSystem = false)))
    reloadFromWatcher(manager)
    system.set(OsAppearance.Light).unsafeRunSync()
    manager.followSystemAppearance.unsafeRunSync()

    themeName(manager).unsafeRunSync() shouldBe "dark"
    system.detections.unsafeRunSync() shouldBe readsBefore
  }

  "A config reload that leaves theme.* alone" should "not ask the OS again" in {
    val path    = configFile
    val system  = os(OsAppearance.Dark)
    val manager = editorOn(path, system)
    manager.followSystemAppearance.unsafeRunSync()
    val readsBefore = system.detections.unsafeRunSync()

    editFile(path, followingDark.withWheelScrollLines(7))
    reloadFromWatcher(manager)

    system.detections.unsafeRunSync() shouldBe readsBefore
  }
