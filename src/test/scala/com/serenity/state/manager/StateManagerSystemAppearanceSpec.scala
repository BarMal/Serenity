package com.serenity.state.manager

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import cats.syntax.all.*
import com.serenity.TestTemp
import com.serenity.command.{Command, CommandIntent, ThemeIntent}
import com.serenity.config.{AppConfig, AppearanceSlot, ThemeFollowConfig}
import com.serenity.rope.Balance
import com.serenity.testkit.AwaitCondition.awaitValue
import com.serenity.ui.theme.appearance.{OsAppearance, OsAppearanceDetector}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory

/** A window focus regain re-reads the OS appearance and, only while `theme.follow_system` is on, switches theme. */
class StateManagerSystemAppearanceSpec extends AnyFlatSpec with Matchers:

  given Balance                                  = Balance.default
  given org.typelevel.log4cats.LoggerFactory[IO] = Slf4jFactory.create[IO]

  private val light = "default-light"
  private val dark  = "default-dark"

  final private class Appearance(current: Ref[IO, OsAppearance], reads: Ref[IO, Int]) extends OsAppearanceDetector:
    def detect: IO[OsAppearance]          = reads.update(_ + 1) >> current.get
    def set(next: OsAppearance): IO[Unit] = current.set(next)
    def detections: IO[Int]               = reads.get

  private def appearance(initial: OsAppearance): IO[Appearance] =
    (Ref.of[IO, OsAppearance](initial), Ref.of[IO, Int](0)).mapN(new Appearance(_, _))

  private def managerWith(followSystem: Boolean, detector: OsAppearanceDetector): IO[StateManager] =
    IO.blocking(TestTemp.directory("system-appearance-state-manager")).flatMap { root =>
      StateManager(
        org.typelevel.log4cats.noop.NoOpLogger.impl[IO],
        sessionRootOverride = Some(root),
        initialConfig = AppConfig.default.withThemeFollowConfig(
          ThemeFollowConfig(
            followSystem = followSystem,
            lightTheme = light,
            darkTheme = dark,
            highContrastTheme = dark
          )
        ),
        appearanceDetector = detector
      )
    }

  private def themeName(manager: StateManager): IO[String] = manager.getCurrentState.map(_.persisted.theme.name)

  private def following(manager: StateManager): IO[Boolean] =
    manager.getCurrentState.map(_.persisted.config.themeFollowConfig.followSystem)

  private def toggleTheme(manager: StateManager): IO[Unit] =
    manager.executeCommand(
      Command.typed("toggle-theme", "Toggles the theme.", CommandIntent.Theme(ThemeIntent.ToggleTheme))
    )

  "A focus regain with follow_system on" should "switch to the light theme once the OS turns light" in {
    val program =
      for
        os      <- appearance(OsAppearance.Dark)
        manager <- managerWith(followSystem = true, os)
        _       <- manager.followSystemAppearance
        _       <- awaitValue(themeName(manager))(_ == dark)
        _       <- os.set(OsAppearance.Light)
        _       <- manager.followSystemAppearance
        landed  <- awaitValue(themeName(manager))(_ == light)
      yield landed

    program.unsafeRunSync() shouldBe light
  }

  it should "switch to the high-contrast theme when the OS asks for high contrast" in {
    val program =
      for
        os      <- appearance(OsAppearance.Light)
        manager <- managerWith(followSystem = true, os)
        _       <- manager.followSystemAppearance
        _       <- awaitValue(themeName(manager))(_ == light)
        _       <- os.set(OsAppearance.HighContrast)
        _       <- manager.followSystemAppearance
        landed  <- awaitValue(themeName(manager))(_ == dark)
      yield landed

    program.unsafeRunSync() shouldBe dark
  }

  it should "keep the current theme when the appearance is Unknown" in {
    val program =
      for
        os      <- appearance(OsAppearance.Light)
        manager <- managerWith(followSystem = true, os)
        _       <- manager.followSystemAppearance
        _       <- awaitValue(themeName(manager))(_ == light)
        _       <- os.set(OsAppearance.Unknown)
        _       <- manager.followSystemAppearance
        after   <- themeName(manager)
      yield after

    program.unsafeRunSync() shouldBe light
  }

  it should "survive a detector that fails" in {
    val failing = new OsAppearanceDetector:
      def detect: IO[OsAppearance] = IO.raiseError(new IllegalStateException("boom"))

    val program =
      for
        manager <- managerWith(followSystem = true, failing)
        before  <- themeName(manager)
        _       <- manager.followSystemAppearance
        after   <- themeName(manager)
      yield (before, after)

    val (before, after) = program.unsafeRunSync()
    after shouldBe before
  }

  "A focus regain with follow_system off" should "leave the theme alone and not even query the OS" in {
    val program =
      for
        os      <- appearance(OsAppearance.Light)
        manager <- managerWith(followSystem = false, os)
        before  <- themeName(manager)
        _       <- manager.followSystemAppearance
        after   <- themeName(manager)
        reads   <- os.detections
      yield (before, after, reads)

    val (before, after, reads) = program.unsafeRunSync()
    before should not be light
    after shouldBe before
    reads shouldBe 0
  }

  "ToggleTheme" should "turn follow_system off, so the next focus regain does not undo the toggle" in {
    val program =
      for
        os         <- appearance(OsAppearance.Dark)
        manager    <- managerWith(followSystem = true, os)
        _          <- manager.followSystemAppearance
        _          <- awaitValue(themeName(manager))(_ == dark)
        _          <- toggleTheme(manager)
        toggled    <- awaitValue(themeName(manager))(_ == light)
        stillOn    <- following(manager)
        _          <- manager.followSystemAppearance
        afterFocus <- themeName(manager)
      yield (toggled, stillOn, afterFocus)

    val (toggled, stillOn, afterFocus) = program.unsafeRunSync()
    toggled shouldBe light
    stillOn shouldBe false
    afterFocus shouldBe light
  }

  it should "leave follow_system alone when it was already off" in {
    val program =
      for
        manager <- managerWith(followSystem = false, OsAppearanceDetector.fixed(OsAppearance.Dark))
        _       <- toggleTheme(manager)
        on      <- following(manager)
      yield on

    program.unsafeRunSync() shouldBe false
  }

  private def setting(intent: ThemeIntent): Command =
    Command.typed("theme-follow-setting", "Sets how the theme follows the OS.", CommandIntent.Theme(intent))

  private def followConfig(manager: StateManager): IO[ThemeFollowConfig] =
    manager.getCurrentState.map(_.persisted.config.themeFollowConfig)

  "SetFollowSystem(true)" should "turn following on and apply the OS theme without waiting for a focus regain" in {
    val program =
      for
        os      <- appearance(OsAppearance.Light)
        manager <- managerWith(followSystem = false, os)
        _       <- manager.executeCommand(setting(ThemeIntent.SetFollowSystem(true)))
        landed  <- awaitValue(themeName(manager))(_ == light)
        on      <- following(manager)
      yield (landed, on)

    program.unsafeRunSync() shouldBe ((light, true))
  }

  "SetFollowSystem(false)" should "turn following off and leave the theme where it is" in {
    val program =
      for
        os      <- appearance(OsAppearance.Dark)
        manager <- managerWith(followSystem = true, os)
        _       <- manager.followSystemAppearance
        _       <- manager.executeCommand(setting(ThemeIntent.SetFollowSystem(false)))
        _       <- os.set(OsAppearance.Light)
        _       <- manager.followSystemAppearance
        after   <- themeName(manager)
        on      <- following(manager)
      yield (after, on)

    program.unsafeRunSync() shouldBe ((dark, false))
  }

  "SetFollowSystemTheme" should "set the theme for that appearance and apply it when the OS is in it" in {
    val program =
      for
        os      <- appearance(OsAppearance.Dark)
        manager <- managerWith(followSystem = true, os)
        _       <- manager.followSystemAppearance
        _       <- manager.executeCommand(setting(ThemeIntent.SetFollowSystemTheme(AppearanceSlot.Dark, light)))
        landed  <- awaitValue(themeName(manager))(_ == light)
        config  <- followConfig(manager)
      yield (landed, config.darkTheme, config.lightTheme)

    program.unsafeRunSync() shouldBe ((light, light, light))
  }

  it should "only record the theme while following is off, without asking the OS" in {
    val program =
      for
        os      <- appearance(OsAppearance.Dark)
        manager <- managerWith(followSystem = false, os)
        before  <- themeName(manager)
        _       <- manager.executeCommand(setting(ThemeIntent.SetFollowSystemTheme(AppearanceSlot.Dark, "paper")))
        after   <- themeName(manager)
        config  <- followConfig(manager)
        reads   <- os.detections
      yield (before, after, config.darkTheme, reads)

    val (before, after, darkTheme, reads) = program.unsafeRunSync()
    after shouldBe before
    darkTheme shouldBe "paper"
    reads shouldBe 0
  }
