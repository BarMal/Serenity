package com.serenity.state.manager

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import com.serenity.TestTemp
import com.serenity.command.{Command, CommandIntent, ThemeCommands, ThemeIntent}
import com.serenity.config.{AppConfig, ThemeFollowConfig}
import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.testkit.AwaitCondition.awaitValue
import com.serenity.ui.theme.appearance.{OsAppearance, OsAppearanceDetector}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory

/** Choosing a theme by hand ends `theme.follow_system`; looking at one in a preview does not. The command runner's
  * settings rows and the theme picker both preview a highlighted theme and commit it on Enter (#2131).
  */
class ThemeFollowManualPickSpec extends AnyFlatSpec with Matchers:

  given Balance                                  = Balance.default
  given org.typelevel.log4cats.LoggerFactory[IO] = Slf4jFactory.create[IO]

  private def followingManager(
    detector: OsAppearanceDetector = OsAppearanceDetector.fixed(OsAppearance.Dark)
  ): StateManager =
    val config = AppConfig.default.withThemeFollowConfig(ThemeFollowConfig(followSystem = true))
    val manager = IO
      .blocking(TestTemp.directory("theme-follow-manual-pick"))
      .flatMap(root =>
        StateManager(
          org.typelevel.log4cats.noop.NoOpLogger.impl[IO],
          sessionRootOverride = Some(root),
          initialConfig = config,
          appearanceDetector = detector
        )
      )
      .unsafeRunSync()
    manager.followSystemAppearance.unsafeRunSync()
    manager

  private def themeName(manager: StateManager): String =
    manager.getCurrentState.unsafeRunSync().persisted.theme.name

  private def following(manager: StateManager): Boolean =
    manager.getCurrentState.unsafeRunSync().persisted.config.themeFollowConfig.followSystem

  private def send(manager: StateManager, events: Event*): Unit =
    events.foreach(manager.applyEvent(_).unsafeRunSync())

  private def runner(manager: StateManager): com.serenity.command.CommandRunner =
    manager.getCurrentState
      .unsafeRunSync()
      .commandRunnerSurface
      .flatMap(_.content match
        case SurfaceContent.CommandPalette(runner) => Some(runner)
        case _                                     => None)
      .getOrElse(fail("Expected the command runner to be open"))

  private def moveRootTo(manager: StateManager, id: String): Unit =
    val current = runner(manager)
    val items   = current.visibleItems
    val target  = items.indexWhere(_.id == id)
    if target < 0 then fail(s"Expected root item $id")
    (1 to (target - current.selectedIndex + items.length) % items.length).foreach(_ => send(manager, MoveDown))

  private def moveSubmenuTo(manager: StateManager, id: String): Unit =
    val current = runner(manager)
    val items   = current.focusedSubmenuItems
    val target  = items.indexWhere(_.id == id)
    if target < 0 then fail(s"Expected settings item $id")
    (1 to (target - current.settingsSurfaceSelectedIndex + items.length) % items.length)
      .foreach(_ => send(manager, MoveDown))

  /** The Theme group of the settings palette, open on its first theme. */
  private def openThemeRow(manager: StateManager): Unit =
    send(manager, ToggleCommandRunner, Enter)
    moveRootTo(manager, "settings-look")
    send(manager, Enter)
    moveSubmenuTo(manager, "theme")
    send(manager, Enter)

  private def openThemePicker(manager: StateManager): Unit =
    manager
      .executeCommand(
        Command
          .typed("open-theme-chooser", "Open the theme chooser.", CommandIntent.Theme(ThemeIntent.OpenThemeChooser))
      )
      .unsafeRunSync()
    awaitValue(manager.getCurrentState.map(_.modalSurface.isDefined))(identity).unsafeRunSync(): Unit

  "Previewing a theme in the settings palette" should "leave theme.follow_system on" in {
    val manager = followingManager()
    val before  = themeName(manager)

    openThemeRow(manager)
    send(manager, MoveDown)
    awaitValue(IO(themeName(manager)))(_ != before).unsafeRunSync()

    following(manager) shouldBe true
  }

  "Accepting a previewed theme in the settings palette" should "apply it and turn theme.follow_system off" in {
    val manager = followingManager()
    val before  = themeName(manager)

    openThemeRow(manager)
    send(manager, MoveDown)
    val previewed = awaitValue(IO(themeName(manager)))(_ != before).unsafeRunSync()
    send(manager, Enter)
    manager.runtimeLifecycle.awaitEffects.unsafeRunSync()

    following(manager) shouldBe false
    themeName(manager) shouldBe previewed
  }

  "Abandoning a previewed theme in the settings palette" should "put the theme back and keep following" in {
    val manager = followingManager()
    val before  = themeName(manager)

    openThemeRow(manager)
    send(manager, MoveDown)
    awaitValue(IO(themeName(manager)))(_ != before).unsafeRunSync()
    send(manager, Escape, Escape, Escape, Escape)
    manager.runtimeLifecycle.awaitEffects.unsafeRunSync()

    following(manager) shouldBe true
    themeName(manager) shouldBe before
  }

  "Highlighting a theme in the theme picker" should "leave theme.follow_system on" in {
    val manager = followingManager()
    val before  = themeName(manager)

    openThemePicker(manager)
    send(manager, ModalNavigate(Direction.Down))
    awaitValue(IO(themeName(manager)))(_ != before).unsafeRunSync()

    following(manager) shouldBe true
  }

  "Picking a theme in the theme picker" should "apply it and turn theme.follow_system off" in {
    val manager = followingManager()
    val before  = themeName(manager)

    openThemePicker(manager)
    send(manager, ModalNavigate(Direction.Down))
    val highlighted = awaitValue(IO(themeName(manager)))(_ != before).unsafeRunSync()
    send(manager, ModalSubmit)
    manager.runtimeLifecycle.awaitEffects.unsafeRunSync()

    following(manager) shouldBe false
    themeName(manager) shouldBe highlighted
  }

  "Dismissing the theme picker" should "restore the theme and keep following" in {
    val manager = followingManager()
    val before  = themeName(manager)

    openThemePicker(manager)
    send(manager, ModalNavigate(Direction.Down))
    awaitValue(IO(themeName(manager)))(_ != before).unsafeRunSync()
    send(manager, ModalDismiss)
    awaitValue(IO(themeName(manager)))(_ == before).unsafeRunSync()

    following(manager) shouldBe true
  }

  "Applying a named theme command" should "turn theme.follow_system off" in {
    val manager = followingManager()

    manager.executeCommand(ThemeCommands.applyTheme("light")).unsafeRunSync()
    awaitValue(IO(themeName(manager)))(_ == "light").unsafeRunSync()

    following(manager) shouldBe false
  }

  "A focus regain while a theme is being previewed" should "leave the preview alone" in {
    val os = Ref.unsafe[IO, OsAppearance](OsAppearance.Dark)
    val manager = followingManager(
      new OsAppearanceDetector:
        def detect: IO[OsAppearance] = os.get
    )
    val before = themeName(manager)

    openThemeRow(manager)
    send(manager, MoveDown)
    val previewed = awaitValue(IO(themeName(manager)))(_ != before).unsafeRunSync()
    os.set(OsAppearance.Light).unsafeRunSync()
    manager.followSystemAppearance.unsafeRunSync()

    themeName(manager) shouldBe previewed
  }

  it should "be followed once the preview is abandoned" in {
    val os = Ref.unsafe[IO, OsAppearance](OsAppearance.Dark)
    val manager = followingManager(
      new OsAppearanceDetector:
        def detect: IO[OsAppearance] = os.get
    )

    openThemeRow(manager)
    send(manager, MoveDown)
    send(manager, Escape, Escape, Escape, Escape)
    os.set(OsAppearance.Light).unsafeRunSync()
    manager.followSystemAppearance.unsafeRunSync()

    themeName(manager) shouldBe "light"
  }
