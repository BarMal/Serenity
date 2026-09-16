package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.command.*
import com.serenity.config.{AppConfig, AppMode}
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

/** Command wiring for switching the app between code and prose mode (issue #1297). */
class StateManagerAppModeSpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private def createStateManager(): StateManager =
    val logger = LoggerFactory[IO].getLogger(using LoggerName("StateManagerAppModeSpec"))
    StateManager(logger).unsafeRunSync()

  "StateManager" should "start in code mode" in {
    val stateManager = createStateManager()

    stateManager.getCurrentState.unsafeRunSync().persisted.config.appMode shouldBe AppMode.Code
  }

  it should "switch to prose mode via the app-mode-prose command" in {
    val stateManager = createStateManager()

    stateManager.commandExecutor
      .executeCommand(
        Command.typed(
          "app-mode-prose",
          "Switch to prose mode",
          CommandIntent.View(ViewIntent.SetAppMode(AppMode.Prose)),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()

    stateManager.getCurrentState.unsafeRunSync().persisted.config.appMode shouldBe AppMode.Prose
  }

  it should "switch back to code mode via the app-mode-code command" in {
    val stateManager = createStateManager()

    stateManager.commandExecutor
      .executeCommand(
        Command.typed(
          "app-mode-prose",
          "Switch to prose mode",
          CommandIntent.View(ViewIntent.SetAppMode(AppMode.Prose)),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()
    stateManager.commandExecutor
      .executeCommand(
        Command.typed(
          "app-mode-code",
          "Switch to code mode",
          CommandIntent.View(ViewIntent.SetAppMode(AppMode.Code)),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()

    stateManager.getCurrentState.unsafeRunSync().persisted.config.appMode shouldBe AppMode.Code
  }

  // issue #1047: one canonical entry per intent -- the app mode is the "app-mode" settings row, not two commands.
  "CommandRegistry" should "not register app-mode commands alongside the app-mode setting" in {
    val registry = com.serenity.command.CommandRegistry.default

    registry.findCommand("app-mode-code") shouldBe None
    registry.findCommand("app-mode-prose") shouldBe None
    val runner = CommandRunner.empty.activate(registry, AppConfig.default)
    def leaves(items: List[CommandSurfaceItem]): List[CommandSurfaceItem] =
      items.flatMap {
        case group: CommandSurfaceItem.GroupItem => leaves(group.children)
        case leaf                                => List(leaf)
      }
    val appMode = leaves(runner.settingsGroups)
      .collectFirst { case item: CommandSurfaceItem.OptionItem if item.id == "app-mode" => item }
      .getOrElse(fail("missing app-mode setting"))
    appMode.options.map(_.intent) shouldBe List(
      CommandIntent.View(ViewIntent.SetAppMode(AppMode.Code)),
      CommandIntent.View(ViewIntent.SetAppMode(AppMode.Prose))
    )
  }
