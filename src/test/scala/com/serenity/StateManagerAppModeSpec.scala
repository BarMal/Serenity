package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.command.{Command, CommandCategory, CommandIntent, ViewIntent}
import com.serenity.config.AppMode
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

    stateManager
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

    stateManager
      .executeCommand(
        Command.typed(
          "app-mode-prose",
          "Switch to prose mode",
          CommandIntent.View(ViewIntent.SetAppMode(AppMode.Prose)),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()
    stateManager
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

  "CommandRegistry" should "register the app-mode-code and app-mode-prose commands" in {
    val registry = com.serenity.command.CommandRegistry.default

    registry.findCommand("app-mode-code").map(_.intent) shouldBe Some(
      CommandIntent.View(ViewIntent.SetAppMode(AppMode.Code))
    )
    registry.findCommand("app-mode-prose").map(_.intent) shouldBe Some(
      CommandIntent.View(ViewIntent.SetAppMode(AppMode.Prose))
    )
  }
