package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.command.{Command, CommandCategory, CommandIntent}
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class StateManagerElementTransitionSettingsSpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private def createStateManager(): StateManager =
    val logger = LoggerFactory[IO].getLogger(using LoggerName("StateManagerElementTransitionSettingsSpec"))
    StateManager(logger).unsafeRunSync()

  "StateManager element transition setting commands" should "update the element transition speed scale config" in {
    val stateManager = createStateManager()

    stateManager
      .executeCommand(
        Command.typed(
          "element-transition-speed-scale",
          "Set element transition speed scale",
          CommandIntent.SetElementTransitionSpeedScale(2.25),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()

    stateManager.getCurrentState.unsafeRunSync().config.elementTransitionSpeedScale shouldBe 2.25
  }
