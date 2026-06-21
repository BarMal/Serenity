package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.animation.TransitionKind
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

  it should "update the editor text transition kind config" in {
    val stateManager = createStateManager()

    stateManager
      .executeCommand(
        Command.typed(
          "editor-text-transition",
          "Set editor text transition",
          CommandIntent.SetEditorInsertionTransitionKind(TransitionKind.TypedText),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()

    stateManager.getCurrentState.unsafeRunSync().config.editorInsertionTransitionKind shouldBe TransitionKind.TypedText
  }

  it should "update the UI element gap config" in {
    val stateManager = createStateManager()

    stateManager
      .executeCommand(
        Command.typed(
          "ui-element-gap",
          "Set UI element gap",
          CommandIntent.SetUiElementGap(3),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()

    stateManager.getCurrentState.unsafeRunSync().config.uiElementGap shouldBe 3
  }

  it should "update the UI corner radius config" in {
    val stateManager = createStateManager()

    stateManager
      .executeCommand(
        Command.typed(
          "ui-corner-radius",
          "Set UI corner radius",
          CommandIntent.SetUiCornerRadiusPx(14),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()

    stateManager.getCurrentState.unsafeRunSync().config.uiCornerRadiusPx shouldBe 14
  }
