package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.animation.{AnimationConfig, TransitionKind}
import com.serenity.command.{Command, CommandCategory, CommandIntent}
import com.serenity.config.RenderFpsTarget
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

  it should "update the editor text transition speed scale config" in {
    val stateManager = createStateManager()

    stateManager
      .executeCommand(
        Command.typed(
          "editor-text-speed-scale",
          "Set editor text speed scale",
          CommandIntent.SetEditorTextTransitionSpeedScale(0.5),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()

    stateManager.getCurrentState.unsafeRunSync().config.editorTextTransitionSpeedScale shouldBe Some(0.5)
  }

  it should "update the command runner transition speed scale config" in {
    val stateManager = createStateManager()

    stateManager
      .executeCommand(
        Command.typed(
          "command-runner-speed-scale",
          "Set command runner speed scale",
          CommandIntent.SetCommandRunnerTransitionSpeedScale(2.25),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()

    stateManager.getCurrentState.unsafeRunSync().config.commandRunnerTransitionSpeedScale shouldBe Some(2.25)
  }

  it should "update the UI transition speed scale config" in {
    val stateManager = createStateManager()

    stateManager
      .executeCommand(
        Command.typed(
          "ui-speed-scale",
          "Set UI speed scale",
          CommandIntent.SetUiTransitionSpeedScale(1.25),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()

    stateManager.getCurrentState.unsafeRunSync().config.uiTransitionSpeedScale shouldBe Some(1.25)
  }

  it should "update the cursor transition speed scale config" in {
    val stateManager = createStateManager()

    stateManager
      .executeCommand(
        Command.typed(
          "cursor-speed-scale",
          "Set cursor speed scale",
          CommandIntent.SetCursorTransitionSpeedScale(0.75),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()

    stateManager.getCurrentState.unsafeRunSync().config.cursorTransitionSpeedScale shouldBe Some(0.75)
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

  it should "update the panel open transition kind config" in {
    val stateManager = createStateManager()

    stateManager
      .executeCommand(
        Command.typed(
          "panel-open-transition",
          "Set panel open transition",
          CommandIntent.SetPanelOpenTransitionKind(TransitionKind.OutlineThenContent),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()

    stateManager.getCurrentState.unsafeRunSync().config.panelOpenTransitionKind shouldBe Some(
      TransitionKind.OutlineThenContent
    )
  }

  it should "update the panel close transition kind config" in {
    val stateManager = createStateManager()

    stateManager
      .executeCommand(
        Command.typed(
          "panel-close-transition",
          "Set panel close transition",
          CommandIntent.SetPanelCloseTransitionKind(TransitionKind.Disabled),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()

    stateManager.getCurrentState.unsafeRunSync().config.panelCloseTransitionKind shouldBe Some(TransitionKind.Disabled)
  }

  it should "update the command runner fade animation config" in {
    val stateManager = createStateManager()

    stateManager
      .executeCommand(
        Command.typed(
          "command-runner-fade",
          "Set command runner fade",
          CommandIntent.SetCommandRunnerAnimation(AnimationConfig.subtle),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()

    stateManager.getCurrentState.unsafeRunSync().config.commandRunnerAnimation shouldBe AnimationConfig.subtle
  }

  it should "update the UI animation config" in {
    val stateManager = createStateManager()

    stateManager
      .executeCommand(
        Command.typed(
          "ui-animation",
          "Set UI animation",
          CommandIntent.SetUiAnimation(AnimationConfig.subtle),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()

    stateManager.getCurrentState.unsafeRunSync().config.uiAnimation shouldBe AnimationConfig.subtle
  }

  it should "update the render FPS target config" in {
    val stateManager = createStateManager()

    stateManager
      .executeCommand(
        Command.typed(
          "render-fps",
          "Set render FPS target",
          CommandIntent.SetRenderFpsTarget(RenderFpsTarget.Fps120),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()

    stateManager.getCurrentState.unsafeRunSync().config.renderFpsTarget shouldBe RenderFpsTarget.Fps120
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
