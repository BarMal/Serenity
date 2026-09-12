package com.serenity

import java.awt.Color

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.animation.{AnimationConfig, AnimationOwner, TransitionKind}
import com.serenity.command.{
  Command,
  CommandCategory,
  CommandIntent,
  GeneralSettingsIntent,
  MotionIntent,
  PanelChromeIntent,
  SettingsIntent
}
import com.serenity.config.*
import com.serenity.config.AppConfigMotionOps.*
import com.serenity.keystroke.events.NextTab
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.ui.layout.ViewportSize
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class StateManagerTransitionSpeedAndKindSpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private def createStateManager(initialConfig: com.serenity.config.AppConfig = AppConfig.default): StateManager =
    val logger = LoggerFactory[IO].getLogger(using LoggerName("StateManagerTransitionSpeedAndKindSpec"))
    StateManager(logger, initialConfig = initialConfig).unsafeRunSync()

  "StateManager transition and appearance setting commands" should "update the editor text transition speed scale config" in {
    val stateManager = createStateManager()

    stateManager.commandExecutor
      .executeCommand(
        Command.typed(
          "editor-text-speed-scale",
          "Set editor text speed scale",
          CommandIntent.Settings(SettingsIntent.Motion(MotionIntent.SetEditorTextTransitionSpeedScale(0.5))),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()

    stateManager.getCurrentState
      .unsafeRunSync()
      .persisted
      .config
      .surfaceConfig
      .editorTextTransitionSpeedScale shouldBe Some(0.5)
  }

  it should "retain editor text animations while starting a pane UI transition" in {
    val stateManager = createStateManager()
    stateManager
      .updateState(state =>
        state.copy(
          persisted = state.persisted.copy(config = state.persisted.config.withMotionPreset(MotionPreset.Smooth)),
          runtime = state.runtime.copy(viewportSize = Some(ViewportSize(80, 24)))
        )
      )
      .unsafeRunSync()

    val firstBufferId = stateManager.getCurrentState.unsafeRunSync().persisted.bufferOrder.head
    stateManager.bufferManager.updateBuffer(firstBufferId, "First").unsafeRunSync()
    val secondBufferId = stateManager.bufferManager.createBuffer("Second", None).unsafeRunSync()
    stateManager
      .updateBufferAnimations { animations =>
        val current = animations.getOrElse(secondBufferId, com.serenity.animation.AnimationState.empty)
        animations.updated(
          secondBufferId,
          current.addCharacterAnimation('z', 0, 0, Color.BLACK, Color.WHITE, 5)
        )
      }
      .unsafeRunSync()

    stateManager.applyEvent(NextTab).unsafeRunSync()

    val animations = stateManager.getBufferAnimations
      .unsafeRunSync()
      .getOrElse(secondBufferId, com.serenity.animation.AnimationState.empty)
    val owners = animations.animations.values.map(_.owner).toSet
    owners should contain allOf (AnimationOwner.EditorText, AnimationOwner.UiTransitions)
    animations.getCell(0, 0).map(_.owner) shouldBe Some(AnimationOwner.EditorText)
  }

  it should "update the command runner transition speed scale config" in {
    val stateManager = createStateManager()

    stateManager.commandExecutor
      .executeCommand(
        Command.typed(
          "command-runner-speed-scale",
          "Set command runner speed scale",
          CommandIntent.Settings(SettingsIntent.Motion(MotionIntent.SetCommandRunnerTransitionSpeedScale(2.25))),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()

    stateManager.getCurrentState
      .unsafeRunSync()
      .persisted
      .config
      .surfaceConfig
      .commandRunnerTransitionSpeedScale shouldBe Some(2.25)
  }

  it should "update the UI transition speed scale config" in {
    val stateManager = createStateManager()

    stateManager.commandExecutor
      .executeCommand(
        Command.typed(
          "ui-speed-scale",
          "Set UI speed scale",
          CommandIntent.Settings(SettingsIntent.Motion(MotionIntent.SetUiTransitionSpeedScale(1.25))),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()

    stateManager.getCurrentState.unsafeRunSync().persisted.config.surfaceConfig.uiTransitionSpeedScale shouldBe Some(
      1.25
    )
  }

  it should "update the cursor transition speed scale config" in {
    val stateManager = createStateManager()

    stateManager.commandExecutor
      .executeCommand(
        Command.typed(
          "cursor-speed-scale",
          "Set cursor speed scale",
          CommandIntent.Settings(SettingsIntent.Motion(MotionIntent.SetCursorTransitionSpeedScale(0.75))),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()

    stateManager.getCurrentState
      .unsafeRunSync()
      .persisted
      .config
      .surfaceConfig
      .cursorTransitionSpeedScale shouldBe Some(0.75)
  }

  it should "update the editor text transition kind config" in {
    val stateManager = createStateManager()

    stateManager.commandExecutor
      .executeCommand(
        Command.typed(
          "editor-text-transition",
          "Set editor text transition",
          CommandIntent.Settings(
            SettingsIntent.Motion(MotionIntent.SetEditorInsertionTransitionKind(TransitionKind.TypedText))
          ),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()

    stateManager.getCurrentState
      .unsafeRunSync()
      .persisted
      .config
      .surfaceConfig
      .editorInsertionTransitionKind shouldBe TransitionKind.TypedText
  }

  it should "mark the motion preset custom when an explicit transition kind is edited" in {
    val stateManager = createStateManager()
    stateManager
      .updateState(state =>
        state
          .copy(persisted = state.persisted.copy(config = state.persisted.config.withMotionPreset(MotionPreset.Smooth)))
      )
      .unsafeRunSync()

    stateManager.commandExecutor
      .executeCommand(
        Command.typed(
          "editor-text-transition",
          "Set editor text transition",
          CommandIntent.Settings(
            SettingsIntent.Motion(MotionIntent.SetEditorInsertionTransitionKind(TransitionKind.TypedText))
          ),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()

    stateManager.getCurrentState
      .unsafeRunSync()
      .persisted
      .config
      .surfaceConfig
      .motionPreset shouldBe MotionPreset.Custom
  }

  it should "update the panel open transition kind config" in {
    val stateManager = createStateManager()

    stateManager.commandExecutor
      .executeCommand(
        Command.typed(
          "panel-open-transition",
          "Set panel open transition",
          CommandIntent.Settings(
            SettingsIntent.Motion(MotionIntent.SetPanelOpenTransitionKind(TransitionKind.OutlineThenContent))
          ),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()

    stateManager.getCurrentState.unsafeRunSync().persisted.config.surfaceConfig.panelOpenTransitionKind shouldBe Some(
      TransitionKind.OutlineThenContent
    )
  }

  it should "update the panel close transition kind config" in {
    val stateManager = createStateManager()

    stateManager.commandExecutor
      .executeCommand(
        Command.typed(
          "panel-close-transition",
          "Set panel close transition",
          CommandIntent.Settings(
            SettingsIntent.Motion(MotionIntent.SetPanelCloseTransitionKind(TransitionKind.Disabled))
          ),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()

    stateManager.getCurrentState.unsafeRunSync().persisted.config.surfaceConfig.panelCloseTransitionKind shouldBe Some(
      TransitionKind.Disabled
    )
  }

  it should "update the command runner transition kind config" in {
    val stateManager = createStateManager()

    stateManager.commandExecutor
      .executeCommand(
        Command.typed(
          "command-runner-transition",
          "Set command runner transition",
          CommandIntent.Settings(
            SettingsIntent.Motion(MotionIntent.SetCommandRunnerTransitionKind(TransitionKind.OutlineThenContent))
          ),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()

    stateManager.getCurrentState
      .unsafeRunSync()
      .persisted
      .config
      .surfaceConfig
      .commandRunnerTransitionKind shouldBe Some(
      TransitionKind.OutlineThenContent
    )
  }

  it should "mark the motion preset custom when command runner transition kind is edited" in {
    val stateManager = createStateManager()
    stateManager
      .updateState(state =>
        state
          .copy(persisted = state.persisted.copy(config = state.persisted.config.withMotionPreset(MotionPreset.Smooth)))
      )
      .unsafeRunSync()

    stateManager.commandExecutor
      .executeCommand(
        Command.typed(
          "command-runner-transition",
          "Set command runner transition",
          CommandIntent.Settings(
            SettingsIntent.Motion(MotionIntent.SetCommandRunnerTransitionKind(TransitionKind.DirectionalSweep))
          ),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()

    val config = stateManager.getCurrentState.unsafeRunSync().persisted.config
    config.surfaceConfig.commandRunnerTransitionKind shouldBe Some(TransitionKind.DirectionalSweep)
    config.surfaceConfig.motionPreset shouldBe MotionPreset.Custom
  }

  it should "update the command runner fade animation config" in {
    val stateManager = createStateManager()

    stateManager.commandExecutor
      .executeCommand(
        Command.typed(
          "command-runner-fade",
          "Set command runner fade",
          CommandIntent.Settings(SettingsIntent.Motion(MotionIntent.SetCommandRunnerAnimation(AnimationConfig.subtle))),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()

    stateManager.getCurrentState
      .unsafeRunSync()
      .persisted
      .config
      .surfaceConfig
      .commandRunnerAnimation shouldBe AnimationConfig.subtle
  }

  it should "mark the motion preset custom when command runner fade is edited" in {
    val stateManager = createStateManager()
    stateManager
      .updateState(state =>
        state
          .copy(persisted = state.persisted.copy(config = state.persisted.config.withMotionPreset(MotionPreset.Smooth)))
      )
      .unsafeRunSync()

    stateManager.commandExecutor
      .executeCommand(
        Command.typed(
          "command-runner-fade",
          "Set command runner fade",
          CommandIntent.Settings(SettingsIntent.Motion(MotionIntent.SetCommandRunnerAnimation(None))),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()

    val config = stateManager.getCurrentState.unsafeRunSync().persisted.config
    config.surfaceConfig.commandRunnerAnimation shouldBe None
    config.surfaceConfig.motionPreset shouldBe MotionPreset.Custom
  }

  it should "update the UI animation config" in {
    val stateManager = createStateManager()

    stateManager.commandExecutor
      .executeCommand(
        Command.typed(
          "ui-animation",
          "Set UI animation",
          CommandIntent.Settings(SettingsIntent.Motion(MotionIntent.SetUiAnimation(AnimationConfig.subtle))),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()

    stateManager.getCurrentState
      .unsafeRunSync()
      .persisted
      .config
      .surfaceConfig
      .uiAnimation shouldBe AnimationConfig.subtle
  }

  it should "update the render FPS target config" in {
    val stateManager = createStateManager()

    stateManager.commandExecutor
      .executeCommand(
        Command.typed(
          "render-fps",
          "Set render FPS target",
          CommandIntent.Settings(
            SettingsIntent.General(GeneralSettingsIntent.SetRenderFpsTarget(RenderFpsTarget.Fps120))
          ),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()

    stateManager.getCurrentState
      .unsafeRunSync()
      .persisted
      .config
      .surfaceConfig
      .renderFpsTarget shouldBe RenderFpsTarget.Fps120
  }

  it should "update the UI element gap config" in {
    val stateManager = createStateManager()

    stateManager.commandExecutor
      .executeCommand(
        Command.typed(
          "ui-element-gap",
          "Set UI element gap",
          CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetUiElementGap(3))),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()

    stateManager.getCurrentState.unsafeRunSync().persisted.config.uiElementGap shouldBe 3
  }

  it should "update the UI corner radius config" in {
    val stateManager = createStateManager()

    stateManager.commandExecutor
      .executeCommand(
        Command.typed(
          "ui-corner-radius",
          "Set UI corner radius",
          CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetUiCornerRadiusPx(14))),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()

    stateManager.getCurrentState.unsafeRunSync().persisted.config.uiCornerRadiusPx shouldBe 14
  }

  it should "update the UI outline thickness config" in {
    val stateManager = createStateManager()

    stateManager.commandExecutor
      .executeCommand(
        Command.typed(
          "ui-outline-thickness",
          "Set UI outline thickness",
          CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetUiOutlineThicknessPx(4))),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()

    stateManager.getCurrentState.unsafeRunSync().persisted.config.uiOutlineThicknessPx shouldBe 4
  }
