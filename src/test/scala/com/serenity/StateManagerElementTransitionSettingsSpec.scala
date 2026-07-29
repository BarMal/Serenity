package com.serenity

import java.awt.Color
import java.nio.file.Files

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.animation.{AnimationConfig, AnimationOwner, TransitionKind, WindowSitter}
import com.serenity.command.{Command, CommandCategory, CommandIntent}
import com.serenity.config.*
import com.serenity.keystroke.events.NextTab
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.ui.layout.ViewportSize
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class StateManagerElementTransitionSettingsSpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private def createStateManager(initialConfig: com.serenity.config.AppConfig = AppConfig.default): StateManager =
    val logger = LoggerFactory[IO].getLogger(using LoggerName("StateManagerElementTransitionSettingsSpec"))
    StateManager(logger, initialConfig = initialConfig).unsafeRunSync()

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

  it should "update the global motion accessibility override without changing the baseline" in {
    val stateManager = createStateManager()
    stateManager
      .updateState(state => state.copy(config = state.config.withMotionPreset(MotionPreset.Expressive)))
      .unsafeRunSync()

    stateManager
      .executeCommand(
        Command.typed(
          "motion-accessibility",
          "Set motion accessibility",
          CommandIntent.SetMotionAccessibility(MotionAccessibility.Off),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()

    val motion = stateManager.getCurrentState
      .unsafeRunSync()
      .config
      .surfaceConfig
      .motionConfiguration
      .getOrElse(fail("Expected authoritative motion configuration"))
    motion.accessibility shouldBe MotionAccessibility.Off
    motion.baseline shouldBe MotionPreset.Expressive
  }

  it should "settle the window sitter when accessibility disables UI motion" in {
    val stateManager = createStateManager()
    stateManager
      .updateState(state => state.copy(windowSitter = WindowSitter.default.observeTyping(1_000_000_000L)))
      .unsafeRunSync()

    stateManager
      .executeCommand(
        Command.typed(
          "motion-accessibility",
          "Set motion accessibility",
          CommandIntent.SetMotionAccessibility(MotionAccessibility.Off),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()

    val sitter = stateManager.getCurrentState.unsafeRunSync().windowSitter
    sitter.isActive shouldBe false
    sitter.glyph shouldBe "·"
  }

  it should "mark the motion preset custom when an explicit motion speed is edited" in {
    val stateManager = createStateManager()
    stateManager
      .updateState(state => state.copy(config = state.config.withMotionPreset(MotionPreset.Smooth)))
      .unsafeRunSync()

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

    stateManager.getCurrentState.unsafeRunSync().config.motionPreset shouldBe MotionPreset.Custom
  }

  it should "persist custom as the authoritative baseline after a manual motion edit" in {
    val stateManager = createStateManager()
    stateManager
      .updateState(state => state.copy(config = state.config.withMotionPreset(MotionPreset.Smooth)))
      .unsafeRunSync()

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

    val config     = stateManager.getCurrentState.unsafeRunSync().config
    val configFile = Files.createTempFile("serenity-custom-motion-baseline", ".conf")
    Files.writeString(configFile, ConfigManager.configToString(config))

    config.surfaceConfig.motionConfiguration.map(_.baseline) shouldBe Some(MotionPreset.Custom)
    ConfigManager
      .loadConfig(Some(configFile.toString))
      .surfaceConfig
      .effectiveMotionBaseline shouldBe MotionPreset.Custom
  }

  it should "promote custom editor timing into the authoritative family" in {
    val stateManager = createStateManager(AppConfig.default.withMotionPreset(MotionPreset.Custom))

    List(CommandIntent.SetAnimationDuration(375), CommandIntent.SetAnimationSteps(9)).zipWithIndex.foreach {
      case (intent, index) =>
        stateManager
          .executeCommand(
            Command.typed(s"custom-editor-timing-$index", "Set custom editor timing", intent, CommandCategory.Settings)
          )
          .unsafeRunSync()
    }

    val config     = stateManager.getCurrentState.unsafeRunSync().config
    val configFile = Files.createTempFile("serenity-custom-editor-timing", ".conf")
    Files.writeString(configFile, ConfigManager.configToString(config))
    val persistedFamily = ConfigManager
      .loadConfig(Some(configFile.toString))
      .surfaceConfig
      .effectiveMotionConfiguration
      .family(MotionFamily.EditorText)

    config.scaledCharacterAnimation.map(_.durationMs) shouldBe Some(375L)
    config.scaledCharacterAnimation.map(_.steps) shouldBe Some(9)
    persistedFamily.animation.map(_.durationMs) shouldBe Some(375L)
    persistedFamily.animation.map(_.steps) shouldBe Some(9)

    val promoted = AppConfig.default
      .withMotionPreset(MotionPreset.Custom)
      .withEditorTextAnimation(
        Some(
          AnimationConfig.smooth.get
            .copy(steps = 9, totalDuration = scala.concurrent.duration.Duration.fromNanos(375_000_000L))
        )
      )
      .withCustomMotionBaseline

    promoted.surfaceConfig.effectiveMotionConfiguration
      .family(MotionFamily.EditorText)
      .animation
      .map(_.durationMs) shouldBe Some(
      375L
    )
    promoted.surfaceConfig.effectiveMotionConfiguration
      .family(MotionFamily.EditorText)
      .animation
      .map(_.steps) shouldBe Some(9)
  }

  it should "preserve authoritative custom editor timing through UI motion edits" in {
    val configFile = Files.createTempFile("serenity-authoritative-editor-timing", ".conf")
    Files.writeString(
      configFile,
      """ui.motion = custom
        |ui.motion.family.editor_text.animation = custom
        |ui.motion.family.editor_text.animation.duration_ms = 375
        |ui.motion.family.editor_text.animation.steps = 9
        |""".stripMargin
    )
    val updated = ConfigManager
      .loadConfig(Some(configFile.toString))
      .withUiTransitionSpeedScale(Some(1.5))
      .withUiAnimation(AnimationConfig.quick)
      .withCustomMotionBaseline
      .surfaceConfig
      .effectiveMotionConfiguration
      .family(MotionFamily.EditorText)

    updated.animation.map(_.durationMs) shouldBe Some(375L)
    updated.animation.map(_.steps) shouldBe Some(9)
  }

  it should "preserve legacy surface family animations when custom editor timing is promoted" in {
    val configured = AppConfig(
      characterAnimation = AnimationConfig.smooth,
      motionPreset = MotionPreset.Custom,
      commandRunnerAnimation = AnimationConfig.subtle,
      uiAnimation = AnimationConfig.quick,
      panelOpenTransitionKind = Some(TransitionKind.OutlineThenContent),
      panelCloseTransitionKind = Some(TransitionKind.Fade)
    )
    val families = configured
      .withEditorTextAnimation(
        Some(
          AnimationConfig.smooth.get
            .copy(steps = 9, totalDuration = scala.concurrent.duration.Duration.fromNanos(375_000_000L))
        )
      )
      .withCustomMotionBaseline
      .surfaceConfig
      .effectiveMotionConfiguration

    families.family(MotionFamily.CommandSurfaces).animation shouldBe AnimationConfig.subtle
    families.family(MotionFamily.PinnedPanels).animation shouldBe AnimationConfig.quick
    families.family(MotionFamily.UiTransitions).animation shouldBe AnimationConfig.quick
  }

  it should "preserve the accessibility override through manual motion edits" in
    List(
      MotionAccessibility.Off     -> CommandIntent.SetElementTransitionSpeedScale(2.25),
      MotionAccessibility.Reduced -> CommandIntent.SetCommandRunnerTransitionKind(TransitionKind.DirectionalSweep)
    ).foreach {
      case (accessibility, edit) =>
        val stateManager = createStateManager()
        stateManager
          .updateState(state => state.copy(config = state.config.withMotionPreset(MotionPreset.Smooth)))
          .unsafeRunSync()

        stateManager
          .executeCommand(
            Command.typed(
              "motion-accessibility",
              "Set motion accessibility",
              CommandIntent.SetMotionAccessibility(accessibility),
              CommandCategory.Settings
            )
          )
          .unsafeRunSync()
        stateManager
          .executeCommand(Command.typed("motion-edit", "Edit motion", edit, CommandCategory.Settings))
          .unsafeRunSync()

        val config = stateManager.getCurrentState.unsafeRunSync().config
        config.motionPreset shouldBe MotionPreset.Custom
        config.surfaceConfig.motionConfiguration.map(_.accessibility) shouldBe Some(accessibility)
        config.surfaceConfig.effectiveMotionConfiguration.families.values.foreach(_.enabled shouldBe false)
    }

  it should "disable the authoritative editor text family when animation mode is none" in {
    val stateManager = createStateManager()
    stateManager
      .updateState(state =>
        state.copy(
          config =
            com.serenity.config.AppConfig.withTestAnimations.withMotionAccessibility(MotionAccessibility.Standard)
        )
      )
      .unsafeRunSync()

    stateManager
      .executeCommand(
        Command.typed(
          "animation-mode",
          "Set animation mode",
          CommandIntent.SetAnimationMode(com.serenity.command.AnimationMode.None),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()

    val config = stateManager.getCurrentState.unsafeRunSync().config
    config.scaledCharacterAnimation shouldBe None
    config.surfaceConfig.effectiveMotionConfiguration
      .family(com.serenity.config.MotionFamily.EditorText)
      .enabled shouldBe false
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

  it should "retain editor text animations while starting a pane UI transition" in {
    val stateManager = createStateManager()
    stateManager
      .updateState(state =>
        state.copy(
          config = state.config.withMotionPreset(MotionPreset.Smooth),
          viewportSize = Some(ViewportSize(80, 24))
        )
      )
      .unsafeRunSync()

    val firstBufferId = stateManager.getCurrentState.unsafeRunSync().bufferOrder.head
    stateManager.updateBuffer(firstBufferId, "First").unsafeRunSync()
    val secondBufferId = stateManager.createBuffer("Second").unsafeRunSync()
    stateManager
      .updateState { state =>
        val buffer = state.buffers(secondBufferId)
        state.copy(buffers =
          state.buffers.updated(
            secondBufferId,
            buffer
              .copy(animations = buffer.animations.addCharacterAnimation('z', 0, 0, Color.BLACK, Color.WHITE, 5))
          )
        )
      }
      .unsafeRunSync()

    stateManager.applyEvent(NextTab).unsafeRunSync()

    val animations = stateManager.getCurrentState
      .unsafeRunSync()
      .buffers(secondBufferId)
      .animations
    val owners = animations.animations.values.map(_.owner).toSet
    owners should contain allOf (AnimationOwner.EditorText, AnimationOwner.UiTransitions)
    animations.getCell(0, 0).map(_.owner) shouldBe Some(AnimationOwner.EditorText)
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

  it should "mark the motion preset custom when an explicit transition kind is edited" in {
    val stateManager = createStateManager()
    stateManager
      .updateState(state => state.copy(config = state.config.withMotionPreset(MotionPreset.Smooth)))
      .unsafeRunSync()

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

    stateManager.getCurrentState.unsafeRunSync().config.motionPreset shouldBe MotionPreset.Custom
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

  it should "update the command runner transition kind config" in {
    val stateManager = createStateManager()

    stateManager
      .executeCommand(
        Command.typed(
          "command-runner-transition",
          "Set command runner transition",
          CommandIntent.SetCommandRunnerTransitionKind(TransitionKind.OutlineThenContent),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()

    stateManager.getCurrentState.unsafeRunSync().config.commandRunnerTransitionKind shouldBe Some(
      TransitionKind.OutlineThenContent
    )
  }

  it should "mark the motion preset custom when command runner transition kind is edited" in {
    val stateManager = createStateManager()
    stateManager
      .updateState(state => state.copy(config = state.config.withMotionPreset(MotionPreset.Smooth)))
      .unsafeRunSync()

    stateManager
      .executeCommand(
        Command.typed(
          "command-runner-transition",
          "Set command runner transition",
          CommandIntent.SetCommandRunnerTransitionKind(TransitionKind.DirectionalSweep),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()

    val config = stateManager.getCurrentState.unsafeRunSync().config
    config.commandRunnerTransitionKind shouldBe Some(TransitionKind.DirectionalSweep)
    config.motionPreset shouldBe MotionPreset.Custom
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

  it should "mark the motion preset custom when command runner fade is edited" in {
    val stateManager = createStateManager()
    stateManager
      .updateState(state => state.copy(config = state.config.withMotionPreset(MotionPreset.Smooth)))
      .unsafeRunSync()

    stateManager
      .executeCommand(
        Command.typed(
          "command-runner-fade",
          "Set command runner fade",
          CommandIntent.SetCommandRunnerAnimation(None),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()

    val config = stateManager.getCurrentState.unsafeRunSync().config
    config.commandRunnerAnimation shouldBe None
    config.motionPreset shouldBe MotionPreset.Custom
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

  it should "update the UI outline thickness config" in {
    val stateManager = createStateManager()

    stateManager
      .executeCommand(
        Command.typed(
          "ui-outline-thickness",
          "Set UI outline thickness",
          CommandIntent.SetUiOutlineThicknessPx(4),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()

    stateManager.getCurrentState.unsafeRunSync().config.uiOutlineThicknessPx shouldBe 4
  }
