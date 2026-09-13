package com.serenity

import java.nio.file.Files

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.animation.{AnimationConfig, TransitionKind, WindowSitter, WindowSitterConfig}
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
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class StateManagerMotionAccessibilitySpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private def createStateManager(initialConfig: com.serenity.config.AppConfig = AppConfig.default): StateManager =
    val logger = LoggerFactory[IO].getLogger(using LoggerName("StateManagerMotionAccessibilitySpec"))
    StateManager(logger, initialConfig = initialConfig).unsafeRunSync()

  "StateManager element transition setting commands" should "update the element transition speed scale config" in {
    val stateManager = createStateManager()

    stateManager.commandExecutor
      .executeCommand(
        Command.typed(
          "element-transition-speed-scale",
          "Set element transition speed scale",
          CommandIntent.Settings(SettingsIntent.Motion(MotionIntent.SetElementTransitionSpeedScale(2.25))),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()

    stateManager.getCurrentState
      .unsafeRunSync()
      .persisted
      .config
      .surfaceConfig
      .elementTransitionSpeedScale shouldBe 2.25
  }

  it should "update the global motion accessibility override without changing the baseline" in {
    val stateManager = createStateManager()
    stateManager
      .updateState(state =>
        state.copy(persisted =
          state.persisted.copy(config = state.persisted.config.withMotionPreset(MotionPreset.Expressive))
        )
      )
      .unsafeRunSync()

    stateManager.commandExecutor
      .executeCommand(
        Command.typed(
          "motion-accessibility",
          "Set motion accessibility",
          CommandIntent.Settings(SettingsIntent.Motion(MotionIntent.SetMotionAccessibility(MotionAccessibility.Off))),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()

    val motion = stateManager.getCurrentState
      .unsafeRunSync()
      .persisted
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
      .updateState(state =>
        state.copy(runtime = state.runtime.copy(windowSitter = WindowSitter.default.observeTyping(1_000_000_000L)))
      )
      .unsafeRunSync()

    stateManager.commandExecutor
      .executeCommand(
        Command.typed(
          "motion-accessibility",
          "Set motion accessibility",
          CommandIntent.Settings(SettingsIntent.Motion(MotionIntent.SetMotionAccessibility(MotionAccessibility.Off))),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()

    val sitter = stateManager.getCurrentState.unsafeRunSync().runtime.windowSitter
    sitter.isActive shouldBe false
    sitter.glyph shouldBe "·"
  }

  it should "restore configured resting sitter frames when UI motion is re-enabled" in {
    val initialConfig = AppConfig.default.withWindowSitterConfig(WindowSitterConfig(frames = Vector("rest", "active")))
    val stateManager  = createStateManager(initialConfig)

    stateManager.commandExecutor
      .executeCommand(
        Command.typed(
          "motion-accessibility-off",
          "Disable motion",
          CommandIntent.Settings(SettingsIntent.Motion(MotionIntent.SetMotionAccessibility(MotionAccessibility.Off))),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()
    stateManager.commandExecutor
      .executeCommand(
        Command.typed(
          "motion-accessibility-standard",
          "Enable motion",
          CommandIntent.Settings(
            SettingsIntent.Motion(MotionIntent.SetMotionAccessibility(MotionAccessibility.Standard))
          ),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()

    stateManager.getCurrentState.unsafeRunSync().runtime.windowSitter.glyph shouldBe "rest"
  }

  it should "update persisted window sitter controls through settings commands" in {
    val stateManager = createStateManager()
    val commands = List(
      CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetWindowSitterEnabled(false))),
      CommandIntent.Settings(
        SettingsIntent.PanelChrome(
          PanelChromeIntent.SetWindowSitterAction(com.serenity.animation.WindowSitterAction.Blink)
        )
      ),
      CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetWindowSitterFrames(Vector(".", "x")))),
      CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetWindowSitterActiveTicks(4))),
      CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetWindowSitterFastActiveTicks(9))),
      CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetWindowSitterFastTypingThresholdMs(275)))
    )
    commands.zipWithIndex.foreach {
      case (intent, index) =>
        stateManager.commandExecutor
          .executeCommand(Command.typed(s"window-sitter-$index", "Set sitter option", intent, CommandCategory.Settings))
          .unsafeRunSync()
    }

    stateManager.getCurrentState.unsafeRunSync().persisted.config.windowSitterConfig shouldBe WindowSitterConfig(
      enabled = false,
      action = com.serenity.animation.WindowSitterAction.Blink,
      frames = Vector(".", "x"),
      activeTicks = 4,
      fastActiveTicks = 9,
      fastTypingThresholdMs = 275
    )
  }

  it should "mark the motion preset custom when an explicit motion speed is edited" in {
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
          "element-transition-speed-scale",
          "Set element transition speed scale",
          CommandIntent.Settings(SettingsIntent.Motion(MotionIntent.SetElementTransitionSpeedScale(2.25))),
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

  it should "persist custom as the authoritative baseline after a manual motion edit" in {
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
          "element-transition-speed-scale",
          "Set element transition speed scale",
          CommandIntent.Settings(SettingsIntent.Motion(MotionIntent.SetElementTransitionSpeedScale(2.25))),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()

    val config     = stateManager.getCurrentState.unsafeRunSync().persisted.config
    val configFile = Files.createTempFile("serenity-custom-motion-baseline", ".conf")
    Files.writeString(configFile, ConfigManager.configToString(config))

    config.surfaceConfig.motionConfiguration.map(_.baseline) shouldBe Some(MotionPreset.Custom)
    ConfigManagerTestSupport
      .loadConfig(Some(configFile.toString))
      .surfaceConfig
      .effectiveMotionBaseline shouldBe MotionPreset.Custom
  }

  it should "promote custom editor timing into the authoritative family" in {
    val stateManager = createStateManager(AppConfig.default.withMotionPreset(MotionPreset.Custom))

    List(
      CommandIntent.Settings(SettingsIntent.General(GeneralSettingsIntent.SetAnimationDuration(375))),
      CommandIntent.Settings(SettingsIntent.General(GeneralSettingsIntent.SetAnimationSteps(9)))
    ).zipWithIndex.foreach {
      case (intent, index) =>
        stateManager.commandExecutor
          .executeCommand(
            Command.typed(s"custom-editor-timing-$index", "Set custom editor timing", intent, CommandCategory.Settings)
          )
          .unsafeRunSync()
    }

    val config     = stateManager.getCurrentState.unsafeRunSync().persisted.config
    val configFile = Files.createTempFile("serenity-custom-editor-timing", ".conf")
    Files.writeString(configFile, ConfigManager.configToString(config))
    val persistedFamily = ConfigManagerTestSupport
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
          AnimationConfig.Enabled.smooth
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
    val updated = ConfigManagerTestSupport
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
      editorConfig = EditorConfig(characterAnimation = AnimationConfig.smooth),
      surfaceConfig = SurfaceConfig(
        motionPreset = MotionPreset.Custom,
        commandRunnerAnimation = AnimationConfig.subtle,
        uiAnimation = AnimationConfig.quick,
        panelOpenTransitionKind = Some(TransitionKind.OutlineThenContent),
        panelCloseTransitionKind = Some(TransitionKind.Fade)
      )
    )
    val families = configured
      .withEditorTextAnimation(
        Some(
          AnimationConfig.Enabled.smooth
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
      MotionAccessibility.Off -> CommandIntent.Settings(
        SettingsIntent.Motion(MotionIntent.SetElementTransitionSpeedScale(2.25))
      ),
      MotionAccessibility.Reduced -> CommandIntent.Settings(
        SettingsIntent.Motion(MotionIntent.SetCommandRunnerTransitionKind(TransitionKind.DirectionalSweep))
      )
    ).foreach {
      case (accessibility, edit) =>
        val stateManager = createStateManager()
        stateManager
          .updateState(state =>
            state.copy(persisted =
              state.persisted.copy(config = state.persisted.config.withMotionPreset(MotionPreset.Smooth))
            )
          )
          .unsafeRunSync()

        stateManager.commandExecutor
          .executeCommand(
            Command.typed(
              "motion-accessibility",
              "Set motion accessibility",
              CommandIntent.Settings(SettingsIntent.Motion(MotionIntent.SetMotionAccessibility(accessibility))),
              CommandCategory.Settings
            )
          )
          .unsafeRunSync()
        stateManager.commandExecutor
          .executeCommand(Command.typed("motion-edit", "Edit motion", edit, CommandCategory.Settings))
          .unsafeRunSync()

        val config = stateManager.getCurrentState.unsafeRunSync().persisted.config
        config.surfaceConfig.motionPreset shouldBe MotionPreset.Custom
        config.surfaceConfig.motionConfiguration.map(_.accessibility) shouldBe Some(accessibility)
        config.surfaceConfig.effectiveMotionConfiguration.families.values.foreach(_.enabled shouldBe false)
    }

  it should "disable the authoritative editor text family when its transition kind is turned off" in {
    val stateManager = createStateManager()
    stateManager
      .updateState(state =>
        state.copy(
          persisted = state.persisted.copy(config =
            com.serenity.config.AppConfig.withTestAnimations.withMotionAccessibility(MotionAccessibility.Standard)
          )
        )
      )
      .unsafeRunSync()

    stateManager.commandExecutor
      .executeCommand(
        Command.typed(
          "editor-text-transition",
          "Set editor text transition",
          CommandIntent.Settings(
            SettingsIntent.Motion(MotionIntent.SetEditorInsertionTransitionKind(TransitionKind.Disabled))
          ),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()

    val config = stateManager.getCurrentState.unsafeRunSync().persisted.config
    config.surfaceConfig.effectiveMotionConfiguration
      .family(com.serenity.config.MotionFamily.EditorText)
      .enabled shouldBe false
  }
