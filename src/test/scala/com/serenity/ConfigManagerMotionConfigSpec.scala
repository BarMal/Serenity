package com.serenity

import java.nio.file.Files

import com.serenity.animation.{AnimationConfig, TransitionKind}
import com.serenity.config.*
import com.serenity.config.AppConfigMotionOps.*
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The authoritative motion/animation hierarchy: presets, per-family speed scales and transition kinds, legacy
  * migration, and custom animation timing.
  */
class ConfigManagerMotionConfigSpec extends AnyFlatSpec with Matchers with OptionValues:

  "ConfigManager" should "report invalid surface motion config values through the surface schema" in {
    val configFile = Files.createTempFile("serenity-surface-motion-invalid-config", ".conf")
    Files.writeString(
      configFile,
      """ui.material = neon
        |ui.motion = turbo
        |ui.motion.speed_scale = 5
        |ui.motion.command_runner_reveal = sideways
        |""".stripMargin
    )

    val result = ConfigManager.loadConfigResult(Some(configFile.toString))

    result.report.invalidEntries.map(_.key) should contain("ui.material")
    result.report.invalidEntries.map(_.key) should contain("ui.motion")
    result.report.invalidEntries.map(_.key) should contain("ui.motion.speed_scale")
    result.report.invalidEntries.map(_.key) should contain("ui.motion.command_runner_reveal")
  }

  it should "round-trip the authoritative motion hierarchy" in {
    val configured = AppConfig.default.withMotionConfiguration(
      MotionConfig(
        MotionAccessibility.Reduced,
        MotionPreset.Expressive,
        Map(
          MotionFamily.CommandSurfaces -> MotionFamilyConfig(
            transitionKind = TransitionKind.TypedText,
            animation = AnimationConfig.subtle,
            speedScale = 0.5
          ),
          MotionFamily.PinnedPanels -> MotionFamilyConfig(
            transitionKind = TransitionKind.Disabled,
            animation = None,
            speedScale = 0.0
          )
        )
      )
    )
    val configFile = Files.createTempFile("serenity-motion-hierarchy", ".conf")
    val serialized = ConfigManager.configToString(configured)
    Files.writeString(configFile, serialized)

    val loaded = ConfigManager.loadConfig(Some(configFile.toString))

    serialized should not include "ui.motion.speed_scale ="
    serialized should not include "ui.motion.editor_text.speed_scale ="
    serialized should not include "ui.motion.command_runner.speed_scale ="
    serialized should not include "ui.motion.ui.speed_scale ="
    serialized should not include "ui.motion.cursor.speed_scale ="
    serialized should not include "ui.motion.command_runner ="
    serialized should not include "ui.motion.command_runner_reveal ="
    serialized should not include "ui.motion.ui ="
    serialized should not include "ui.motion.editor_text ="
    serialized should not include "ui.motion.panel_open ="
    serialized should not include "ui.motion.panel_close ="
    serialized should include("ui.motion.family.command_surfaces.speed_scale = 0.5")
    serialized should include("ui.motion.family.pinned_panels.close_transition = off")
    loaded.surfaceConfig.motionConfiguration shouldBe configured.surfaceConfig.motionConfiguration.map(configuration =>
      configuration.withFallback(MotionConfig.fromLegacy(configured.surfaceConfig, configuration.baseline))
    )
    loaded.surfaceConfig.effectiveMotionBaseline shouldBe MotionPreset.Expressive
  }

  it should "never let a motion family's enabled flag contradict its transition kind" in {
    // `enabled = true` on a family whose transition kind is otherwise Disabled must not resurrect a contradictory
    // "enabled but Disabled" family: enabled is derived from transitionKind, so there is nothing else for it to turn
    // on.
    val enabledOnly = Files.createTempFile("serenity-motion-enabled-only", ".conf")
    Files.writeString(
      enabledOnly,
      """ui.motion.family.editor_text.transition = off
        |ui.motion.family.editor_text.enabled = true
        |""".stripMargin
    )
    val loadedEnabledOnly = ConfigManager.loadConfig(Some(enabledOnly.toString))
    val editorFamily = loadedEnabledOnly.surfaceConfig.motionConfiguration
      .getOrElse(fail("Expected authoritative motion configuration"))
      .families(MotionFamily.EditorText)
    editorFamily.transitionKind shouldBe TransitionKind.Disabled
    editorFamily.enabled shouldBe false

    // Setting enabled = false must actually disable the family (drive transitionKind to Disabled), not merely be
    // ignored while transitionKind stays on.
    val disabledOverride = Files.createTempFile("serenity-motion-disabled-override", ".conf")
    Files.writeString(
      disabledOverride,
      """ui.motion.family.editor_text.transition = typed
        |ui.motion.family.editor_text.enabled = false
        |""".stripMargin
    )
    val loadedDisabledOverride = ConfigManager.loadConfig(Some(disabledOverride.toString))
    val disabledEditorFamily = loadedDisabledOverride.surfaceConfig.motionConfiguration
      .getOrElse(fail("Expected authoritative motion configuration"))
      .families(MotionFamily.EditorText)
    disabledEditorFamily.transitionKind shouldBe TransitionKind.Disabled
    disabledEditorFamily.enabled shouldBe false
  }

  it should "preserve distinct legacy panel transitions when migrating to the authoritative hierarchy" in {
    val legacy = AppConfig.default
      .withPanelOpenTransitionKind(Some(TransitionKind.DirectionalSweep))
      .withPanelCloseTransitionKind(Some(TransitionKind.Disabled))
    val migrated   = legacy.withMotionConfiguration(MotionConfig.fromLegacy(legacy.surfaceConfig))
    val configFile = Files.createTempFile("serenity-panel-transition-migration", ".conf")
    Files.writeString(configFile, ConfigManager.configToString(migrated))

    val loaded = ConfigManager.loadConfig(Some(configFile.toString))

    loaded.effectivePanelOpenTransitionKind shouldBe TransitionKind.DirectionalSweep
    loaded.effectivePanelCloseTransitionKind shouldBe TransitionKind.Disabled
  }

  it should "round-trip custom family animation timing" in {
    val customAnimation = AnimationConfig(
      steps = 7,
      totalDuration = scala.concurrent.duration.Duration.fromNanos(320_000_000)
    )
    val configured = AppConfig.default.withMotionConfiguration(
      MotionConfig(
        MotionAccessibility.Standard,
        MotionPreset.Smooth,
        Map(
          MotionFamily.CommandSurfaces -> MotionFamilyConfig(
            transitionKind = TransitionKind.TypedText,
            animation = Some(customAnimation),
            speedScale = 1.0
          )
        )
      )
    )
    val configFile = Files.createTempFile("serenity-custom-family-animation", ".conf")
    val serialized = ConfigManager.configToString(configured)
    Files.writeString(configFile, serialized)

    val loaded = ConfigManager.loadConfig(Some(configFile.toString))

    serialized should include("ui.motion.family.command_surfaces.animation.preset = custom")
    serialized should include("ui.motion.family.command_surfaces.animation.duration_ms = 320")
    serialized should include("ui.motion.family.command_surfaces.animation.steps = 7")
    loaded.surfaceConfig.effectiveMotionConfiguration.family(MotionFamily.CommandSurfaces).animation shouldBe Some(
      customAnimation
    )
  }

  it should "load and write material and motion presets" in {
    val configFile = Files.createTempFile("serenity-material-motion-config", ".conf")
    Files.writeString(
      configFile,
      """ui.material = crystal
        |ui.motion = reduced
        |ui.motion.speed_scale = 1.75
        |ui.motion.editor_text.speed_scale = 0.50
        |ui.motion.command_runner.speed_scale = 2.25
        |ui.motion.ui.speed_scale = 1.25
        |ui.motion.cursor.speed_scale = 0.75
        |ui.motion.editor_text = typed
        |ui.motion.panel_open = directional
        |ui.motion.panel_close = off
        |ui.motion.command_runner_reveal = outline
        |ui.motion.command_runner = subtle
        |ui.motion.ui = smooth
        |""".stripMargin
    )

    val config = ConfigManager.loadConfig(Some(configFile.toString))

    config.surfaceConfig.materialPreset shouldBe MaterialPreset.Crystal
    config.surfaceConfig.backgroundStyle shouldBe BackgroundStyle.GlassLike
    config.surfaceConfig.blurRadius shouldBe 0.42f
    config.surfaceConfig.motionPreset shouldBe MotionPreset.Reduced
    config.surfaceConfig.elementTransitionSpeedScale shouldBe 1.75
    config.surfaceConfig.editorTextTransitionSpeedScale shouldBe Some(0.5)
    config.surfaceConfig.commandRunnerTransitionSpeedScale shouldBe Some(2.25)
    config.surfaceConfig.uiTransitionSpeedScale shouldBe Some(1.25)
    config.surfaceConfig.cursorTransitionSpeedScale shouldBe Some(0.75)
    config.surfaceConfig.editorInsertionTransitionKind shouldBe TransitionKind.TypedText
    config.surfaceConfig.panelOpenTransitionKind shouldBe Some(TransitionKind.DirectionalSweep)
    config.surfaceConfig.panelCloseTransitionKind shouldBe Some(TransitionKind.Disabled)
    config.surfaceConfig.commandRunnerTransitionKind shouldBe Some(TransitionKind.OutlineThenContent)
    config.surfaceConfig.commandRunnerAnimation shouldBe com.serenity.animation.AnimationConfig.subtle
    config.surfaceConfig.uiAnimation shouldBe com.serenity.animation.AnimationConfig.smooth
    config.editorConfig.characterAnimation shouldBe None
    val serialized = ConfigManager.configToString(config)
    serialized should include("ui.material = crystal")
    serialized should include("ui.motion.preset = reduced")
    serialized should include("ui.motion.accessibility = standard")
    serialized should include("ui.motion.family.editor_text.transition = typed")
    serialized should include("ui.motion.family.editor_text.speed_scale = 0.5")
    serialized should include("ui.motion.family.command_surfaces.transition = outline")
    serialized should include("ui.motion.family.command_surfaces.animation.preset = subtle")
    serialized should include("ui.motion.family.command_surfaces.speed_scale = 2.25")
    serialized should include("ui.motion.family.pinned_panels.open_transition = directional")
    serialized should include("ui.motion.family.pinned_panels.close_transition = off")
    serialized should include("ui.motion.family.ui_transitions.animation.preset = smooth")
    serialized should include("ui.motion.family.cursor.speed_scale = 0.75")
  }

  it should "load and write the post-processing effect" in {
    val configFile = Files.createTempFile("serenity-post-processing-config", ".conf")
    Files.writeString(configFile, "ui.post_processing = scanlines-glow\nui.shadows = false\n")

    val config = ConfigManager.loadConfig(Some(configFile.toString))

    config.surfaceConfig.postProcessingEffect shouldBe PostProcessingEffect.ScanlinesAndGlow
    config.surfaceConfig.uiShadowsEnabled shouldBe false
    ConfigManager.configToString(config) should include("ui.post_processing = scanlines-glow")
    ConfigManager.configToString(config) should include("ui.shadows = false")
  }

  it should "round-trip custom character animation duration and steps" in {
    val customAnimation = AnimationConfig(
      steps = 7,
      totalDuration = scala.concurrent.duration.Duration.fromNanos(320_000_000)
    )
    val written = ConfigManager.configToString(AppConfig.default.withCharacterAnimation(customAnimation))

    written should include("character.animation.preset = custom")
    written should include("character.animation.duration_ms = 320")
    written should include("character.animation.steps = 7")

    val configFile = Files.createTempFile("serenity-custom-animation-config", ".conf")
    Files.writeString(configFile, written)

    val loaded = ConfigManager.loadConfig(Some(configFile.toString))

    loaded.surfaceConfig.motionPreset shouldBe MotionPreset.Custom
    loaded.editorConfig.characterAnimation.value.durationMs shouldBe 320
    loaded.editorConfig.characterAnimation.value.steps shouldBe 7
  }
