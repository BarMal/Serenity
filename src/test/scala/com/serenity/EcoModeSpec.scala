package com.serenity

import com.serenity.app.{EcoMode, LaunchOptions}
import com.serenity.config.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class EcoModeSpec extends AnyFlatSpec with Matchers:

  private val customized: AppConfig = AppConfig.default
    .withFontConfig(AppConfig.default.editorConfig.fontConfig.copy(codeFontFamily = "Iosevka"))
    .withUiElementGap(4.0)
    .withInterfaceDensity(InterfaceDensity.Compact)
    .withWindowChromeMode(WindowChromeMode.Native)
    .withMotionPreset(MotionPreset.Expressive)
    .withHotkeyOverride(HotkeyAction.Save, "ctrl+shift+s")

  "EcoMode.overlay" should "set the render fps target to 30 and motion accessibility to reduced" in {
    val eco = EcoMode.overlay(customized)

    eco.surfaceConfig.renderFpsTarget shouldBe RenderFpsTarget.Fps30
    eco.surfaceConfig.motionConfiguration.map(_.accessibility) shouldBe Some(MotionAccessibility.Reduced)
  }

  it should "disable every motion family, including cursor blink/breathe, without a separate cursor knob" in {
    val eco = EcoMode.overlay(customized)

    MotionFamily.values.foreach { family =>
      eco.surfaceConfig.effectiveMotionConfiguration.family(family).enabled shouldBe false
    }
  }

  it should "leave every other setting -- theme-adjacent, input, window, document config -- untouched" in {
    val eco = EcoMode.overlay(customized)

    eco.editorConfig shouldBe customized.editorConfig
    eco.inputConfig shouldBe customized.inputConfig
    eco.cursorConfig shouldBe customized.cursorConfig
    eco.windowConfig shouldBe customized.windowConfig
    eco.windowSitterConfig shouldBe customized.windowSitterConfig
    eco.documentConfig shouldBe customized.documentConfig
    eco.interfaceConfig shouldBe customized.interfaceConfig
    eco.languageToolsConfig shouldBe customized.languageToolsConfig

    // Within surfaceConfig, only the fps target and the motion hierarchy's accessibility field moved.
    eco.surfaceConfig.copy(
      renderFpsTarget = customized.surfaceConfig.renderFpsTarget,
      motionConfiguration = customized.surfaceConfig.motionConfiguration
    ) shouldBe customized.surfaceConfig
  }

  it should "preserve the user's motion baseline and per-family values under the accessibility override" in {
    val eco = EcoMode.overlay(customized)

    eco.surfaceConfig.motionConfiguration.map(_.baseline) shouldBe
      customized.surfaceConfig.motionConfiguration.map(_.baseline)
    eco.surfaceConfig.motionConfiguration.map(_.families) shouldBe
      customized.surfaceConfig.motionConfiguration.map(_.families)
  }

  it should "be idempotent" in {
    EcoMode.overlay(EcoMode.overlay(customized)) shouldBe EcoMode.overlay(customized)
  }

  "EcoMode.isRequested" should "activate on the --eco CLI flag" in {
    EcoMode.isRequested(LaunchOptions(eco = true), env = Map.empty) shouldBe true
  }

  it should "activate on SERENITY_ECO=1 when the CLI flag is absent" in {
    EcoMode.isRequested(LaunchOptions(eco = false), env = Map("SERENITY_ECO" -> "1")) shouldBe true
  }

  it should "not activate on other SERENITY_ECO values" in {
    EcoMode.isRequested(LaunchOptions(eco = false), env = Map("SERENITY_ECO" -> "0")) shouldBe false
    EcoMode.isRequested(LaunchOptions(eco = false), env = Map("SERENITY_ECO" -> "true")) shouldBe false
  }

  it should "default to false with no flag, env var, or config key" in {
    EcoMode.isRequested(LaunchOptions(eco = false), env = Map.empty) shouldBe false
  }

  it should "let the CLI flag win when it and the env var disagree" in {
    EcoMode.isRequested(LaunchOptions(eco = true), env = Map("SERENITY_ECO" -> "0")) shouldBe true
  }

  "EcoMode.applyIfRequested" should "leave the config untouched when eco is not requested" in {
    EcoMode.applyIfRequested(customized, LaunchOptions(eco = false), env = Map.empty) shouldBe customized
  }

  it should "apply the eco overlay when requested via the CLI flag" in {
    EcoMode.applyIfRequested(customized, LaunchOptions(eco = true), env = Map.empty) shouldBe EcoMode.overlay(
      customized
    )
  }

  it should "apply the eco overlay when requested via the environment" in {
    EcoMode.applyIfRequested(
      customized,
      LaunchOptions(eco = false),
      env = Map("SERENITY_ECO" -> "1")
    ) shouldBe EcoMode.overlay(customized)
  }
