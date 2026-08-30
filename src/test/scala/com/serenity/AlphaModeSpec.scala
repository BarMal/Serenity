package com.serenity

import com.serenity.app.{AlphaMode, LaunchOptions}
import com.serenity.config.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AlphaModeSpec extends AnyFlatSpec with Matchers:

  private val customized: AppConfig = AppConfig.default
    .withFontConfig(AppConfig.default.editorConfig.fontConfig.copy(codeFontFamily = "Iosevka"))
    .withUiElementGap(4.0)
    .withInterfaceDensity(InterfaceDensity.Compact)
    .withWindowChromeMode(WindowChromeMode.Native)
    .withMotionPreset(MotionPreset.Expressive)
    .withHotkeyOverride(HotkeyAction.Save, "ctrl+shift+s")

  "AlphaMode.overlay" should "enable the command-runner cursor-peek prototype" in {
    AlphaMode.overlay(customized).surfaceConfig.commandRunnerCursorPeekEnabled shouldBe true
  }

  it should "leave every other setting -- theme-adjacent, input, window, document config -- untouched" in {
    val alpha = AlphaMode.overlay(customized)

    alpha.editorConfig shouldBe customized.editorConfig
    alpha.inputConfig shouldBe customized.inputConfig
    alpha.cursorConfig shouldBe customized.cursorConfig
    alpha.windowConfig shouldBe customized.windowConfig
    alpha.windowSitterConfig shouldBe customized.windowSitterConfig
    alpha.documentConfig shouldBe customized.documentConfig
    alpha.interfaceConfig shouldBe customized.interfaceConfig
    alpha.languageToolsConfig shouldBe customized.languageToolsConfig

    // Within surfaceConfig, only the cursor-peek flag moved.
    alpha.surfaceConfig.copy(
      commandRunnerCursorPeekEnabled = customized.surfaceConfig.commandRunnerCursorPeekEnabled
    ) shouldBe customized.surfaceConfig
  }

  it should "be idempotent" in {
    AlphaMode.overlay(AlphaMode.overlay(customized)) shouldBe AlphaMode.overlay(customized)
  }

  "AlphaMode.isRequested" should "activate on the --alpha CLI flag" in {
    AlphaMode.isRequested(LaunchOptions(alpha = true)) shouldBe true
  }

  it should "default to false with no flag" in {
    AlphaMode.isRequested(LaunchOptions(alpha = false)) shouldBe false
  }

  "AlphaMode.applyIfRequested" should "leave the config untouched when alpha is not requested" in {
    AlphaMode.applyIfRequested(customized, LaunchOptions(alpha = false)) shouldBe customized
  }

  it should "apply the alpha overlay when requested via the CLI flag" in {
    AlphaMode.applyIfRequested(customized, LaunchOptions(alpha = true)) shouldBe AlphaMode.overlay(customized)
  }
