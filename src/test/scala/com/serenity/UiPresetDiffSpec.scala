package com.serenity

import com.serenity.config.*
import com.serenity.config.AppConfigMotionOps.*
import com.serenity.ui.fonts.FontLoader.FontConfig
import com.serenity.ui.presets.{PresetChange, UiPreset, UiPresetDiff}
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class UiPresetDiffSpec extends AnyFlatSpec with Matchers:

  private def keys(changes: List[PresetChange]): List[String] = changes.map(_.key)

  "UiPresetDiff.changes" should "report nothing when a preset already matches the current state" in {
    val current = AppConfig.default
    val preset = UiPreset(
      name = "Custom",
      config = current,
      themeName = Theme.dark.name
    )

    val changes = UiPresetDiff.changes(
      currentConfig = current,
      currentThemeName = Theme.dark.name,
      currentHasDockedPanels = false,
      currentHasWorkspaceTree = false,
      preset = preset
    )

    changes shouldBe Nil
  }

  it should "capture a single scalar field change for a custom preset" in {
    val current = AppConfig.default.withLineNumbers(false)
    val preset = UiPreset(
      name = "Custom",
      config = current.withLineNumbers(true),
      themeName = Theme.dark.name
    )

    val changes = UiPresetDiff.changes(
      currentConfig = current,
      currentThemeName = Theme.dark.name,
      currentHasDockedPanels = false,
      currentHasWorkspaceTree = false,
      preset = preset
    )

    changes should have size 1
    changes.head.key shouldBe "editor.line_numbers"
    changes.head.currentValue shouldBe "false"
    changes.head.newValue shouldBe "true"
  }

  it should "exclude fields the preset leaves untouched" in {
    val current = AppConfig.default.withLineNumbers(false).withPaneHeaders(true)
    val preset = UiPreset(
      name = "Custom",
      // showPaneHeaders is left at the current value; only showLineNumbers differs.
      config = current.withLineNumbers(true),
      themeName = Theme.dark.name
    )

    val changes = UiPresetDiff.changes(
      currentConfig = current,
      currentThemeName = Theme.dark.name,
      currentHasDockedPanels = false,
      currentHasWorkspaceTree = false,
      preset = preset
    )

    keys(changes) shouldBe List("editor.line_numbers")
  }

  it should "diff a custom (non-built-in) preset by plain config replacement, including a theme change" in {
    val current = AppConfig.default
    val preset = UiPreset(
      name = "My Saved Setup",
      config = current.withSyntaxHighlighting(!current.languageToolsConfig.syntaxHighlightingEnabled),
      themeName = Theme.light.name
    )

    val changes = UiPresetDiff.changes(
      currentConfig = current,
      currentThemeName = Theme.dark.name,
      currentHasDockedPanels = false,
      currentHasWorkspaceTree = false,
      preset = preset
    )

    keys(changes) should contain theSameElementsAs List("theme", "editor.syntax_highlighting")
  }

  it should "report docked-panel and workspace-tree presence changes" in {
    val current = AppConfig.default
    val preset = UiPreset.builtIn("Code").getOrElse(fail("missing Code preset"))

    val changes = UiPresetDiff.changes(
      currentConfig = current,
      currentThemeName = preset.themeName,
      currentHasDockedPanels = false,
      currentHasWorkspaceTree = false,
      preset = preset
    )

    keys(changes) should contain("dockedPanels")
    changes.find(_.key == "dockedPanels").map(_.newValue) shouldBe Some("present")
  }

  it should "produce the same changed-field set the 'code' built-in workflow's known merge rules predict" in {
    // Every field mergeBuiltInWorkflowConfig's "code" case actually assigns (UiPreset.scala:178-182), forced to a
    // value that differs from what the Code preset carries, so each one is guaranteed to show up as a change --
    // fields the merge leaves alone (e.g. statusLine, showPaneHeaders, blurRadius) are left at their defaults, which
    // the Code preset also inherits from AppConfig.default, so they must NOT show up. Setting a non-default motion
    // preset also carries its own character-animation config along (AppConfigMotionOps.withMotionPreset), so the
    // "motion.character" group changes too once patchMotionConfig copies it back from the preset's own preset.
    val current = AppConfig.default
      .withAppMode(AppMode.Prose)
      .withMotionPreset(MotionPreset.Expressive)
      .withFontConfig(FontConfig(codeFontFamily = "Current Code Font"))
      .withLineNumbers(false)
      .withInterfaceDensity(InterfaceDensity.Spacious)
      .withSyntaxHighlighting(false)
      .withSpellCheck(AppConfig.default.languageToolsConfig.spellCheck.copy(enabled = true))

    val codePreset = UiPreset.builtIn("Code").getOrElse(fail("missing Code preset"))

    val changes = UiPresetDiff.changes(
      currentConfig = current,
      currentThemeName = codePreset.themeName,
      currentHasDockedPanels = true,
      currentHasWorkspaceTree = false,
      preset = codePreset
    )

    keys(changes) should contain theSameElementsAs List(
      "workspace.mode",
      "motion",
      "motion.character",
      "typography.code.family",
      "editor.line_numbers",
      "ui.density",
      "editor.syntax_highlighting",
      "spellcheck.enabled"
    )
  }
