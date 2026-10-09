package com.serenity

import _root_.io.circe.Json
import _root_.io.circe.parser.decode
import _root_.io.circe.syntax.*
import com.serenity.config.*
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.presets.{UiPreset, UiPresetDiff}
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A preset may name no theme, in which case applying it leaves the theme as it is. */
class UiPresetOptionalThemeSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val themeless = UiPreset(name = "Themeless", config = AppConfig.default.withLineNumbers(true))

  private val withTheme = themeless.copy(themeName = Some(Theme.dark.name))

  private def state(theme: Theme): AppState =
    val initial = AppState.empty(AppConfig.default)
    initial.copy(persisted = initial.persisted.copy(theme = theme))

  "A saved preset" should "keep its theme when it is loaded" in {
    val saved = withTheme.asJson.noSpaces

    decode[UiPreset](saved).map(_.themeName) shouldBe Right(Some(Theme.dark.name))
  }

  it should "count the theme of a preset saved before themes were optional as set" in {
    val legacy = withTheme.asJson.mapObject(_.add("themeName", Json.fromString("light")))

    decode[UiPreset](legacy.noSpaces).map(_.themeName) shouldBe Right(Some("light"))
  }

  it should "load without a theme when none was saved" in {
    val stored = withTheme.asJson.mapObject(_.remove("themeName"))

    decode[UiPreset](stored.noSpaces).map(_.themeName) shouldBe Right(None)
  }

  it should "treat a blank theme name as no theme rather than one that cannot load" in {
    val stored = withTheme.asJson.mapObject(_.add("themeName", Json.fromString("")))

    decode[UiPreset](stored.noSpaces).map(_.themeName) shouldBe Right(None)
  }

  "A preset without a theme" should "be written without a themeName field" in {
    themeless.asJson.asObject.map(_.contains("themeName")) shouldBe Some(false)
  }

  it should "survive a save and load still without a theme" in {
    decode[UiPreset](themeless.asJson.noSpaces) shouldBe Right(themeless)
  }

  it should "leave the current theme alone when applied" in {
    val applied = UiPreset.applyToState(themeless, state(Theme.light), None)

    applied.persisted.theme shouldBe Theme.light
  }

  it should "offer no theme change" in {
    val changes = UiPresetDiff.changes(AppConfig.default, Theme.light.name, false, false, themeless)

    changes.map(_.key) should not contain "theme"
  }

  it should "leave the theme alone when the theme change is selected" in {
    val applied = UiPresetDiff.applySelected(state(Theme.light), None, themeless, Set("theme"))

    applied.persisted.theme shouldBe Theme.light
  }

  "Naming a theme on a preset" should "set it on a preset that had none" in {
    themeless.withThemeName(Some("light")).themeName shouldBe Some("light")
  }

  it should "replace the theme a preset already names" in {
    withTheme.withThemeName(Some("light")).themeName shouldBe Some("light")
  }

  it should "clear the theme when given none" in {
    withTheme.withThemeName(None) shouldBe themeless
  }

  it should "trim the name, and treat a blank one as none" in {
    themeless.withThemeName(Some("  light ")).themeName shouldBe Some("light")
    withTheme.withThemeName(Some("   ")).themeName shouldBe None
  }

  it should "change nothing but the theme" in {
    val rich =
      withTheme.copy(targetEditorPaneCount = Some(2), unknownFields = _root_.io.circe.JsonObject("x" -> Json.True))

    rich.withThemeName(Some("light")) shouldBe rich.copy(themeName = Some("light"))
  }

  it should "survive a save and load, and a cleared theme be written without the field" in {
    val named = themeless.withThemeName(Some("light"))

    decode[UiPreset](named.asJson.noSpaces) shouldBe Right(named)
    named.withThemeName(None).asJson.asObject.map(_.contains("themeName")) shouldBe Some(false)
  }

  "A preset with a theme" should "apply it when not following the OS" in {
    val applied = UiPreset.applyToState(withTheme, state(Theme.light), Some(Theme.dark))

    applied.persisted.theme shouldBe Theme.dark
  }

  it should "offer a theme change only when the theme differs" in {
    val differs = UiPresetDiff.changes(AppConfig.default, Theme.light.name, false, false, withTheme)
    val same    = UiPresetDiff.changes(AppConfig.default, Theme.dark.name, false, false, withTheme)

    differs.filter(_.key == "theme").map(change => (change.currentValue, change.newValue)) shouldBe
      List((Theme.light.name, Theme.dark.name))
    same.map(_.key) should not contain "theme"
  }

  "Capturing a preset from the current state" should "include the current theme" in {
    UiPreset.capture("Snapshot", state(Theme.light), None).themeName shouldBe Some(Theme.light.name)
  }

  "The built-in workflow presets" should "name no theme, since they never change it" in {
    UiPreset.builtIns.map(_.themeName) should contain only None
  }
