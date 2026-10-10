package com.serenity

import com.serenity.command.*
import com.serenity.config.*
import com.serenity.rope.Balance
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The ways to give a preset a theme, or take it away, from the preset's own Edit Preset page. */
class CommandRunnerUiPresetThemeSettingsSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private def everything(items: List[CommandSurfaceItem]): List[CommandSurfaceItem] =
    items.flatMap {
      case group: CommandSurfaceItem.GroupItem => group :: everything(group.children)
      case item                                => List(item)
    }

  private def presetActions(editing: Option[String]): List[CommandSurfaceItem] =
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default)
      .withUiPresetNames(List("Drafting"))
      .copy(editingPresetName = editing)
    everything(runner.settingsGroups)
      .collectFirst { case group: CommandSurfaceItem.GroupItem if group.id == "settings-preset-edit" => group }
      .getOrElse(fail("missing Edit Preset page"))
      .children

  private def input(id: String, editing: Option[String] = Some("Drafting")): CommandSurfaceItem.InputItem =
    presetActions(editing)
      .collectFirst { case item: CommandSurfaceItem.InputItem if item.id == id => item }
      .getOrElse(fail(s"missing input $id"))

  private def picker(id: String, editing: Option[String] = Some("Drafting")): CommandSurfaceItem.OptionItem =
    presetActions(editing)
      .collectFirst { case item: CommandSurfaceItem.OptionItem if item.id == id => item }
      .getOrElse(fail(s"missing picker $id"))

  "Preset Actions" should "offer to set the preset's theme by name, starting from the preset being edited" in {
    val setTheme = input("ui-preset-set-theme")

    setTheme.label shouldBe "Set Preset Theme"
    setTheme.hint shouldBe "Preset -> Theme"
    setTheme.currentValue shouldBe "Drafting -> "
    setTheme.parse("Drafting -> light") shouldBe Some(
      CommandIntent.UiPresets(UiPresetsIntent.SetUiPresetTheme("Drafting", "light"))
    )
  }

  it should "not accept a theme command without a theme name" in {
    input("ui-preset-set-theme").parse("Drafting ->") shouldBe None
    input("ui-preset-set-theme").parse("Drafting") shouldBe None
  }

  it should "offer to use the current theme for the preset being edited" in {
    val useCurrent = picker("ui-preset-use-current-theme")

    useCurrent.label shouldBe "Use Current Theme"
    useCurrent.selectedOption shouldBe "Drafting"
    useCurrent.selectedIntent shouldBe Some(
      CommandIntent.UiPresets(UiPresetsIntent.UseCurrentThemeForUiPreset("Drafting"))
    )
  }

  it should "offer to clear the theme of the preset being edited" in {
    val clear = picker("ui-preset-clear-theme")

    clear.label shouldBe "Clear Preset Theme"
    clear.selectedOption shouldBe "Drafting"
    clear.selectedIntent shouldBe Some(CommandIntent.UiPresets(UiPresetsIntent.ClearUiPresetTheme("Drafting")))
  }

  it should "let the pickers reach every built-in and saved preset" in {
    val expected = List("Writing", "Documentation", "Code", "Compact", "Review", "Drafting")

    picker("ui-preset-use-current-theme").options.map(_.label) shouldBe expected
    picker("ui-preset-clear-theme").options.map(_.label) shouldBe expected
  }
