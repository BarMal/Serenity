package com.serenity

import com.serenity.command.{
  CommandIntent,
  CommandRunnerSettingsInputItems,
  CommandSurfaceItem,
  InterfaceChromeIntent,
  KeybindingsIntent,
  SettingsIntent,
  SpellCheckIntent,
  TextDisplayIntent,
  UiPresetsIntent
}
import com.serenity.config.*
import com.serenity.config.AppConfigMotionOps.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CommandRunnerSettingsInputItemsSpec extends AnyFlatSpec with Matchers:

  private def inputById(items: List[CommandSurfaceItem.InputItem], id: String): CommandSurfaceItem.InputItem =
    items.find(_.id == id).getOrElse(fail(s"missing input item $id"))

  "CommandRunnerSettingsInputItems" should "build config-backed settings input rows independently of runner state" in {
    val config = AppConfig.default
      .withInterfaceConfig(InterfaceConfig(elementGap = Some(3), outlineThicknessPx = 4))
      .withCommandRunnerVisibleRows(Some(9))
      .withTextAreaInsets(TextAreaInsets(left = 0.10, right = 0.20, top = 0.15, bottom = 0.25))
      .withSpellCheck(
        SpellCheckConfig(
          enabled = true,
          languages = List("EN", "fr"),
          dictionaryPaths = List("C:\\Dictionaries\\en_US.dic"),
          additionalWords = List("Cats")
        )
      )

    val items = CommandRunnerSettingsInputItems.build(config)

    inputById(items, "ui-element-gap").currentValue shouldBe "3"
    inputById(items, "ui-element-gap").parse("4") shouldBe Some(
      CommandIntent.Settings(SettingsIntent.InterfaceChrome(InterfaceChromeIntent.SetUiElementGap(4)))
    )
    inputById(items, "ui-element-gap").parse("0.75") shouldBe Some(
      CommandIntent.Settings(SettingsIntent.InterfaceChrome(InterfaceChromeIntent.SetUiElementGap(0.75)))
    )
    inputById(items, "ui-element-gap").parse("9") shouldBe None
    inputById(items, "ui-outline-thickness").currentValue shouldBe "4"
    inputById(items, "ui-outline-thickness").parse("5") shouldBe Some(
      CommandIntent.Settings(SettingsIntent.InterfaceChrome(InterfaceChromeIntent.SetUiOutlineThicknessPx(5)))
    )
    inputById(items, "ui-outline-thickness").parse("9") shouldBe None
    // issue #1046: "command-runner-visible-rows" (and the other two command-runner row/spacing inputs) is no longer
    // built here -- Interface Density is the one control governing all three now.
    items.map(_.id) should not contain "command-runner-visible-rows"
    inputById(items, "text-area-top").currentValue shouldBe "15.0"
    inputById(items, "text-area-top").parse("12.5") shouldBe Some(
      CommandIntent.Settings(SettingsIntent.TextDisplay(TextDisplayIntent.SetTextAreaTopInset(0.125)))
    )
    inputById(items, "text-area-bottom").currentValue shouldBe "25.0"
    inputById(items, "text-area-bottom").parse("12.5") shouldBe Some(
      CommandIntent.Settings(SettingsIntent.TextDisplay(TextDisplayIntent.SetTextAreaBottomInset(0.125)))
    )
    inputById(items, "spellcheck-languages").currentValue shouldBe "en,fr"
    inputById(items, "spellcheck-languages").parse("fr,en") shouldBe
      Some(CommandIntent.Settings(SettingsIntent.SpellCheck(SpellCheckIntent.SetSpellCheckLanguages(List("fr", "en")))))
    inputById(items, "spellcheck-dictionaries").currentValue shouldBe "C:\\Dictionaries\\en_US.dic"
    inputById(items, "spellcheck-dictionaries").parse("C:\\Dictionaries\\en_US.dic,/usr/share/hunspell/fr.dic") shouldBe
      Some(
        CommandIntent.Settings(
          SettingsIntent.SpellCheck(
            SpellCheckIntent.SetSpellCheckDictionaryPaths(
              List("C:\\Dictionaries\\en_US.dic", "/usr/share/hunspell/fr.dic")
            )
          )
        )
      )
    inputById(items, "spellcheck-words").currentValue shouldBe "cats"
  }

  it should "build preset and binding parsers as reusable schema rows" in {
    val items = CommandRunnerSettingsInputItems.build(AppConfig.default)

    inputById(items, "ui-preset-save-as-new").parse("Focus") shouldBe Some(
      CommandIntent.UiPresets(UiPresetsIntent.SaveUiPresetAsNew("Focus"))
    )
    // issue #1060: "ui-preset-overwrite" is a picker (`CommandRunnerSettingsItems.presetActionOptionItem`) built
    // directly in `CommandRunnerSettingsGroups`, not a typed InputItem from this flat builder.
    items.map(_.id) should not contain "ui-preset-overwrite"
    inputById(items, "ui-preset-rename").parse("Focus -> Review") shouldBe
      Some(CommandIntent.UiPresets(UiPresetsIntent.RenameUiPreset("Focus", "Review")))
    inputById(items, "ui-preset-rename").parse("Focus") shouldBe None
    inputById(items, "keymap-global-command_palette").parse("ctrl+k") shouldBe
      Some(
        CommandIntent.Keybindings(
          KeybindingsIntent.SetGlobalHotkey(com.serenity.config.HotkeyAction.ToggleCommandRunner, "ctrl+k")
        )
      )
    inputById(items, "keymap-global-command_palette").parse("reset") shouldBe
      Some(
        CommandIntent.Keybindings(
          KeybindingsIntent.ResetGlobalHotkey(com.serenity.config.HotkeyAction.ToggleCommandRunner)
        )
      )
  }

  it should "show every current binding in global keymap rows" in {
    val action = HotkeyAction.ToggleCommandRunner
    val config = AppConfig.default.withHotkeyConfig(
      HotkeyConfig(
        HotkeyConfig.defaultBindings.updated(
          action,
          List(
            HotkeyTrigger.parse("ctrl+k").get,
            HotkeyTrigger.parse("ctrl+l").get
          )
        )
      )
    )

    inputById(CommandRunnerSettingsInputItems.build(config), "keymap-global-command_palette").currentValue shouldBe
      "ctrl+k, ctrl+l"
  }

  // issue #1621 carve-out: unset UI element gap / line-number margin / line-number padding used to show "auto" in
  // these rows regardless of surface, because `build` only had `AppConfig` in scope. `isTuiMode` (already carried on
  // `CommandRunner`, mirroring how `activate` receives it separately from `state.runtime.isTuiMode`) resolves the
  // same GUI-cell-vs-TUI-zero default `AppState.effectiveUiElementGap`/`effectiveLineNumberMarginLeft`/
  // `effectiveLineNumberPadding` use, so the row shows the number that would actually apply.
  "an unset UI element gap and line-number margin/padding" should "show the GUI default on the GUI surface" in {
    val items = CommandRunnerSettingsInputItems.build(AppConfig.default, isTuiMode = false)

    inputById(items, "ui-element-gap").currentValue shouldBe "1"
    inputById(items, "line-number-margin-left").currentValue shouldBe "1"
    inputById(items, "line-number-padding").currentValue shouldBe "1"
  }

  it should "show the TUI default (zero) on the TUI surface" in {
    val items = CommandRunnerSettingsInputItems.build(AppConfig.default, isTuiMode = true)

    inputById(items, "ui-element-gap").currentValue shouldBe "0"
    inputById(items, "line-number-margin-left").currentValue shouldBe "0"
    inputById(items, "line-number-padding").currentValue shouldBe "0"
  }

  it should "show an explicit value the same way on both surfaces" in {
    val config = AppConfig.default
      .withInterfaceConfig(InterfaceConfig(elementGap = Some(3)))
      .withLineNumberLayout(LineNumberLayout(marginLeft = Some(2), padding = Some(4)))

    inputById(CommandRunnerSettingsInputItems.build(config, isTuiMode = false), "ui-element-gap").currentValue shouldBe
      "3"
    inputById(CommandRunnerSettingsInputItems.build(config, isTuiMode = true), "ui-element-gap").currentValue shouldBe
      "3"
    inputById(
      CommandRunnerSettingsInputItems.build(config, isTuiMode = false),
      "line-number-margin-left"
    ).currentValue shouldBe "2"
    inputById(
      CommandRunnerSettingsInputItems.build(config, isTuiMode = true),
      "line-number-padding"
    ).currentValue shouldBe "4"
  }
