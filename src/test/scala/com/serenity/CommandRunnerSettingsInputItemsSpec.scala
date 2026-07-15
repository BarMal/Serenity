package com.serenity

import com.serenity.command.{CommandIntent, CommandRunnerSettingsInputItems, CommandSurfaceItem}
import com.serenity.config.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CommandRunnerSettingsInputItemsSpec extends AnyFlatSpec with Matchers:

  private def inputById(items: List[CommandSurfaceItem.InputItem], id: String): CommandSurfaceItem.InputItem =
    items.find(_.id == id).getOrElse(fail(s"missing input item $id"))

  "CommandRunnerSettingsInputItems" should "build config-backed settings input rows independently of runner state" in {
    val config = AppConfig.default
      .withInterfaceConfig(InterfaceConfig(elementGap = 0.75, outlineThicknessPx = 4))
      .withCommandRunnerItemGapRows(0.25)
      .withCommandRunnerCursorGapRows(Some(0.5))
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

    inputById(items, "ui-element-gap").currentValue shouldBe "0.75"
    inputById(items, "ui-element-gap").isDecimal shouldBe true
    inputById(items, "ui-element-gap").parse("0.5") shouldBe Some(CommandIntent.SetUiElementGap(0.5))
    inputById(items, "ui-element-gap").parse("9") shouldBe None
    inputById(items, "ui-outline-thickness").currentValue shouldBe "4"
    inputById(items, "ui-outline-thickness").parse("5") shouldBe Some(CommandIntent.SetUiOutlineThicknessPx(5))
    inputById(items, "ui-outline-thickness").parse("9") shouldBe None
    inputById(items, "command-runner-visible-rows").currentValue shouldBe "9"
    inputById(items, "command-runner-visible-rows").searchText.toLowerCase should include("visible commands")
    inputById(items, "command-runner-visible-rows").parse("auto") shouldBe
      Some(CommandIntent.SetCommandRunnerVisibleRows(None))
    inputById(items, "command-runner-item-gap-rows").isDecimal shouldBe true
    inputById(items, "command-runner-item-gap-rows").parse("0.25") shouldBe
      Some(CommandIntent.SetCommandRunnerItemGapRows(0.25))
    inputById(items, "command-runner-cursor-gap-rows").isDecimal shouldBe true
    inputById(items, "command-runner-cursor-gap-rows").parse("0.5") shouldBe
      Some(CommandIntent.SetCommandRunnerCursorGapRows(Some(0.5)))
    inputById(items, "text-area-top").currentValue shouldBe "15.0"
    inputById(items, "text-area-top").parse("12.5") shouldBe Some(CommandIntent.SetTextAreaTopInset(0.125))
    inputById(items, "text-area-bottom").currentValue shouldBe "25.0"
    inputById(items, "text-area-bottom").parse("12.5") shouldBe Some(CommandIntent.SetTextAreaBottomInset(0.125))
    inputById(items, "spellcheck-languages").currentValue shouldBe "en,fr"
    inputById(items, "spellcheck-languages").parse("fr,en") shouldBe
      Some(CommandIntent.SetSpellCheckLanguages(List("fr", "en")))
    inputById(items, "spellcheck-dictionaries").currentValue shouldBe "C:\\Dictionaries\\en_US.dic"
    inputById(items, "spellcheck-dictionaries").parse("C:\\Dictionaries\\en_US.dic,/usr/share/hunspell/fr.dic") shouldBe
      Some(
        CommandIntent.SetSpellCheckDictionaryPaths(List("C:\\Dictionaries\\en_US.dic", "/usr/share/hunspell/fr.dic"))
      )
    inputById(items, "spellcheck-words").currentValue shouldBe "cats"
  }

  it should "build preset and binding parsers as reusable schema rows" in {
    val items = CommandRunnerSettingsInputItems.build(AppConfig.default)

    inputById(items, "ui-preset-create").parse("Focus") shouldBe Some(CommandIntent.SaveUiPreset("Focus"))
    inputById(items, "ui-preset-rename").parse("Focus -> Review") shouldBe
      Some(CommandIntent.RenameUiPreset("Focus", "Review"))
    inputById(items, "ui-preset-rename").parse("Focus") shouldBe None
    inputById(items, "keymap-global-command_palette").parse("ctrl+k") shouldBe
      Some(CommandIntent.SetGlobalHotkey(com.serenity.config.HotkeyAction.ToggleCommandRunner, "ctrl+k"))
    inputById(items, "keymap-global-command_palette").parse("reset") shouldBe
      Some(CommandIntent.ResetGlobalHotkey(com.serenity.config.HotkeyAction.ToggleCommandRunner))
  }
