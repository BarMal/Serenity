package com.serenity

import java.nio.file.Path

import com.serenity.command.*
import com.serenity.config.{AppConfig, HotkeyAction}
import com.serenity.keystroke.events.{InsertChar, TabKey}
import com.serenity.state.manager.StateManager
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CommandRunnerLoggingSpec extends AnyFlatSpec with Matchers:

  // issue #931: category tabs are retired, so switching `activeCategory` no longer changes what `visibleItems`
  // shows (it's always every command, unfiltered) -- `selected` is just the first registered command now, not a
  // settings group. The `category=Settings` field itself is untouched here (still a real, if now-inert, field on
  // `CommandRunner`; retiring it entirely is tracked as follow-up cleanup, not part of this stage's dispatch
  // migration).
  "StateManager.describeCommandRunnerEvent" should "describe browse mode with category and selected item" in {
    val registry          = CommandRegistry.withToggleUI
    given CommandRegistry = registry
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default)
      .withActiveCategory(CommandCategory.Settings)

    StateManager
      .describeCommandRunnerEvent(TabKey, runner)
      .shouldBe(
        "event=TabKey mode=browse category=Settings selected=command:open-settings"
      )
  }

  it should "describe search mode with the selected command, and without the typed character or query text" in {
    val registry          = CommandRegistry.withToggleUI
    given CommandRegistry = registry
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default)
      .updateSearchTerm("toggle")

    val description = StateManager.describeCommandRunnerEvent(InsertChar('t'), runner)

    description.shouldBe(
      "event=InsertChar mode=search category=All selected=command:toggle-bookmark"
    )
    description should not include "query="
    description should not include "InsertChar(t)"
  }

  "StateManager.describeCommandExecution" should "describe typed command intents clearly" in {
    val command = Command.typed(
      "toggle-line-numbers",
      "Toggle line numbers display on/off",
      CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.ToggleLineNumbers)),
      CommandCategory.View
    )

    StateManager
      .describeCommandExecution(command)
      .shouldBe(
        "command=toggle-line-numbers category=View intent=Settings(PanelChrome(ToggleLineNumbers))"
      )
  }

  it should "describe typed command intents without special casing" in {
    val command = Command.typed("custom", "Run custom action", CommandIntent.File(FileIntent.OpenFile))

    StateManager
      .describeCommandExecution(command)
      .shouldBe(
        "command=custom category=Edit intent=File(OpenFile)"
      )
  }

  it should "describe a comment intent by shape only, without the comment text" in {
    val command = Command.typed(
      "add-document-comment",
      "Add a comment to the document",
      CommandIntent.Comments(CommentsIntent.AddDocumentComment("this is a private note about the quarterly plan"))
    )

    val description = StateManager.describeCommandExecution(command)

    description.shouldBe("command=add-document-comment category=Edit intent=Comments(AddDocumentComment)")
    description should not include "private note"
  }

  it should "describe a recent-file intent by shape only, without the file path" in {
    val command = Command.typed(
      "open-recent-file",
      "Open a recently used file",
      CommandIntent.File(FileIntent.OpenRecentFile(Path.of("/home/user/secret-project/notes.txt")))
    )

    val description = StateManager.describeCommandExecution(command)

    description.shouldBe("command=open-recent-file category=Edit intent=File(OpenRecentFile)")
    description should not include "secret-project"
    description should not include "notes.txt"
  }

  it should "describe a spell-check dictionary intent by shape only, without the words or paths" in {
    val command = Command.typed(
      "set-spell-check-words",
      "Set the custom spell-check dictionary",
      CommandIntent.Settings(
        SettingsIntent.SpellCheck(SpellCheckIntent.SetSpellCheckWords(List("zephyrion", "quixotical")))
      )
    )

    val description = StateManager.describeCommandExecution(command)

    description.shouldBe("command=set-spell-check-words category=Edit intent=Settings(SpellCheck(SetSpellCheckWords))")
    description should not include "zephyrion"
    description should not include "quixotical"
  }

  it should "describe a spell-check dictionary-paths intent by shape only, without the paths" in {
    val command = Command.typed(
      "set-spell-check-dictionary-paths",
      "Set custom spell-check dictionary paths",
      CommandIntent.Settings(
        SettingsIntent.SpellCheck(
          SpellCheckIntent.SetSpellCheckDictionaryPaths(List("/home/user/.dictionaries/custom.dic"))
        )
      )
    )

    val description = StateManager.describeCommandExecution(command)

    description.shouldBe(
      "command=set-spell-check-dictionary-paths category=Edit intent=Settings(SpellCheck(SetSpellCheckDictionaryPaths))"
    )
    description should not include ".dictionaries"
    description should not include "custom.dic"
  }

  it should "describe a UI preset intent by shape only, without the preset name" in {
    val command = Command.typed(
      "save-ui-preset-as-new",
      "Save the current UI layout as a new preset",
      CommandIntent.UiPresets(UiPresetsIntent.SaveUiPresetAsNew("My Secret Layout"))
    )

    val description = StateManager.describeCommandExecution(command)

    description.shouldBe("command=save-ui-preset-as-new category=Edit intent=UiPresets(SaveUiPresetAsNew)")
    description should not include "My Secret Layout"
  }

  it should "describe a key-binding intent by shape only, without the binding text" in {
    val command = Command.typed(
      "set-global-hotkey",
      "Rebind a global hotkey",
      CommandIntent.Keybindings(KeybindingsIntent.SetGlobalHotkey(HotkeyAction.Save, "Ctrl+Shift+Alt+K"))
    )

    val description = StateManager.describeCommandExecution(command)

    description.shouldBe("command=set-global-hotkey category=Edit intent=Keybindings(SetGlobalHotkey)")
    description should not include "Ctrl+Shift+Alt+K"
  }
end CommandRunnerLoggingSpec
