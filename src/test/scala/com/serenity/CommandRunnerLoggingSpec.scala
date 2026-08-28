package com.serenity

import com.serenity.command.*
import com.serenity.config.AppConfig
import com.serenity.keystroke.events.{InsertChar, TabKey}
import com.serenity.state.manager.StateManager
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CommandRunnerLoggingSpec extends AnyFlatSpec with Matchers:

  "StateManager.describeCommandRunnerEvent" should "describe browse mode with category and selected item" in {
    val registry          = CommandRegistry.withToggleUI
    given CommandRegistry = registry
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default)
      .withActiveCategory(CommandCategory.Settings)

    StateManager
      .describeCommandRunnerEvent(TabKey, runner)
      .shouldBe(
        "event=TabKey mode=browse category=Settings selected=group:settings-workspace-layout"
      )
  }

  it should "describe search mode with the current query and selected command" in {
    val registry          = CommandRegistry.withToggleUI
    given CommandRegistry = registry
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default)
      .updateSearchTerm("toggle")

    StateManager
      .describeCommandRunnerEvent(InsertChar('t'), runner)
      .shouldBe(
        "event=InsertChar(t) mode=search query=toggle category=All selected=command:toggle-bookmark"
      )
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
end CommandRunnerLoggingSpec
