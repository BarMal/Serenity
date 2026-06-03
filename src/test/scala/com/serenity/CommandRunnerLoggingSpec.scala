package com.serenity

import cats.effect.IO
import com.serenity.command.{Command, CommandCategory, CommandIntent, CommandRegistry, CommandRunner}
import com.serenity.keystroke.events.{InsertChar, TabKey}
import com.serenity.state.manager.StateManager
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CommandRunnerLoggingSpec extends AnyFlatSpec with Matchers:

  "StateManager.describeCommandRunnerEvent" should "describe browse mode with category and selected item" in {
    val registry = CommandRegistry.withToggleUI
    given CommandRegistry = registry
    val runner = CommandRunner.empty
      .activate(registry)
      .withActiveCategory(CommandCategory.Settings)

    StateManager.describeCommandRunnerEvent(TabKey, runner).shouldBe(
      "event=TabKey mode=browse category=Settings selected=group:settings-animation"
    )
  }

  it should "describe search mode with the current query and selected command" in {
    val registry = CommandRegistry.withToggleUI
    given CommandRegistry = registry
    val runner = CommandRunner.empty
      .activate(registry)
      .updateSearchTerm("toggle")

    StateManager.describeCommandRunnerEvent(InsertChar('t'), runner).shouldBe(
      "event=InsertChar(t) mode=search query=toggle category=All selected=command:toggle-theme"
    )
  }

  "StateManager.describeCommandExecution" should "describe typed command intents clearly" in {
    val command = Command.typed(
      "toggle-line-numbers",
      "Toggle line numbers display on/off",
      CommandIntent.ToggleLineNumbers,
      CommandCategory.View
    )

    StateManager.describeCommandExecution(command).shouldBe(
      "command=toggle-line-numbers category=View intent=ToggleLineNumbers"
    )
  }

  it should "describe custom commands clearly" in {
    val command = Command("custom", "Run custom action", _ => IO.unit)

    StateManager.describeCommandExecution(command).shouldBe(
      "command=custom category=Edit intent=Custom"
    )
  }
end CommandRunnerLoggingSpec
