package com.serenity

import com.serenity.command.{CommandIntent, CommandRegistry, ViewIntent}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ShortcutsHelpCommandSpec extends AnyFlatSpec with Matchers:

  "CommandRegistry.default" should "register a toggle-shortcuts-help command reachable from the command palette" in {
    val command = CommandRegistry.default.getAllCommands
      .find(_.name == "toggle-shortcuts-help")
      .getOrElse(fail("Expected a toggle-shortcuts-help command"))

    command.intent shouldBe CommandIntent.View(ViewIntent.ToggleShortcutsHelp)
  }
