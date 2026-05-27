package com.serenity

import cats.effect.IO
import com.serenity.command.*
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SimpleCommandRunnerSpec extends AnyFlatSpec with Matchers:

  "Command" should "have name and description" in {
    val cmd = com.serenity.command.Command("test", "Test command", (_: AppState) => IO.unit)
    cmd.name shouldBe "test"
    cmd.description shouldBe "Test command"
  }

  "CommandSearcher" should "find commands by name" in {
    val commands = List(
      com.serenity.command.Command("save", "Save file", (_: AppState) => IO.unit),
      com.serenity.command.Command("open", "Open file", (_: AppState) => IO.unit)
    )
    val searcher = new CommandSearcher(commands)

    val results = searcher.search("save")
    results.length shouldBe 1
    results.head.name shouldBe "save"
  }

  "CommandRunner" should "start inactive" in {
    val runner = CommandRunner.empty
    runner.isActive shouldBe false
    runner.searchTerm shouldBe ""
    runner.selectedIndex shouldBe 0
  }

  it should "activate with commands" in {
    val registry = CommandRegistry.default
    val runner   = CommandRunner.empty.activate(registry)

    runner.isActive shouldBe true
    runner.filteredCommands should not be empty
  }

  it should "update search term" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry

    val runner  = CommandRunner.empty.activate(registry)
    val updated = runner.updateSearchTerm("save")

    updated.searchTerm shouldBe "save"
    updated.filteredCommands.exists(_.name.contains("save")) shouldBe true
  }

  it should "move selection correctly" in {
    val commands = List(
      com.serenity.command.Command("cmd1", "Command 1", (_: AppState) => IO.unit),
      com.serenity.command.Command("cmd2", "Command 2", (_: AppState) => IO.unit),
      com.serenity.command.Command("cmd3", "Command 3", (_: AppState) => IO.unit)
    )
    val runner = CommandRunner.withCommands(commands)

    val moved = runner.moveSelection(1)
    moved.selectedIndex shouldBe 1

    val wrapped = moved.moveSelection(3) // Should wrap around
    wrapped.selectedIndex shouldBe 1 // (1 + 3) % 3 = 1
  }

  "CommandRegistry" should "provide default commands" in {
    val registry = CommandRegistry.default
    val commands = registry.getAllCommands

    commands should not be empty
    commands.exists(_.name == "save") shouldBe true
    commands.exists(_.name == "open") shouldBe true
    commands.exists(_.name == "quit") shouldBe true
  }
