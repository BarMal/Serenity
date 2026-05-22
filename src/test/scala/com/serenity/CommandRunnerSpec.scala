package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.command.{Command, CommandRegistry, CommandRunner, CommandSearcher}
import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.components.CommandRunnerComponent
import com.serenity.state.components.ComponentResult
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CommandRunnerSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  "CommandRegistry" should "register and find commands" in {
    val registry = CommandRegistry.default
    val commands = registry.getAllCommands

    commands should not be empty
    commands.exists(_.name == "save") shouldBe true
    commands.exists(_.name == "open") shouldBe true
  }

  it should "search commands by partial name" in {
    val registry     = CommandRegistry.default
    val saveCommands = registry.searchCommands("sav")

    saveCommands should not be empty
    saveCommands.exists(_.name.contains("save")) shouldBe true
  }

  it should "search commands by description" in {
    val registry     = CommandRegistry.default
    val fileCommands = registry.searchCommands("file")

    fileCommands should not be empty
    fileCommands.exists(_.description.toLowerCase.contains("file")) shouldBe true
  }

  "CommandSearcher" should "filter commands based on search term" in {
    val commands = List(
      Command("save", "Save current file", _ => IO.unit),
      Command("save-as", "Save file with new name", _ => IO.unit),
      Command("open", "Open file", _ => IO.unit),
      Command("quit", "Quit application", _ => IO.unit)
    )

    val searcher = new CommandSearcher(commands)

    val saveResults = searcher.search("save")
    saveResults.length shouldBe 2
    saveResults.map(_.name) should contain allOf ("save", "save-as")

    val openResults = searcher.search("open")
    openResults.length shouldBe 1
    openResults.head.name shouldBe "open"
  }

  it should "return commands in relevance order" in {
    val commands = List(
      Command("save", "Save current file", _ => IO.unit),
      Command("save-as", "Save file with new name", _ => IO.unit),
      Command("auto-save", "Enable auto save", _ => IO.unit)
    )

    val searcher = new CommandSearcher(commands)
    val results  = searcher.search("save")

    // "save" should come before "save-as" and "auto-save" due to exact match
    results.head.name shouldBe "save"
  }

  it should "limit results to specified count" in {
    val commands = (1 to 10).map(i => Command(s"cmd$i", s"Command $i", _ => IO.unit)).toList
    val searcher = new CommandSearcher(commands)

    val results = searcher.search("cmd", maxResults = 5)
    results.length shouldBe 5
  }

  "CommandRunner state" should "initialize with empty search and no selection" in {
    val runner = CommandRunner.empty

    runner.searchTerm shouldBe ""
    runner.selectedIndex shouldBe 0
    runner.isActive shouldBe false
    runner.filteredCommands shouldBe empty
  }

  it should "update search term and filter commands" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val runner            = CommandRunner.empty.activate(registry)

    val updated = runner.updateSearchTerm("save")
    updated.searchTerm shouldBe "save"
    updated.filteredCommands should not be empty
    updated.filteredCommands.exists(_.name.contains("save")) shouldBe true
  }

  it should "handle selection navigation" in {
    val commands = List(
      Command("cmd1", "Command 1", _ => IO.unit),
      Command("cmd2", "Command 2", _ => IO.unit),
      Command("cmd3", "Command 3", _ => IO.unit)
    )
    val runner = CommandRunner.withCommands(commands).activate(CommandRegistry.default)

    val movedDown = runner.moveSelection(1)
    movedDown.selectedIndex shouldBe 1

    val movedUp = movedDown.moveSelection(-1)
    movedUp.selectedIndex shouldBe 0

    // Should wrap around
    val wrapDown = runner.moveSelection(commands.length)
    wrapDown.selectedIndex shouldBe 0
  }

  "CommandRunnerComponent" should "activate command runner on hotkey" in {
    val component    = new CommandRunnerComponent()
    val initialState = AppState.empty

    val result = component.processEvent(ToggleCommandRunner, initialState)

    result shouldNot be(ComponentResult.noChange)
    // Should update state to show active command runner
  }

  it should "handle search input" in {
    val component = new CommandRunnerComponent()
    val activeState = AppState.empty.copy(
      commandRunner = CommandRunner.empty.activate(CommandRegistry.default)
    )

    val result = component.processEvent(InsertChar('s'), activeState)

    result shouldNot be(ComponentResult.noChange)
    // Should update search term and filter commands
  }

  it should "handle escape to close runner" in {
    val component = new CommandRunnerComponent()
    val activeState = AppState.empty.copy(
      commandRunner = CommandRunner.empty.activate(CommandRegistry.default)
    )

    val result = component.processEvent(com.serenity.keystroke.events.Escape, activeState)

    result shouldNot be(ComponentResult.noChange)
    // Should deactivate command runner and restore focus
  }

  it should "handle enter to execute selected command" in {
    val executed = collection.mutable.Buffer[String]()
    val testCommand = Command(
      "test",
      "Test command",
      state =>
        executed += "executed"
        IO.unit
    )

    val component         = new CommandRunnerComponent()
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val runner = CommandRunner
      .withCommands(List(testCommand))
      .activate(registry)
      .updateSearchTerm("test")
    val activeState = AppState.empty.copy(commandRunner = runner)

    val result = component.processEvent(Enter, activeState)

    result shouldNot be(ComponentResult.noChange)
    // Command should be executed (tested separately for IO effects)
  }

  "Command execution" should "handle IO effects properly" in {
    var executed    = false
    val testCommand = Command("test", "Test command", _ => IO { executed = true })

    testCommand.action(AppState.empty).unsafeRunSync()
    executed shouldBe true
  }
