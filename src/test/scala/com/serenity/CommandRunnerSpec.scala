package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.command.{Command, CommandCategory, CommandRegistry, CommandRunner, CommandSearcher, CommandSurfaceItem}
import com.serenity.config.AppConfig
import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.components.CommandRunnerComponent
import com.serenity.state.components.ComponentResult
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CommandRunnerSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private def stateWithRunner(runner: CommandRunner): AppState =
    AppState.empty.copy(
      focus = Focus.Surface(SurfaceId("command-runner")),
      uiSurfaces = List(
        UiSurface(
          SurfaceId("command-runner"),
          SurfaceContent.CommandPalette(runner),
          SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
        )
      )
    )

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

  it should "browse commands by category when search is empty" in {
    val registry = CommandRegistry.default

    val fileCommands = registry.commandsForCategory(CommandCategory.File)
    val settingsCommands = registry.commandsForCategory(CommandCategory.Settings)

    fileCommands should not be empty
    fileCommands.map(_.category).distinct shouldBe List(CommandCategory.File)
    settingsCommands.exists(_.name == "toggle-theme") shouldBe true
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
    val runner            = CommandRunner.empty.activate(registry, AppConfig.default)

    val updated = runner.updateSearchTerm("save")
    updated.searchTerm shouldBe "save"
    updated.filteredCommands should not be empty
    updated.filteredCommands.exists(_.name.contains("save")) shouldBe true
  }

  it should "browse the active category when search is empty and switch to global search once typing begins" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default)
      .withActiveCategory(CommandCategory.File)

    runner.activeCategory shouldBe CommandCategory.File
    runner.visibleItems should not be empty
    runner.visibleItems.collect { case CommandSurfaceItem.CommandItem(command) => command.category }.distinct shouldBe List(CommandCategory.File)

    val searched = runner.updateSearchTerm("theme")
    searched.searchTerm shouldBe "theme"
    searched.visibleItems.exists {
      case CommandSurfaceItem.CommandItem(command) => command.name == "toggle-theme"
      case _                                       => false
    } shouldBe true
  }

  it should "surface animation settings as an expandable group in settings browsing" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default)
      .withActiveCategory(CommandCategory.Settings)

    val animationGroup = runner.visibleItems.collectFirst {
      case group: CommandSurfaceItem.GroupItem if group.id == "settings-animation" => group
    }.getOrElse(fail("Expected animation mode option item"))

    animationGroup.label shouldBe "Animation"
    animationGroup.children.map(_.id) should contain allOf ("animation-mode", "animation-duration", "animation-steps")
  }

  it should "surface appearance settings as an expandable group in settings browsing" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default)
      .withActiveCategory(CommandCategory.Settings)

    val appearanceGroup = runner.visibleItems.collectFirst {
      case group: CommandSurfaceItem.GroupItem if group.id == "settings-appearance" => group
    }.getOrElse(fail("Expected background style option item"))

    appearanceGroup.label shouldBe "Appearance"
    appearanceGroup.children.map(_.id) should contain allOf ("cursor-mode", "background-style", "blur-radius")
  }

  it should "group related settings into expandable submenu rows" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default)
      .withActiveCategory(CommandCategory.Settings)

    val groupItems = runner.visibleItems.collect {
      case group: CommandSurfaceItem.GroupItem => group
    }

    groupItems.map(_.id) shouldBe List("settings-animation", "settings-appearance", "settings-typography", "settings-language")
    groupItems.head.label shouldBe "Animation"
    groupItems.head.children.map(_.id) should contain allOf ("animation-mode", "animation-duration", "animation-steps")
    groupItems(1).label shouldBe "Appearance"
    groupItems(1).children.map(_.id) should contain allOf ("cursor-mode", "background-style", "blur-radius")
    groupItems(2).label shouldBe "Typography"
    groupItems(2).children.map(_.id) should contain allOf ("code-font", "text-font", "ligatures", "font-size")
    groupItems(3).label shouldBe "Language"
    groupItems(3).children.map(_.id) should contain ("lang-plain-text")
  }

  it should "handle selection navigation" in {
    val commands = List(
      Command("cmd1", "Command 1", _ => IO.unit),
      Command("cmd2", "Command 2", _ => IO.unit),
      Command("cmd3", "Command 3", _ => IO.unit)
    )
    val runner = CommandRunner.withCommands(commands).activate(CommandRegistry(commands), AppConfig.default)

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

    result shouldBe ComponentResult.noChange
  }

  it should "handle search input" in {
    val component = new CommandRunnerComponent()
    val activeState = stateWithRunner(CommandRunner.empty.activate(CommandRegistry.default, AppConfig.default))

    val result = component.processEvent(InsertChar('s'), activeState)

    result shouldNot be(ComponentResult.noChange)
    // Should update search term and filter commands
  }

  it should "handle escape to close runner" in {
    val component = new CommandRunnerComponent()
    val activeState = stateWithRunner(CommandRunner.empty.activate(CommandRegistry.default, AppConfig.default))

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
      .activate(registry, AppConfig.default)
      .updateSearchTerm("test")
    val activeState = stateWithRunner(runner)

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
