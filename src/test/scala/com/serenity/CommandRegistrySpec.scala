package com.serenity

import com.serenity.command.*
import com.serenity.rope.Balance
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CommandRegistrySpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  "CommandRegistry" should "register and find commands" in {
    val registry = CommandRegistry.default
    val commands = registry.getAllCommands

    commands should not be empty
    commands.exists(_.name == "save") shouldBe true
    commands.exists(_.name == "open") shouldBe true
    registry.findCommand("open").map(_.label) shouldBe Some("Open File")
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

  it should "search commands by human-facing label" in {
    val registry       = CommandRegistry.default
    val saveAsCommands = registry.searchCommands("save as")

    saveAsCommands.map(_.name) should contain("save-as")
  }

  "CommandSearcher" should "filter commands based on search term" in {
    val commands = List(
      Command.typed("save", "Save current file", CommandIntent.File(FileIntent.SaveCurrentFile)),
      Command.typed("save-as", "Save file with new name", CommandIntent.File(FileIntent.SaveCurrentFileAs)),
      Command.typed("open", "Open file", CommandIntent.File(FileIntent.OpenFile)),
      Command.typed("quit", "Quit application", CommandIntent.Lifecycle(LifecycleIntent.QuitApp))
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
      Command.typed("save", "Save current file", CommandIntent.File(FileIntent.SaveCurrentFile)),
      Command.typed("save-as", "Save file with new name", CommandIntent.File(FileIntent.SaveCurrentFileAs)),
      Command.typed(
        "auto-save",
        "Enable auto save",
        CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.ToggleLineNumbers))
      )
    )

    val searcher = new CommandSearcher(commands)
    val results  = searcher.search("save")

    // "save" should come before "save-as" and "auto-save" due to exact match
    results.head.name shouldBe "save"
  }

  it should "rank commands deterministically when every search token matches" in {
    val commands = List(
      Command.typed("open-file", "Open a file from disk", CommandIntent.File(FileIntent.OpenFile)),
      Command.typed("open-recent", "Open a recent workspace file", CommandIntent.File(FileIntent.OpenFileSearch)),
      Command.typed("close-file", "Close the current file", CommandIntent.File(FileIntent.CloseCurrentFile))
    )

    val results = new CommandSearcher(commands).search("open file")

    results.map(_.name) shouldBe List("open-file", "open-recent")
  }

  it should "limit results to specified count" in {
    val commands =
      (1 to 10).map(i => Command.typed(s"cmd$i", s"Command $i", CommandIntent.Theme(ThemeIntent.ToggleTheme))).toList
    val searcher = new CommandSearcher(commands)

    val results = searcher.search("cmd", maxResults = 5)
    results.length shouldBe 5
  }

  it should "browse commands by category when search is empty" in {
    val registry = CommandRegistry.default

    val fileCommands     = registry.commandsForCategory(CommandCategory.File)
    val settingsCommands = registry.commandsForCategory(CommandCategory.Settings)

    fileCommands should not be empty
    fileCommands.map(_.category).distinct shouldBe List(CommandCategory.File)
    settingsCommands.exists(_.name == "toggle-theme") shouldBe true
  }

  it should "reuse categorized command lists from the registry" in {
    val registry = CommandRegistry.default

    registry.commandsForCategory(CommandCategory.File) shouldBe theSameInstanceAs(
      registry.commandsForCategory(CommandCategory.File)
    )
  }

  it should "omit redundant top-level typography toggle commands" in {
    val registry     = CommandRegistry.default
    val commandNames = registry.getAllCommands.map(_.name)

    commandNames should not contain "increase-font-size"
    commandNames should not contain "decrease-font-size"
    commandNames should not contain "toggle-ligatures"
  }

  it should "include session persistence commands in the file category" in {
    val registry = CommandRegistry.default

    val fileCommandNames = registry.commandsForCategory(CommandCategory.File).map(_.name)

    fileCommandNames should contain allOf ("save-session", "restore-session", "clear-session")
    registry.findCommand("save-session").map(_.intent) shouldBe Some(CommandIntent.Session(SessionIntent.SaveSession))
    registry.findCommand("restore-session").map(_.intent) shouldBe Some(
      CommandIntent.Session(SessionIntent.RestoreSession)
    )
    registry.findCommand("clear-session").map(_.intent) shouldBe Some(CommandIntent.Session(SessionIntent.ClearSession))
  }
