package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.command.*
import com.serenity.config.ToolbarDisplayMode
import com.serenity.keystroke.events.{Enter, InsertChar, ToggleCommandRunner}
import com.serenity.state.manager.StateManager
import com.serenity.state.models.SurfaceContent
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

/** TDD tests for toggleable UI commands functionality.
  *
  * Requirements:
  *   1. Command registry contains toggle commands for line numbers and gutter
  *   2. Commands can be found by search terms ("line", "gutter", "toggle")
  *   3. Executing toggle commands properly updates AppConfig
  *   4. Commands work correctly with current state (toggle on/off appropriately)
  *   5. Commands integrate properly with existing command runner system
  */
class ToggleUICommandsSpec extends AnyFlatSpec with Matchers:

  given com.serenity.rope.Balance = com.serenity.rope.Balance.default

  private def createStateManager(): StateManager =
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger(using LoggerName("ToggleUICommandsSpec"))
    StateManager.apply(logger).unsafeRunSync()

  private def executeCommandThroughRunner(
    stateManager: StateManager,
    searchTerm: String,
    expectedCommandName: String
  ): Unit =
    val beforeOpen = stateManager.getCurrentState.unsafeRunSync()
    if beforeOpen.commandRunnerSurface
          .flatMap {
            _.content match
              case SurfaceContent.CommandPalette(runner) => Some(runner.isActive)
              case _                                     => None
          }
          .getOrElse(false) == false
    then stateManager.applyEvent(ToggleCommandRunner).unsafeRunSync()
    searchTerm.foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())
    stateManager.getCurrentState.unsafeRunSync().commandRunnerSurface.flatMap {
      _.content match
        case SurfaceContent.CommandPalette(runner) => runner.selectedCommand.map(_.name)
        case _                                     => None
    } shouldBe Some(expectedCommandName)
    stateManager.applyEvent(Enter).unsafeRunSync()

  behavior of "Toggle Line Numbers Command"

  it should "be found in command registry by search terms" in {
    val registry = CommandRegistry.withToggleUI

    val lineResults   = registry.searchCommands("line")
    val numberResults = registry.searchCommands("numbers")
    val toggleResults = registry.searchCommands("toggle")
    val command       = registry.findCommand("toggle-line-numbers").get

    lineResults.map(_.name) should contain("toggle-line-numbers")
    numberResults.map(_.name) should contain("toggle-line-numbers")
    toggleResults.map(_.name) should contain("toggle-line-numbers")
    command.intent shouldBe CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.ToggleLineNumbers))
  }

  it should "toggle line numbers from enabled to disabled" in {
    val stateManager = createStateManager()

    val initialState = stateManager.getCurrentState.unsafeRunSync()
    initialState.persisted.config.surfaceConfig.showLineNumbers shouldBe true

    executeCommandThroughRunner(stateManager, "toggle-line-numbers", "toggle-line-numbers")

    val finalState = stateManager.getCurrentState.unsafeRunSync()
    finalState.persisted.config.surfaceConfig.showLineNumbers shouldBe false
  }

  it should "toggle line numbers from disabled to enabled" in {
    val stateManager = createStateManager()

    stateManager
      .updateState(s => s.copy(persisted = s.persisted.copy(config = s.persisted.config.withLineNumbers(false))))
      .unsafeRunSync()
    stateManager.getCurrentState.unsafeRunSync().persisted.config.surfaceConfig.showLineNumbers shouldBe false

    executeCommandThroughRunner(stateManager, "toggle-line-numbers", "toggle-line-numbers")

    val finalState = stateManager.getCurrentState.unsafeRunSync()
    finalState.persisted.config.surfaceConfig.showLineNumbers shouldBe true
  }

  behavior of "Toggle Gutter Command"

  it should "be found in command registry by search terms" in {
    val registry = CommandRegistry.withToggleUI

    val gutterResults = registry.searchCommands("gutter")
    val statusResults = registry.searchCommands("status")
    val toggleResults = registry.searchCommands("toggle")
    val command       = registry.findCommand("toggle-gutter").get

    gutterResults.map(_.name) should contain("toggle-gutter")
    statusResults.map(_.name) should contain("toggle-gutter")
    toggleResults.map(_.name) should contain("toggle-gutter")
    command.intent shouldBe CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.ToggleGutter))
  }

  it should "toggle gutter from enabled to disabled" in {
    val stateManager = createStateManager()

    stateManager.getCurrentState.unsafeRunSync().persisted.config.surfaceConfig.showGutter shouldBe true

    executeCommandThroughRunner(stateManager, "toggle-gutter", "toggle-gutter")

    val finalState = stateManager.getCurrentState.unsafeRunSync()
    finalState.persisted.config.surfaceConfig.showGutter shouldBe false
  }

  it should "toggle gutter from disabled to enabled" in {
    val stateManager = createStateManager()

    stateManager
      .updateState(s => s.copy(persisted = s.persisted.copy(config = s.persisted.config.withGutter(false))))
      .unsafeRunSync()
    stateManager.getCurrentState.unsafeRunSync().persisted.config.surfaceConfig.showGutter shouldBe false

    executeCommandThroughRunner(stateManager, "toggle-gutter", "toggle-gutter")

    val finalState = stateManager.getCurrentState.unsafeRunSync()
    finalState.persisted.config.surfaceConfig.showGutter shouldBe true
  }

  behavior of "Toggle Pane Headers Command"

  it should "be found in command registry by search terms" in {
    val registry = CommandRegistry.withToggleUI

    val paneResults   = registry.searchCommands("pane")
    val headerResults = registry.searchCommands("header")
    val command       = registry.findCommand("toggle-pane-headers").get

    paneResults.map(_.name) should contain("toggle-pane-headers")
    headerResults.map(_.name) should contain("toggle-pane-headers")
    command.intent shouldBe CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.TogglePaneHeaders))
  }

  it should "toggle pane headers from enabled to disabled" in {
    val stateManager = createStateManager()

    stateManager.getCurrentState.unsafeRunSync().persisted.config.surfaceConfig.showPaneHeaders shouldBe true

    executeCommandThroughRunner(stateManager, "toggle-pane-headers", "toggle-pane-headers")

    val finalState = stateManager.getCurrentState.unsafeRunSync()
    finalState.persisted.config.surfaceConfig.showPaneHeaders shouldBe false
  }

  it should "toggle pane headers from disabled to enabled" in {
    val stateManager = createStateManager()

    stateManager
      .updateState(s => s.copy(persisted = s.persisted.copy(config = s.persisted.config.withPaneHeaders(false))))
      .unsafeRunSync()
    stateManager.getCurrentState.unsafeRunSync().persisted.config.surfaceConfig.showPaneHeaders shouldBe false

    executeCommandThroughRunner(stateManager, "toggle-pane-headers", "toggle-pane-headers")

    val finalState = stateManager.getCurrentState.unsafeRunSync()
    finalState.persisted.config.surfaceConfig.showPaneHeaders shouldBe true
  }

  it should "have a descriptive command name and description" in {
    val registry      = CommandRegistry.withToggleUI
    val headerCommand = registry.findCommand("toggle-pane-headers").get

    headerCommand.name shouldBe "toggle-pane-headers"
    headerCommand.label shouldBe "Toggle Pane Headers"
    headerCommand.description should include("pane header")
  }

  behavior of "Combined Toggle UI Command Integration"

  it should "allow toggling both line numbers and gutter independently" in {
    val stateManager = createStateManager()

    val initialState = stateManager.getCurrentState.unsafeRunSync()
    initialState.persisted.config.surfaceConfig.showLineNumbers shouldBe true
    initialState.persisted.config.surfaceConfig.showGutter shouldBe true

    executeCommandThroughRunner(stateManager, "toggle-line-numbers", "toggle-line-numbers")

    val midState = stateManager.getCurrentState.unsafeRunSync()
    midState.persisted.config.surfaceConfig.showLineNumbers shouldBe false
    midState.persisted.config.surfaceConfig.showGutter shouldBe true
    midState.commandRunnerSurface shouldBe None

    executeCommandThroughRunner(stateManager, "toggle-gutter", "toggle-gutter")

    val finalState = stateManager.getCurrentState.unsafeRunSync()
    finalState.persisted.config.surfaceConfig.showLineNumbers shouldBe false
    finalState.persisted.config.surfaceConfig.showGutter shouldBe false
  }

  it should "have descriptive command names and descriptions" in {
    val registry      = CommandRegistry.withToggleUI
    val lineCommand   = registry.findCommand("toggle-line-numbers").get
    val gutterCommand = registry.findCommand("toggle-gutter").get

    lineCommand.name shouldBe "toggle-line-numbers"
    lineCommand.label shouldBe "Toggle Line Numbers"
    lineCommand.description should include("line numbers")

    gutterCommand.name shouldBe "toggle-gutter"
    gutterCommand.label shouldBe "Toggle Gutter"
    gutterCommand.description should include("gutter")
  }

  behavior of "Toggle Word Wrap Command"

  it should "expose a line wrap command alias for soft wrapping" in {
    val registry = CommandRegistry.withToggleUI

    val lineResults = registry.searchCommands("line wrap")
    val wrapResults = registry.searchCommands("wrap")
    val command     = registry.findCommand("toggle-line-wrap").get

    lineResults.map(_.name) should contain("toggle-line-wrap")
    wrapResults.map(_.name) should contain("toggle-line-wrap")
    command.intent shouldBe CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.ToggleWordWrap))
  }

  it should "toggle soft line wrapping through the line wrap command" in {
    val stateManager = createStateManager()

    stateManager.getCurrentState.unsafeRunSync().persisted.config.surfaceConfig.wordWrapEnabled shouldBe true

    executeCommandThroughRunner(stateManager, "toggle-line-wrap", "toggle-line-wrap")

    stateManager.getCurrentState.unsafeRunSync().persisted.config.surfaceConfig.wordWrapEnabled shouldBe false
  }

  it should "be found in command registry by search terms" in {
    val registry = CommandRegistry.withToggleUI

    val wordResults = registry.searchCommands("word")
    val wrapResults = registry.searchCommands("wrap")
    val command     = registry.findCommand("toggle-word-wrap").get

    wordResults.map(_.name) should contain("toggle-word-wrap")
    wrapResults.map(_.name) should contain("toggle-word-wrap")
    command.intent shouldBe CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.ToggleWordWrap))
  }

  it should "toggle text body focus from disabled to enabled" in {
    val stateManager = createStateManager()

    stateManager.getCurrentState.unsafeRunSync().persisted.config.surfaceConfig.focusedTextBodyEnabled shouldBe false

    executeCommandThroughRunner(stateManager, "toggle-text-body-focus", "toggle-text-body-focus")

    stateManager.getCurrentState.unsafeRunSync().persisted.config.surfaceConfig.focusedTextBodyEnabled shouldBe true
  }

  it should "open the contextual toolbar without toggling the setting" in {
    val stateManager = createStateManager()

    stateManager.getCurrentState.unsafeRunSync().persisted.config.surfaceConfig.contextualToolbarEnabled shouldBe true

    executeCommandThroughRunner(stateManager, "toggle-contextual-toolbar", "toggle-contextual-toolbar")

    val finalState = stateManager.getCurrentState.unsafeRunSync()
    finalState.persisted.config.surfaceConfig.contextualToolbarEnabled shouldBe true
    finalState.contextualToolbarSurface shouldBe defined
    finalState.commandRunnerSurface shouldBe None
  }

  it should "set the command runner's persistent key-hint footer explicitly from a stateful option" in {
    val stateManager = createStateManager()

    stateManager.getCurrentState
      .unsafeRunSync()
      .persisted
      .config
      .surfaceConfig
      .commandRunnerShowKeyHints shouldBe true

    stateManager
      .executeCommand(
        Command.typed(
          "command-runner-key-hints-off",
          "Set command runner key hints off",
          CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetCommandRunnerShowKeyHints(false))),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()

    stateManager.getCurrentState
      .unsafeRunSync()
      .persisted
      .config
      .surfaceConfig
      .commandRunnerShowKeyHints shouldBe false

    stateManager
      .executeCommand(
        Command.typed(
          "command-runner-key-hints-on",
          "Set command runner key hints on",
          CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetCommandRunnerShowKeyHints(true))),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()

    stateManager.getCurrentState
      .unsafeRunSync()
      .persisted
      .config
      .surfaceConfig
      .commandRunnerShowKeyHints shouldBe true
  }

  it should "toggle word wrap from enabled to disabled" in {
    val stateManager = createStateManager()

    stateManager.getCurrentState.unsafeRunSync().persisted.config.surfaceConfig.wordWrapEnabled shouldBe true

    executeCommandThroughRunner(stateManager, "toggle-word-wrap", "toggle-word-wrap")

    stateManager.getCurrentState.unsafeRunSync().persisted.config.surfaceConfig.wordWrapEnabled shouldBe false
  }

  it should "set text display settings explicitly from stateful options" in {
    val stateManager = createStateManager()

    stateManager
      .executeCommand(
        Command.typed(
          "line-numbers-off",
          "Set line numbers off",
          CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetLineNumbers(false))),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()
    stateManager
      .executeCommand(
        Command.typed(
          "gutter-off",
          "Set gutter off",
          CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetGutter(false))),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()
    stateManager
      .executeCommand(
        Command.typed(
          "word-wrap-off",
          "Set word wrap off",
          CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetWordWrap(false))),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()
    stateManager
      .executeCommand(
        Command.typed(
          "focused-body-on",
          "Set focused text body on",
          CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetFocusedTextBody(true))),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()
    stateManager
      .executeCommand(
        Command.typed(
          "contextual-toolbar-off",
          "Set contextual toolbar off",
          CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetContextualToolbarEnabled(false))),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()
    stateManager
      .executeCommand(
        Command.typed(
          "contextual-toolbar-text-only",
          "Set contextual toolbar display to text only",
          CommandIntent.Settings(
            SettingsIntent.PanelChrome(PanelChromeIntent.SetContextualToolbarDisplayMode(ToolbarDisplayMode.TextOnly))
          ),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()

    val disabledState = stateManager.getCurrentState.unsafeRunSync()
    disabledState.persisted.config.surfaceConfig.showLineNumbers shouldBe false
    disabledState.persisted.config.surfaceConfig.showGutter shouldBe false
    disabledState.persisted.config.surfaceConfig.wordWrapEnabled shouldBe false
    disabledState.persisted.config.surfaceConfig.focusedTextBodyEnabled shouldBe true
    disabledState.persisted.config.surfaceConfig.contextualToolbarEnabled shouldBe false
    disabledState.persisted.config.surfaceConfig.contextualToolbarDisplayMode shouldBe ToolbarDisplayMode.TextOnly

    stateManager
      .executeCommand(
        Command
          .typed(
            "line-numbers-on",
            "Set line numbers on",
            CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetLineNumbers(true))),
            CommandCategory.Settings
          )
      )
      .unsafeRunSync()
    stateManager
      .executeCommand(
        Command.typed(
          "gutter-on",
          "Set gutter on",
          CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetGutter(true))),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()
    stateManager
      .executeCommand(
        Command.typed(
          "word-wrap-on",
          "Set word wrap on",
          CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetWordWrap(true))),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()
    stateManager
      .executeCommand(
        Command.typed(
          "focused-body-off",
          "Set focused text body off",
          CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetFocusedTextBody(false))),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()
    stateManager
      .executeCommand(
        Command.typed(
          "contextual-toolbar-on",
          "Set contextual toolbar on",
          CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetContextualToolbarEnabled(true))),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()
    stateManager
      .executeCommand(
        Command.typed(
          "contextual-toolbar-icon-text",
          "Set contextual toolbar display to icon and text",
          CommandIntent.Settings(
            SettingsIntent.PanelChrome(
              PanelChromeIntent.SetContextualToolbarDisplayMode(ToolbarDisplayMode.IconAndText)
            )
          ),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()

    val enabledState = stateManager.getCurrentState.unsafeRunSync()
    enabledState.persisted.config.surfaceConfig.showLineNumbers shouldBe true
    enabledState.persisted.config.surfaceConfig.showGutter shouldBe true
    enabledState.persisted.config.surfaceConfig.wordWrapEnabled shouldBe true
    enabledState.persisted.config.surfaceConfig.focusedTextBodyEnabled shouldBe false
    enabledState.persisted.config.surfaceConfig.contextualToolbarEnabled shouldBe true
    enabledState.persisted.config.surfaceConfig.contextualToolbarDisplayMode shouldBe ToolbarDisplayMode.IconAndText
  }
