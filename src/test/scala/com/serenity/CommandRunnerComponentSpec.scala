package com.serenity

import com.serenity.command.*
import com.serenity.config.*
import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.components.{CommandRunnerComponent, ComponentResult}
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CommandRunnerComponentSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private def stateWithRunner(runner: CommandRunner): AppState =
    AppState.empty.copy(
      persisted = AppState.empty.persisted.copy(focus = Focus.Surface(SurfaceId("command-runner"))),
      runtime = AppState.empty.runtime.copy(
        uiSurfaces = List(
          UiSurface(
            SurfaceId("command-runner"),
            SurfaceContent.CommandPalette(runner),
            SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
          )
        )
      )
    )

  "CommandRunnerComponent" should "activate command runner on hotkey" in {
    val component    = new CommandRunnerComponent()
    val initialState = AppState.empty

    val result = component.processEvent(ToggleCommandRunner, initialState)

    result shouldBe ComponentResult.noChange
  }

  it should "handle search input" in {
    val component   = new CommandRunnerComponent()
    val activeState = stateWithRunner(CommandRunner.empty.activate(CommandRegistry.default, AppConfig.default))

    val result = component.processEvent(InsertChar('s'), activeState)

    result shouldNot be(ComponentResult.noChange)
    // Should update search term and filter commands
  }

  it should "handle escape to close runner" in {
    val component   = new CommandRunnerComponent()
    val activeState = stateWithRunner(CommandRunner.empty.activate(CommandRegistry.default, AppConfig.default))

    val result = component.processEvent(com.serenity.keystroke.events.Escape, activeState)

    result shouldNot be(ComponentResult.noChange)
    // Should deactivate command runner and restore focus
  }

  it should "handle enter to execute selected command" in {
    val testCommand = Command.typed(
      "test",
      "Test command",
      CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.ToggleLineNumbers))
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
  }

  "Command model" should "carry typed intents without a custom execution escape hatch" in {
    val testCommand = Command.typed(
      "test",
      "Test command",
      CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.ToggleLineNumbers))
    )

    testCommand.intent shouldBe CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.ToggleLineNumbers))
  }
