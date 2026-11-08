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

  private def stateAfter(result: ComponentResult, before: AppState): AppState = result match
    case ComponentResult.StateChange(update) => update(before)
    case ComponentResult.ReducerUpdate(done) => done.state
    case ComponentResult.Composite(results)  => results.foldLeft(before)((state, next) => stateAfter(next, state))
    case _                                   => before

  private def runnerIn(state: AppState): Option[CommandRunner] =
    state.commandRunnerSurface.collect { case UiSurface(_, SurfaceContent.CommandPalette(runner), _, _) => runner }

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
    runnerIn(stateAfter(result, activeState)).map(_.searchTerm) shouldBe Some("s")
  }

  it should "handle escape to close runner" in {
    val component   = new CommandRunnerComponent()
    val activeState = stateWithRunner(CommandRunner.empty.activate(CommandRegistry.default, AppConfig.default))

    val result = component.processEvent(com.serenity.keystroke.events.Escape, activeState)

    result shouldNot be(ComponentResult.noChange)
    runnerIn(stateAfter(result, activeState)).exists(_.isActive) shouldBe false
  }

  it should "handle enter to execute selected command" in {
    val testCommand = Command.typed(
      "test",
      "Test command",
      CommandIntent.Settings(SettingsIntent.TextDisplay(TextDisplayIntent.ToggleLineNumbers))
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
      CommandIntent.Settings(SettingsIntent.TextDisplay(TextDisplayIntent.ToggleLineNumbers))
    )

    testCommand.intent shouldBe CommandIntent.Settings(SettingsIntent.TextDisplay(TextDisplayIntent.ToggleLineNumbers))
  }
