package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.command.{Command, CommandIntent, CommandRegistry, CommandRunner}
import com.serenity.keystroke.events.*
import com.serenity.state.models.*
import com.serenity.state.reducers.CommandRunnerReducer
import com.serenity.ui.layout.Layout
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CommandRunnerReducerSpec extends AnyFlatSpec with Matchers:

  private def activeState(registry: CommandRegistry): AppState =
    AppState(
      buffers = Map.empty,
      layout = Layout.empty,
      focus = Focus.CommandRunner,
      commandRunner = CommandRunner.empty.activate(registry).withPreviousFocus(Focus.EditorPane(PaneId(2)))
    )

  "CommandRunnerReducer" should "activate the command runner from an editor focus" in {
    val registry = CommandRegistry.default
    val initialState = AppState(
      buffers = Map.empty,
      layout = Layout.empty,
      focus = Focus.EditorPane(PaneId(1)),
      commandRunner = CommandRunner.empty
    )

    val result = CommandRunnerReducer.reduce(ToggleCommandRunner, initialState, registry)

    result.state.focus shouldBe Focus.CommandRunner
    result.state.commandRunner.isActive shouldBe true
    result.state.commandRunner.previousFocus shouldBe Some(Focus.EditorPane(PaneId(1)))
  }

  it should "filter commands when typing and execute the selection on enter" in {
    var executionCalled = false
    val command  = Command("test", "Test command", _ => IO { executionCalled = true })
    val registry = CommandRegistry(List(command))
    val state    = activeState(registry)

    val typed = CommandRunnerReducer.reduce(InsertChar('t'), state, registry)
    typed.state.commandRunner.searchTerm shouldBe "t"
    typed.state.commandRunner.filteredCommands.map(_.name) shouldBe List("test")

    val executed = CommandRunnerReducer.reduce(Enter, typed.state, registry)
    executed.effects should have size 1
    executed.effects.head match
      case com.serenity.state.reducers.AppEffect.ExecuteCommand(commandToRun) =>
        commandToRun.execute(typed.state).unsafeRunSync()
        executionCalled shouldBe true
      case other =>
        fail(s"Expected ExecuteCommand effect, got $other")

    executed.state.commandRunner.isActive shouldBe false
    executed.state.focus shouldBe Focus.EditorPane(PaneId(2))
  }

  it should "surface typed command intents through execute effects" in {
    val command  = Command.typed("toggle-line-numbers", "Toggle line numbers display on/off", CommandIntent.ToggleLineNumbers)
    val registry = CommandRegistry(List(command))
    val state    = activeState(registry)

    val executed = CommandRunnerReducer.reduce(Enter, state, registry)

    executed.effects should have size 1
    executed.effects.head match
      case com.serenity.state.reducers.AppEffect.ExecuteCommand(commandToRun) =>
        commandToRun.intent shouldBe CommandIntent.ToggleLineNumbers
      case other =>
        fail(s"Expected ExecuteCommand effect, got $other")
  }
