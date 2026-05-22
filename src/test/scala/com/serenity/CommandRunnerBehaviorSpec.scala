package com.serenity

import cats.effect.IO
import com.serenity.command.{Command, CommandRegistry, CommandRunner}
import com.serenity.keystroke.events.*
import com.serenity.state.components.{CommandRunnerComponent, ComponentResult}
import com.serenity.state.models.*
import com.serenity.ui.layout.Layout
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class CommandRunnerBehaviorSpec extends AnyFunSpec with Matchers:

  describe("Command runner navigation and execution"):
    it("should activate and gain focus when toggled"):
      val registry  = CommandRegistry.default
      val component = CommandRunnerComponent(registry)

      val initialState = AppState(
        buffers = Map.empty,
        layout = Layout.empty,
        focus = Focus.EditorPane(PaneId(1)),
        commandRunner = CommandRunner.empty
      )

      val result = component.processEvent(ToggleCommandRunner, initialState)
      result match
        case ComponentResult.StateChange(update) =>
          val newState = update(initialState)

          // Should activate command runner and change focus
          newState.commandRunner.isActive shouldEqual true
          newState.focus shouldEqual Focus.CommandRunner
          newState.commandRunner.previousFocus shouldEqual Some(Focus.EditorPane(PaneId(1)))
        case _ => fail("Expected state change")

    it("should deactivate and restore previous focus when escaped"):
      val registry  = CommandRegistry.default
      val component = CommandRunnerComponent(registry)

      val activeRunner = CommandRunner.empty
        .activate(registry)
        .withPreviousFocus(Focus.EditorPane(PaneId(2)))

      val initialState = AppState(
        buffers = Map.empty,
        layout = Layout.empty,
        focus = Focus.CommandRunner,
        commandRunner = activeRunner
      )

      val result = component.processEvent(Escape, initialState)
      result match
        case ComponentResult.StateChange(update) =>
          val newState = update(initialState)

          // Should deactivate and restore focus
          newState.commandRunner.isActive shouldEqual false
          newState.focus shouldEqual Focus.EditorPane(PaneId(2))
        case _ => fail("Expected state change")

    it("should navigate up and down through command list"):
      val commands = List(
        Command("first", "First command", _ => IO.unit),
        Command("second", "Second command", _ => IO.unit),
        Command("third", "Third command", _ => IO.unit)
      )
      val registry  = CommandRegistry(commands)
      val component = CommandRunnerComponent(registry)

      val activeRunner = CommandRunner.empty.activate(registry)
      val initialState = AppState(
        buffers = Map.empty,
        layout = Layout.empty,
        focus = Focus.CommandRunner,
        commandRunner = activeRunner
      )

      // Initial selection should be first command
      initialState.commandRunner.selectedIndex shouldEqual 0
      initialState.commandRunner.selectedCommand.map(_.name) shouldEqual Some("first")

      // Move down once
      val downResult = component.processEvent(MoveDown, initialState)
      downResult match
        case ComponentResult.StateChange(update) =>
          val newState = update(initialState)
          newState.commandRunner.selectedIndex shouldEqual 1
          newState.commandRunner.selectedCommand.map(_.name) shouldEqual Some("second")

          // Move down again
          val downResult2 = component.processEvent(MoveDown, newState)
          downResult2 match
            case ComponentResult.StateChange(update2) =>
              val newState2 = update2(newState)
              newState2.commandRunner.selectedIndex shouldEqual 2
              newState2.commandRunner.selectedCommand.map(_.name) shouldEqual Some("third")
            case _ => fail("Expected state change")
        case _ => fail("Expected state change")

    it("should wrap navigation at boundaries"):
      val commands = List(
        Command("first", "First command", _ => IO.unit),
        Command("second", "Second command", _ => IO.unit)
      )
      val registry  = CommandRegistry(commands)
      val component = CommandRunnerComponent(registry)

      val activeRunner = CommandRunner.empty.activate(registry)
      val initialState = AppState(
        buffers = Map.empty,
        layout = Layout.empty,
        focus = Focus.CommandRunner,
        commandRunner = activeRunner
      )

      // Move up from first item should wrap to last
      val upResult = component.processEvent(MoveUp, initialState)
      upResult match
        case ComponentResult.StateChange(update) =>
          val newState = update(initialState)
          newState.commandRunner.selectedIndex shouldEqual 1
          newState.commandRunner.selectedCommand.map(_.name) shouldEqual Some("second")
        case _ => fail("Expected state change")

    it("should filter commands when typing"):
      val commands = List(
        Command("save", "Save file", _ => IO.unit),
        Command("search", "Search text", _ => IO.unit),
        Command("open", "Open file", _ => IO.unit)
      )
      val registry  = CommandRegistry(commands)
      val component = CommandRunnerComponent(registry)

      val activeRunner = CommandRunner.empty.activate(registry)
      val initialState = AppState(
        buffers = Map.empty,
        layout = Layout.empty,
        focus = Focus.CommandRunner,
        commandRunner = activeRunner
      )

      // Type 's' to filter
      val typeResult = component.processEvent(InsertChar('s'), initialState)
      typeResult match
        case ComponentResult.StateChange(update) =>
          val newState = update(initialState)

          newState.commandRunner.searchTerm shouldEqual "s"
          newState.commandRunner.filteredCommands should have size 2 // save, search
          newState.commandRunner.filteredCommands.map(_.name) should contain allOf ("save", "search")
          newState.commandRunner.selectedIndex shouldEqual 0 // Reset to first filtered item
        case _ => fail("Expected state change")

    it("should execute selected command when enter is pressed"):
      var executionCalled = false
      val commands = List(
        Command("test", "Test command", _ => IO { executionCalled = true })
      )
      val registry  = CommandRegistry(commands)
      val component = CommandRunnerComponent(registry)

      val activeRunner = CommandRunner.empty
        .activate(registry)
        .withPreviousFocus(Focus.EditorPane(PaneId(1)))

      val initialState = AppState(
        buffers = Map.empty,
        layout = Layout.empty,
        focus = Focus.CommandRunner,
        commandRunner = activeRunner
      )

      val result = component.processEvent(Enter, initialState)
      result match
        case ComponentResult.StateChange(update) =>
          val newState = update(initialState)

          // Should execute command and close runner
          executionCalled shouldEqual true
          newState.commandRunner.isActive shouldEqual false
          newState.focus shouldEqual Focus.EditorPane(PaneId(1))
        case _ => fail("Expected state change")

    it("should handle backspace in search term"):
      val registry  = CommandRegistry.default
      val component = CommandRunnerComponent(registry)

      val activeRunner = CommandRunner.empty.activate(registry).updateSearchTerm("test")(using registry)
      val initialState = AppState(
        buffers = Map.empty,
        layout = Layout.empty,
        focus = Focus.CommandRunner,
        commandRunner = activeRunner
      )

      initialState.commandRunner.searchTerm shouldEqual "test"

      val result = component.processEvent(DeleteBackward, initialState)
      result match
        case ComponentResult.StateChange(update) =>
          val newState = update(initialState)
          newState.commandRunner.searchTerm shouldEqual "tes"
        case _ => fail("Expected state change")
