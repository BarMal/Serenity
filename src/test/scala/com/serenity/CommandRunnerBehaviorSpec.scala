package com.serenity

import com.serenity.command.*
import com.serenity.config.AppConfig
import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.components.{CommandRunnerComponent, ComponentResult}
import com.serenity.state.models.*
import com.serenity.state.reducers.AppEventReducer
import com.serenity.ui.layout.Layout
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class CommandRunnerBehaviorSpec extends AnyFunSpec with Matchers:

  given Balance = Balance.default

  private def runnerState(
    registry: CommandRegistry,
    runner: CommandRunner,
    focus: Focus
  ): AppState =
    AppState(
      buffers = Map.empty,
      layout = Layout.empty,
      focus = focus,
      uiSurfaces = List(
        UiSurface(
          SurfaceId("command-runner"),
          SurfaceContent.CommandPalette(runner),
          SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
        )
      )
    )

  private def runnerFrom(state: AppState): CommandRunner =
    state.commandRunnerSurface.flatMap {
      _.content match
        case SurfaceContent.CommandPalette(runner) => Some(runner)
        case _                                     => None
    }.get

  describe("Command runner navigation and execution"):
    it("should activate and gain focus when toggled"):
      val registry     = CommandRegistry.default
      val initialState = runnerState(registry, CommandRunner.empty, Focus.EditorPane(PaneId(1)))

      val result   = AppEventReducer.reduce(ToggleCommandRunner, initialState, registry)
      val newState = result.state
      val runner   = runnerFrom(newState)

      runner.isActive shouldEqual true
      newState.focus shouldEqual Focus.Surface(SurfaceId("command-runner"))
      newState.focusHistory should contain(Focus.EditorPane(PaneId(1)))

    it("should replace an open find modal when toggled"):
      val registry = CommandRegistry.default
      val initialState = AppState.initial.copy(
        uiSurfaces = List(
          UiSurface(
            SurfaceId("find"),
            SurfaceContent.ModalWorkflow(Modal.Find("needle", Nil, 0)),
            SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
          )
        ),
        focus = Focus.Surface(SurfaceId("find")),
        focusHistory = List(Focus.EditorPane(PaneId(0)))
      )

      val result   = AppEventReducer.reduce(ToggleCommandRunner, initialState, registry)
      val newState = result.state

      newState.modalSurface shouldBe None
      newState.commandRunnerSurface shouldBe defined
      newState.commandRunnerSurface.map(surface => Focus.Surface(surface.id)) shouldBe Some(newState.focus)

    it("should deactivate and restore previous focus when escaped"):
      val registry  = CommandRegistry.default
      val component = CommandRunnerComponent(registry)
      val activeRunner = CommandRunner.empty
        .activate(registry, AppConfig.default)

      val initialState = runnerState(registry, activeRunner, Focus.Surface(SurfaceId("command-runner")))
        .copy(focusHistory = List(Focus.EditorPane(PaneId(2))))

      component.processEvent(Escape, initialState) match
        case ComponentResult.StateChange(update) =>
          val newState = update(initialState)
          newState.commandRunnerSurface shouldBe None
          newState.focus shouldEqual Focus.EditorPane(PaneId(2))
        case _ =>
          fail("Expected state change")

    it("should navigate up and down through command list"):
      val commands = List(
        Command.typed("first", "First command", CommandIntent.ToggleTheme),
        Command.typed("second", "Second command", CommandIntent.ToggleLineNumbers),
        Command.typed("third", "Third command", CommandIntent.ToggleGutter)
      )
      val registry  = CommandRegistry(commands)
      val component = CommandRunnerComponent(registry)
      val initialState = runnerState(
        registry,
        CommandRunner.empty.activate(registry, AppConfig.default),
        Focus.Surface(SurfaceId("command-runner"))
      )

      runnerFrom(initialState).selectedIndex shouldEqual 0
      runnerFrom(initialState).selectedCommand.map(_.name) shouldEqual Some("first")

      component.processEvent(MoveDown, initialState) match
        case ComponentResult.StateChange(update) =>
          val newState = update(initialState)
          runnerFrom(newState).selectedIndex shouldEqual 1
          runnerFrom(newState).selectedCommand.map(_.name) shouldEqual Some("second")

          component.processEvent(MoveDown, newState) match
            case ComponentResult.StateChange(update2) =>
              val newState2 = update2(newState)
              runnerFrom(newState2).selectedIndex shouldEqual 2
              runnerFrom(newState2).selectedCommand.map(_.name) shouldEqual Some("third")
            case _ =>
              fail("Expected state change")
        case _ =>
          fail("Expected state change")

    it("should wrap navigation at boundaries"):
      val commands = List(
        Command.typed("first", "First command", CommandIntent.ToggleTheme),
        Command.typed("second", "Second command", CommandIntent.ToggleLineNumbers)
      )
      val registry  = CommandRegistry(commands)
      val component = CommandRunnerComponent(registry)
      val initialState = runnerState(
        registry,
        CommandRunner.empty.activate(registry, AppConfig.default),
        Focus.Surface(SurfaceId("command-runner"))
      )

      component.processEvent(MoveUp, initialState) match
        case ComponentResult.StateChange(update) =>
          val newState = update(initialState)
          runnerFrom(newState).selectedIndex shouldEqual 1
          runnerFrom(newState).selectedCommand.map(_.name) shouldEqual Some("second")
        case _ =>
          fail("Expected state change")

    it("should filter commands when typing"):
      val commands = List(
        Command.typed("save", "Save file", CommandIntent.SaveCurrentFile),
        Command.typed("search", "Search text", CommandIntent.FindInCurrentFile),
        Command.typed("open", "Open file", CommandIntent.OpenFile)
      )
      val registry  = CommandRegistry(commands)
      val component = CommandRunnerComponent(registry)
      val initialState = runnerState(
        registry,
        CommandRunner.empty.activate(registry, AppConfig.default),
        Focus.Surface(SurfaceId("command-runner"))
      )

      component.processEvent(InsertChar('s'), initialState) match
        case ComponentResult.StateChange(update) =>
          val newState = update(initialState)
          val runner   = runnerFrom(newState)

          runner.searchTerm shouldEqual "s"
          runner.filteredCommands should have size 2
          runner.filteredCommands.map(_.name) should contain allOf ("save", "search")
          runner.selectedIndex shouldEqual 0
        case _ =>
          fail("Expected state change")

    it("should execute selected command when enter is pressed"):
      val commands = List(
        Command.typed("test", "Test command", CommandIntent.ToggleLineNumbers)
      )
      val registry  = CommandRegistry(commands)
      val component = CommandRunnerComponent(registry)
      val runner    = CommandRunner.empty.activate(registry, AppConfig.default)
      val initialState = runnerState(registry, runner, Focus.Surface(SurfaceId("command-runner")))
        .copy(focusHistory = List(Focus.EditorPane(PaneId(1))))

      component.processEvent(Enter, initialState) match
        case ComponentResult.Composite(
              List(ComponentResult.StateChange(update), ComponentResult.ExecuteCommand(command))
            ) =>
          val newState = update(initialState)

          command.intent shouldEqual CommandIntent.ToggleLineNumbers
          newState.commandRunnerSurface shouldBe None
          newState.focus shouldEqual Focus.EditorPane(PaneId(1))
        case _ =>
          fail("Expected state change")

    it("should handle backspace in search term"):
      val registry  = CommandRegistry.default
      val component = CommandRunnerComponent(registry)
      val activeRunner =
        CommandRunner.empty.activate(registry, AppConfig.default).updateSearchTerm("test")(using registry)
      val initialState = runnerState(registry, activeRunner, Focus.Surface(SurfaceId("command-runner")))

      runnerFrom(initialState).searchTerm shouldEqual "test"

      component.processEvent(DeleteBackward, initialState) match
        case ComponentResult.StateChange(update) =>
          val newState = update(initialState)
          runnerFrom(newState).searchTerm shouldEqual "tes"
        case _ =>
          fail("Expected state change")
