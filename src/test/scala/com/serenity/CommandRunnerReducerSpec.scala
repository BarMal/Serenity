package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.command.{Command, CommandCategory, CommandIntent, CommandRegistry, CommandRunner, CommandSurfaceItem}
import com.serenity.keystroke.events.*
import com.serenity.state.models.*
import com.serenity.state.reducers.CommandRunnerReducer
import com.serenity.ui.layout.Layout
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CommandRunnerReducerSpec extends AnyFlatSpec with Matchers:

  private def activeState(registry: CommandRegistry): AppState =
    val runner = CommandRunner.empty.activate(registry).withPreviousFocus(Focus.EditorPane(PaneId(2)))
    val surface = UiSurface(
      SurfaceId("command-runner"),
      SurfaceContent.CommandPalette(runner),
      SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
    )
    AppState(
      buffers = Map.empty,
      layout = Layout.empty,
      focus = Focus.Surface(surface.id),
      uiSurfaces = List(surface)
    )

  "CommandRunnerReducer" should "ignore global activation events because activation is owned by the app reducer" in {
    val registry = CommandRegistry.default
    val inactiveSurface = UiSurface(
      SurfaceId("command-runner"),
      SurfaceContent.CommandPalette(CommandRunner.empty),
      SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
    )
    val initialState = AppState(
      buffers = Map.empty,
      layout = Layout.empty,
      focus = Focus.EditorPane(PaneId(1)),
      uiSurfaces = List(inactiveSurface)
    )

    val result = CommandRunnerReducer.reduce(ToggleCommandRunner, initialState, registry)

    result.state shouldBe initialState
    result.effects shouldBe Nil
  }

  it should "filter commands when typing and execute the selection on enter" in {
    var executionCalled = false
    val command  = Command("test", "Test command", _ => IO { executionCalled = true })
    val registry = CommandRegistry(List(command))
    val state    = activeState(registry)

    val typed = CommandRunnerReducer.reduce(InsertChar('t'), state, registry)
    val typedRunner = typed.state.uiSurfaces.collectFirst {
      case UiSurface(_, SurfaceContent.CommandPalette(runner), _, _) => runner
    }.get
    typedRunner.searchTerm shouldBe "t"
    typedRunner.filteredCommands.map(_.name) shouldBe List("test")

    val executed = CommandRunnerReducer.reduce(Enter, typed.state, registry)
    executed.effects should have size 1
    executed.effects.head match
      case com.serenity.state.reducers.AppEffect.ExecuteCommand(commandToRun) =>
        commandToRun.execute(typed.state).unsafeRunSync()
        executionCalled shouldBe true
      case other =>
        fail(s"Expected ExecuteCommand effect, got $other")

    executed.state.commandRunnerSurface shouldBe None
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

  it should "remove the command palette surface entirely when escaped" in {
    val registry = CommandRegistry.default
    val state    = activeState(registry)

    val closed = CommandRunnerReducer.reduce(Escape, state, registry)

    closed.state.commandRunnerSurface shouldBe None
    closed.state.focus shouldBe Focus.EditorPane(PaneId(2))
  }

  it should "switch categories with tab and reverse-tab while search is empty" in {
    val registry = CommandRegistry.default
    given CommandRegistry = registry
    val state    = activeState(registry)

    val movedRight = CommandRunnerReducer.reduce(TabKey, state, registry)
    val runnerAfterRight = movedRight.state.commandRunnerSurface.flatMap {
      _.content match
        case SurfaceContent.CommandPalette(runner) => Some(runner)
        case _                                     => None
    }.getOrElse(fail("Expected command runner surface"))

    runnerAfterRight.activeCategory shouldBe CommandCategory.File

    val movedLeft = CommandRunnerReducer.reduce(ReverseTabKey, movedRight.state, registry)
    val runnerAfterLeft = movedLeft.state.commandRunnerSurface.flatMap {
      _.content match
        case SurfaceContent.CommandPalette(runner) => Some(runner)
        case _                                     => None
    }.getOrElse(fail("Expected command runner surface"))

    runnerAfterLeft.activeCategory shouldBe CommandCategory.All
  }

  it should "leave the category unchanged when left and right are pressed on non-option rows" in {
    val registry = CommandRegistry.default
    given CommandRegistry = registry
    val state    = activeState(registry)

    val movedRight = CommandRunnerReducer.reduce(RunnerNavigate(Direction.Right), state, registry)
    val runnerAfterRight = movedRight.state.commandRunnerSurface.flatMap {
      _.content match
        case SurfaceContent.CommandPalette(runner) => Some(runner)
        case _                                     => None
    }.getOrElse(fail("Expected command runner surface"))

    runnerAfterRight.activeCategory shouldBe CommandCategory.All

    val movedLeft = CommandRunnerReducer.reduce(RunnerNavigate(Direction.Left), state, registry)
    val runnerAfterLeft = movedLeft.state.commandRunnerSurface.flatMap {
      _.content match
        case SurfaceContent.CommandPalette(runner) => Some(runner)
        case _                                     => None
    }.getOrElse(fail("Expected command runner surface"))

    runnerAfterLeft.activeCategory shouldBe CommandCategory.All
  }

  it should "search globally even when opened on a narrower category" in {
    val registry = CommandRegistry.default
    given CommandRegistry = registry
    val runner = CommandRunner.empty
      .activate(registry)
      .withActiveCategory(CommandCategory.File)
      .withPreviousFocus(Focus.EditorPane(PaneId(2)))
    val surface = UiSurface(
      SurfaceId("command-runner"),
      SurfaceContent.CommandPalette(runner),
      SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
    )
    val state = AppState(
      buffers = Map.empty,
      layout = Layout.empty,
      focus = Focus.Surface(surface.id),
      uiSurfaces = List(surface)
    )

    val typed = CommandRunnerReducer.reduce(InsertChar('t'), state, registry)
    val typedRunner = typed.state.commandRunnerSurface.flatMap {
      _.content match
        case SurfaceContent.CommandPalette(updatedRunner) => Some(updatedRunner)
        case _                                            => None
    }.getOrElse(fail("Expected command runner surface"))

    typedRunner.searchTerm shouldBe "t"
    typedRunner.visibleItems.exists {
      case CommandSurfaceItem.CommandItem(command) => command.name == "toggle-theme"
      case _                                       => false
    } shouldBe true
  }

  it should "adjust the selected animation option inline with left and right" in {
    val registry = CommandRegistry.default
    given CommandRegistry = registry
    val runner = CommandRunner.empty
      .activate(registry)
      .withActiveCategory(CommandCategory.Settings)
      .withSelectedItem("animation-mode")
      .withPreviousFocus(Focus.EditorPane(PaneId(2)))
    val surface = UiSurface(
      SurfaceId("command-runner"),
      SurfaceContent.CommandPalette(runner),
      SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
    )
    val state = AppState(
      buffers = Map.empty,
      layout = Layout.empty,
      focus = Focus.Surface(surface.id),
      uiSurfaces = List(surface)
    )

    val movedLeft = CommandRunnerReducer.reduce(RunnerNavigate(Direction.Left), state, registry)
    val runnerAfterLeft = movedLeft.state.commandRunnerSurface.flatMap {
      _.content match
        case SurfaceContent.CommandPalette(updatedRunner) => Some(updatedRunner)
        case _                                            => None
    }.getOrElse(fail("Expected command runner surface"))

    runnerAfterLeft.visibleItems.collectFirst {
      case option: CommandSurfaceItem.OptionItem if option.id == "animation-mode" => option.selectedOption
    } shouldBe Some("Subtle")

    val movedRight = CommandRunnerReducer.reduce(RunnerNavigate(Direction.Right), movedLeft.state, registry)
    val runnerAfterRight = movedRight.state.commandRunnerSurface.flatMap {
      _.content match
        case SurfaceContent.CommandPalette(updatedRunner) => Some(updatedRunner)
        case _                                            => None
    }.getOrElse(fail("Expected command runner surface"))

    runnerAfterRight.visibleItems.collectFirst {
      case option: CommandSurfaceItem.OptionItem if option.id == "animation-mode" => option.selectedOption
    } shouldBe Some("Full")
  }
