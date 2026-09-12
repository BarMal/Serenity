package com.serenity

import com.serenity.command.*
import com.serenity.config.*
import com.serenity.keystroke.events.*
import com.serenity.state.models.*
import com.serenity.state.reducers.CommandRunnerReducer
import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Top-level activation, dismissal, and root-level search/submit behavior of `CommandRunnerReducer` -- split out of
  * `CommandRunnerReducerSpec` to keep each file focused on one concern (see that file's history for the rationale).
  */
class CommandRunnerReducerLifecycleSpec extends AnyFlatSpec with Matchers:

  private def activeState(registry: CommandRegistry, config: AppConfig = AppConfig.default): AppState =
    val runner = CommandRunner.empty.activate(registry, config)
    val surface = UiSurface(
      SurfaceId("command-runner"),
      SurfaceContent.CommandPalette(runner),
      SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
    )
    AppState(
      persisted = Persisted(
        layout = Layout.empty,
        buffers = Map.empty,
        focus = Focus.Surface(surface.id)
      ),
      runtime = Runtime(
        uiSurfaces = List(surface),
        focusHistory = List(Focus.EditorPane(PaneId(2)))
      )
    )

  "CommandRunnerReducer" should "ignore global activation events because activation is owned by the app reducer" in {
    val registry = CommandRegistry.default
    val inactiveSurface = UiSurface(
      SurfaceId("command-runner"),
      SurfaceContent.CommandPalette(CommandRunner.empty),
      SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
    )
    val initialState = AppState(
      persisted = Persisted(
        layout = Layout.empty,
        buffers = Map.empty,
        focus = Focus.EditorPane(PaneId(1))
      ),
      runtime = Runtime(uiSurfaces = List(inactiveSurface))
    )

    val result = CommandRunnerReducer.reduce(ToggleCommandRunner, initialState, registry)

    result.state shouldBe initialState
    result.effects shouldBe Nil
  }

  it should "filter commands when typing and execute the selection on enter" in {
    val command = Command.typed(
      "test",
      "Test command",
      CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.ToggleLineNumbers))
    )
    val registry = CommandRegistry(List(command))
    val state    = activeState(registry)

    val typed = CommandRunnerReducer.reduce(InsertChar('t'), state, registry)
    val typedRunner = typed.state.runtime.uiSurfaces.collectFirst {
      case UiSurface(_, SurfaceContent.CommandPalette(runner), _, _) => runner
    }.get
    typedRunner.searchTerm shouldBe "t"
    typedRunner.filteredCommands.map(_.name) shouldBe List("test")

    val executed = CommandRunnerReducer.reduce(Enter, typed.state, registry)
    executed.effects should have size 1
    executed.effects.head match
      case com.serenity.state.reducers.AppEffect.ExecuteCommand(commandToRun) =>
        commandToRun.intent shouldBe CommandIntent.Settings(
          SettingsIntent.PanelChrome(PanelChromeIntent.ToggleLineNumbers)
        )
      case other =>
        fail(s"Expected ExecuteCommand effect, got $other")

    executed.state.commandRunnerSurface shouldBe None
    executed.state.persisted.focus shouldBe Focus.EditorPane(PaneId(2))
  }

  it should "surface typed command intents through execute effects" in {
    val command =
      Command.typed(
        "toggle-line-numbers",
        "Toggle line numbers display on/off",
        CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.ToggleLineNumbers))
      )
    val registry = CommandRegistry(List(command))
    val state    = activeState(registry)

    val executed = CommandRunnerReducer.reduce(Enter, state, registry)

    executed.effects should have size 1
    executed.effects.head match
      case com.serenity.state.reducers.AppEffect.ExecuteCommand(commandToRun) =>
        commandToRun.intent shouldBe CommandIntent.Settings(
          SettingsIntent.PanelChrome(PanelChromeIntent.ToggleLineNumbers)
        )
      case other =>
        fail(s"Expected ExecuteCommand effect, got $other")
  }

  it should "remove the command palette surface entirely when escaped" in {
    val registry = CommandRegistry.default
    val state    = activeState(registry)

    val closed = CommandRunnerReducer.reduce(Escape, state, registry)

    closed.state.commandRunnerSurface shouldBe None
    closed.state.persisted.focus shouldBe Focus.EditorPane(PaneId(2))
  }

  // issue #931/#1057: "settings-language" and category tabs are retired -- searching now happens unconditionally
  // globally, which was already true once typing began, before this migration; it's simply the only behavior left.
  it should "search globally" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default)
    val surface = UiSurface(
      SurfaceId("command-runner"),
      SurfaceContent.CommandPalette(runner),
      SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
    )
    val state = AppState(
      persisted = Persisted(
        layout = Layout.empty,
        buffers = Map.empty,
        focus = Focus.Surface(surface.id)
      ),
      runtime = Runtime(uiSurfaces = List(surface))
    )

    val typed = CommandRunnerReducer.reduce(InsertChar('t'), state, registry)
    val typedRunner = typed.state.commandRunnerSurface
      .flatMap {
        _.content match
          case SurfaceContent.CommandPalette(updatedRunner) => Some(updatedRunner)
          case _                                            => None
      }
      .getOrElse(fail("Expected command runner surface"))

    typedRunner.searchTerm shouldBe "t"
    typedRunner.visibleItems.exists {
      case CommandSurfaceItem.CommandItem(command) => command.name == "toggle-theme"
      case _                                       => false
    } shouldBe true
  }
