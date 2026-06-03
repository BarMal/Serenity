package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.command.{Command, CommandCategory, CommandIntent, CommandRegistry, CommandRunner, CommandSurfaceItem}
import com.serenity.config.{AppConfig, BackgroundStyle}
import com.serenity.keystroke.events.*
import com.serenity.state.models.*
import com.serenity.state.reducers.{AppEffect, CommandRunnerReducer}
import com.serenity.ui.layout.Layout
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CommandRunnerReducerSpec extends AnyFlatSpec with Matchers:

  private def activeState(registry: CommandRegistry): AppState =
    val runner = CommandRunner.empty.activate(registry, AppConfig.default)
    val surface = UiSurface(
      SurfaceId("command-runner"),
      SurfaceContent.CommandPalette(runner),
      SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
    )
    AppState(
      buffers = Map.empty,
      layout = Layout.empty,
      focus = Focus.Surface(surface.id),
      uiSurfaces = List(surface),
      focusHistory = List(Focus.EditorPane(PaneId(2)))
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
      .activate(registry, AppConfig.default)
      .withActiveCategory(CommandCategory.File)
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

  it should "adjust the selected animation option inside the submenu with left and right" in {
    val registry = CommandRegistry.default
    given CommandRegistry = registry
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default)
      .withActiveCategory(CommandCategory.Settings)
      .withSelectedItem("settings-animation")
      .enterSelectedGroup
      .copy(activeSubmenu = Some(com.serenity.command.CommandRunnerSubmenuState("settings-animation")))
    val surface = UiSurface(
      SurfaceId("command-runner"),
      SurfaceContent.CommandPalette(runner),
      SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
    )
    val submenuSurface = UiSurface(
      SurfaceId("command-runner-submenu"),
      SurfaceContent.CommandPaletteSubmenu(runner, "settings-animation", previewOnly = false),
      SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
    )
    val state = AppState(
      buffers = Map.empty,
      layout = Layout.empty,
      focus = Focus.Surface(submenuSurface.id),
      uiSurfaces = List(surface, submenuSurface)
    )

    val movedLeft = CommandRunnerReducer.reduce(RunnerNavigate(Direction.Left), state, registry)
    val runnerAfterLeft = movedLeft.state.commandRunnerSurface.flatMap {
      _.content match
        case SurfaceContent.CommandPalette(updatedRunner) => Some(updatedRunner)
        case _                                            => None
    }.getOrElse(fail("Expected command runner surface"))

    runnerAfterLeft.submenuItems("settings-animation").collectFirst {
      case option: CommandSurfaceItem.OptionItem if option.id == "animation-mode" => option.selectedOption
    }.shouldBe(Some("Subtle"))

    val movedRight = CommandRunnerReducer.reduce(RunnerNavigate(Direction.Right), movedLeft.state, registry)
    val runnerAfterRight = movedRight.state.commandRunnerSurface.flatMap {
      _.content match
        case SurfaceContent.CommandPalette(updatedRunner) => Some(updatedRunner)
        case _                                            => None
    }.getOrElse(fail("Expected command runner surface"))

    runnerAfterRight.submenuItems("settings-animation").collectFirst {
      case option: CommandSurfaceItem.OptionItem if option.id == "animation-mode" => option.selectedOption
    }.shouldBe(Some("Full"))
  }

  it should "adjust the selected background style inside the submenu with left and right" in {
    val registry = CommandRegistry.default
    given CommandRegistry = registry
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default)
      .withActiveCategory(CommandCategory.Settings)
      .withSelectedItem("settings-appearance")
      .enterSelectedGroup
      .copy(activeSubmenu = Some(com.serenity.command.CommandRunnerSubmenuState("settings-appearance", selectedIndex = 1)))
    val surface = UiSurface(
      SurfaceId("command-runner"),
      SurfaceContent.CommandPalette(runner),
      SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
    )
    val submenuSurface = UiSurface(
      SurfaceId("command-runner-submenu"),
      SurfaceContent.CommandPaletteSubmenu(runner, "settings-appearance", previewOnly = false),
      SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
    )
    val state = AppState(
      buffers = Map.empty,
      layout = Layout.empty,
      focus = Focus.Surface(submenuSurface.id),
      uiSurfaces = List(surface, submenuSurface)
    )

    val movedLeft = CommandRunnerReducer.reduce(RunnerNavigate(Direction.Left), state, registry)
    val runnerAfterLeft = movedLeft.state.commandRunnerSurface.flatMap {
      _.content match
        case SurfaceContent.CommandPalette(updatedRunner) => Some(updatedRunner)
        case _                                            => None
    }.getOrElse(fail("Expected command runner surface"))

    runnerAfterLeft.submenuItems("settings-appearance").collectFirst {
      case option: CommandSurfaceItem.OptionItem if option.id == "background-style" => option.selectedOption
    }.shouldBe(Some("Transparent"))

    val movedRight = CommandRunnerReducer.reduce(RunnerNavigate(Direction.Right), movedLeft.state, registry)
    movedRight.effects.exists {
      case e: AppEffect.ExecuteCommand => e.command.intent == CommandIntent.SetBackgroundStyle(BackgroundStyle.Frosted)
      case _                           => false
    } shouldBe true
  }

  it should "open a preview submenu for the selected expandable settings row without moving focus" in {
    val registry = CommandRegistry.default
    given CommandRegistry = registry
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default)
      .withActiveCategory(CommandCategory.Settings)
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

    val previewed = CommandRunnerReducer.reduce(RunnerNavigate(Direction.Down), state, registry)

    previewed.state.commandRunnerSurface shouldBe defined
    previewed.state.commandRunnerSubmenuSurface shouldBe defined
    previewed.state.focus shouldBe Focus.Surface(surface.id)
  }

  it should "focus the submenu on enter and return to the parent runner on escape" in {
    val registry = CommandRegistry.default
    given CommandRegistry = registry
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default)
      .withActiveCategory(CommandCategory.Settings)
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

    val entered = CommandRunnerReducer.reduce(RunnerSubmit, state, registry)

    entered.state.commandRunnerSurface shouldBe defined
    entered.state.commandRunnerSubmenuSurface shouldBe defined
    entered.state.focus shouldBe Focus.Surface(SurfaceId("command-runner-submenu"))

    val exited = CommandRunnerReducer.reduce(RunnerDismiss, entered.state, registry)

    exited.state.commandRunnerSurface shouldBe defined
    exited.state.commandRunnerSubmenuSurface shouldBe defined
    exited.state.focus shouldBe Focus.Surface(SurfaceId("command-runner"))
  }

  it should "exit submenu edit mode on escape before leaving the submenu" in {
    val registry = CommandRegistry.default
    val state = CommandRunnerReducer.reduce(
      RunnerSubmit,
      settingsStateOnItem("settings-animation", "animation-duration"),
      registry
    ).state

    runnerFrom(state).activeSubmenu.flatMap(_.editingItemId) shouldBe Some("animation-duration")

    val escaped = CommandRunnerReducer.reduce(RunnerDismiss, state, registry)
    val runner  = runnerFrom(escaped.state)

    runner.activeSubmenu.flatMap(_.editingItemId) shouldBe None
    runner.activeSubmenu.map(_.editingText) shouldBe Some("")
    escaped.state.commandRunnerSubmenuSurface shouldBe defined
    escaped.state.focus shouldBe Focus.Surface(SurfaceId("command-runner-submenu"))
  }

  it should "preserve submenu selection when exiting to the parent and re-entering the same group" in {
    val registry = CommandRegistry.default
    val state = settingsStateOnItem("settings-animation", "animation-steps")

    val exited = CommandRunnerReducer.reduce(RunnerDismiss, state, registry)
    val reentered = CommandRunnerReducer.reduce(RunnerSubmit, exited.state, registry)
    val runner = runnerFrom(reentered.state)

    runner.activeSubmenu.map(_.selectedIndex) shouldBe Some(2)
    runner.activeSubmenu.flatMap(_.selectedItem(runner.submenuItems("settings-animation")).map(_.id)) shouldBe
      Some("animation-steps")
  }

  it should "discard in-progress submenu edit text when exiting and re-entering the group" in {
    val registry = CommandRegistry.default
    val editingState = List('5').foldLeft(
      CommandRunnerReducer.reduce(RunnerSubmit, settingsStateOnItem("settings-animation", "animation-steps"), registry).state
    ) { (s, c) =>
      CommandRunnerReducer.reduce(RunnerInsertChar(c), s, registry).state
    }

    val stoppedEditing = CommandRunnerReducer.reduce(RunnerDismiss, editingState, registry)
    val exited = CommandRunnerReducer.reduce(RunnerDismiss, stoppedEditing.state, registry)
    val reentered = CommandRunnerReducer.reduce(RunnerSubmit, exited.state, registry)
    val runner = runnerFrom(reentered.state)

    runner.activeSubmenu.map(_.selectedIndex) shouldBe Some(2)
    runner.activeSubmenu.flatMap(_.editingItemId) shouldBe None
    runner.activeSubmenu.map(_.editingText) shouldBe Some("")
  }

  private def settingsStateOnItem(
    groupId: String,
    itemId: String,
    config: AppConfig = AppConfig.default
  ): AppState =
    val registry = CommandRegistry.default
    given CommandRegistry = registry
    val baseRunner = CommandRunner.empty
      .activate(registry, config)
      .withActiveCategory(CommandCategory.Settings)
    val groupIndex = baseRunner.settingsGroups.find(_.id == groupId).map(_.children.indexWhere(_.id == itemId)).getOrElse(0)
    val runner = baseRunner
      .withSelectedItem(groupId)
      .enterSelectedGroup
      .copy(activeSubmenu = Some(com.serenity.command.CommandRunnerSubmenuState(groupId, selectedIndex = groupIndex)))
    val surface = UiSurface(
      SurfaceId("command-runner"),
      SurfaceContent.CommandPalette(runner),
      SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
    )
    val submenuSurface = UiSurface(
      SurfaceId("command-runner-submenu"),
      SurfaceContent.CommandPaletteSubmenu(runner, groupId, previewOnly = false),
      SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
    )
    AppState(
      buffers = Map.empty,
      layout = Layout.empty,
      focus = Focus.Surface(submenuSurface.id),
      uiSurfaces = List(surface, submenuSurface)
    )

  private def runnerFrom(state: AppState): CommandRunner =
    state.commandRunnerSurface.flatMap {
      _.content match
        case SurfaceContent.CommandPalette(r) => Some(r)
        case _                               => None
    }.getOrElse(fail("Expected command runner surface"))

  it should "select an input item without auto-entering edit mode" in {
    val registry = CommandRegistry.default
    val state    = settingsStateOnItem("settings-animation", "animation-duration")
    val runner   = runnerFrom(state)

    runner.activeSubmenu.flatMap(_.editingItemId) shouldBe None
    runner.activeSubmenu.map(_.editingText) shouldBe Some("")
  }

  it should "enter edit mode on submit when a submenu input item is selected" in {
    val registry = CommandRegistry.default
    val state    = settingsStateOnItem("settings-animation", "animation-duration")

    val entered = CommandRunnerReducer.reduce(RunnerSubmit, state, registry)
    val runner  = runnerFrom(entered.state)

    runner.activeSubmenu.flatMap(_.editingItemId) shouldBe Some("animation-duration")
    runner.activeSubmenu.map(_.editingText) shouldBe Some("200")
    entered.effects shouldBe Nil
  }

  it should "accept digits when editing an InputItem" in {
    val registry   = CommandRegistry.default
    val state      = CommandRunnerReducer.reduce(
      RunnerSubmit,
      settingsStateOnItem("settings-animation", "animation-steps"),
      registry
    ).state
    val textBefore = runnerFrom(state).activeSubmenu.map(_.editingText).getOrElse("")

    val result = CommandRunnerReducer.reduce(RunnerInsertChar('5'), state, registry)
    runnerFrom(result.state).activeSubmenu.map(_.editingText) shouldBe Some(textBefore + "5")
  }

  it should "reject non-numeric characters silently" in {
    val registry = CommandRegistry.default
    val state    = CommandRunnerReducer.reduce(
      RunnerSubmit,
      settingsStateOnItem("settings-animation", "animation-steps"),
      registry
    ).state

    val result = CommandRunnerReducer.reduce(RunnerInsertChar('x'), state, registry)
    runnerFrom(result.state).activeSubmenu.map(_.editingText) shouldBe runnerFrom(state).activeSubmenu.map(_.editingText)
  }

  it should "reject a decimal point on an integer InputItem" in {
    val registry = CommandRegistry.default
    val state    = CommandRunnerReducer.reduce(
      RunnerSubmit,
      settingsStateOnItem("settings-animation", "animation-steps"),
      registry
    ).state

    val result = CommandRunnerReducer.reduce(RunnerInsertChar('.'), state, registry)
    runnerFrom(result.state).activeSubmenu.map(_.editingText) shouldBe runnerFrom(state).activeSubmenu.map(_.editingText)
  }

  it should "accept a decimal point on a decimal InputItem once dots are cleared" in {
    val registry = CommandRegistry.default
    val state    = CommandRunnerReducer.reduce(
      RunnerSubmit,
      settingsStateOnItem("settings-appearance", "blur-radius"),
      registry
    ).state

    // Clear pre-filled text (e.g. "0.0") so no dot remains
    val cleared = runnerFrom(state).activeSubmenu.map(_.editingText).getOrElse("").foldLeft(state) { (s, _) =>
      val r = CommandRunnerReducer.reduce(RunnerDeleteBackward, s, registry)
      s.copy(uiSurfaces = s.uiSurfaces.map(surf => surf.copy(content = SurfaceContent.CommandPalette(runnerFrom(r.state)))))
    }

    val after0 = CommandRunnerReducer.reduce(RunnerInsertChar('0'), cleared, registry)
    val s0 = cleared.copy(uiSurfaces = cleared.uiSurfaces.map(s => s.copy(content = SurfaceContent.CommandPalette(runnerFrom(after0.state)))))

    val afterDot = CommandRunnerReducer.reduce(RunnerInsertChar('.'), s0, registry)
    runnerFrom(afterDot.state).activeSubmenu.map(_.editingText) shouldBe Some("0.")
  }

  it should "reject a second decimal point" in {
    val registry = CommandRegistry.default
    val state    = CommandRunnerReducer.reduce(
      RunnerSubmit,
      settingsStateOnItem("settings-appearance", "blur-radius"),
      registry
    ).state

    val after0 = runnerFrom(CommandRunnerReducer.reduce(RunnerInsertChar('0'), state, registry).state)
    val s1 = state.copy(uiSurfaces = state.uiSurfaces.map(s => s.copy(content = SurfaceContent.CommandPalette(after0))))
    val afterDot = runnerFrom(CommandRunnerReducer.reduce(RunnerInsertChar('.'), s1, registry).state)
    val s2 = state.copy(uiSurfaces = state.uiSurfaces.map(s => s.copy(content = SurfaceContent.CommandPalette(afterDot))))
    val afterSecondDot = runnerFrom(CommandRunnerReducer.reduce(RunnerInsertChar('.'), s2, registry).state)

    afterSecondDot.activeSubmenu.map(_.editingText) shouldBe afterDot.activeSubmenu.map(_.editingText)
  }

  it should "delete the last character on backspace" in {
    val registry = CommandRegistry.default
    val state    = CommandRunnerReducer.reduce(
      RunnerSubmit,
      settingsStateOnItem("settings-animation", "animation-steps"),
      registry
    ).state
    val runner   = runnerFrom(state)
    val textBefore = runner.activeSubmenu.map(_.editingText).getOrElse("")

    val result = CommandRunnerReducer.reduce(RunnerDeleteBackward, state, registry)
    runnerFrom(result.state).activeSubmenu.map(_.editingText) shouldBe Some(textBefore.dropRight(1))
  }

  it should "fire SetAnimationSteps intent on Enter with valid value" in {
    val registry = CommandRegistry.default
    val state    = CommandRunnerReducer.reduce(
      RunnerSubmit,
      settingsStateOnItem("settings-animation", "animation-steps"),
      registry
    ).state

    // Clear pre-filled value, then type "20"
    val cleared = runnerFrom(state).activeSubmenu.map(_.editingText).getOrElse("").foldLeft(state) { (s, _) =>
      val r = CommandRunnerReducer.reduce(RunnerDeleteBackward, s, registry)
      s.copy(uiSurfaces = s.uiSurfaces.map(surf => surf.copy(content = SurfaceContent.CommandPalette(runnerFrom(r.state)))))
    }
    val typed = List('2', '0').foldLeft(cleared) { (s, c) =>
      val r = CommandRunnerReducer.reduce(RunnerInsertChar(c), s, registry)
      s.copy(uiSurfaces = s.uiSurfaces.map(surf => surf.copy(content = SurfaceContent.CommandPalette(runnerFrom(r.state)))))
    }

    val result = CommandRunnerReducer.reduce(RunnerSubmit, typed, registry)
    result.effects.exists {
      case e: AppEffect.ExecuteCommand => e.command.intent == CommandIntent.SetAnimationSteps(20)
      case _                           => false
    } shouldBe true
  }

  it should "be a no-op on Enter when the value is out of bounds" in {
    val registry = CommandRegistry.default
    val state    = CommandRunnerReducer.reduce(
      RunnerSubmit,
      settingsStateOnItem("settings-animation", "animation-steps"),
      registry
    ).state

    val typedOutOfBounds = List('9', '9', '9').foldLeft(state) { (s, c) =>
      val r = CommandRunnerReducer.reduce(RunnerInsertChar(c), s, registry)
      s.copy(uiSurfaces = s.uiSurfaces.map(surf => surf.copy(content = SurfaceContent.CommandPalette(runnerFrom(r.state)))))
    }

    val result = CommandRunnerReducer.reduce(RunnerSubmit, typedOutOfBounds, registry)
    result.effects shouldBe Nil
  }

  it should "discard editing text when navigating to a different item" in {
    val registry = CommandRegistry.default
    given CommandRegistry = registry
    val state = CommandRunnerReducer.reduce(
      RunnerSubmit,
      settingsStateOnItem("settings-animation", "animation-steps"),
      registry
    ).state

    val typedState = List('5').foldLeft(state) { (s, c) =>
      val r = CommandRunnerReducer.reduce(RunnerInsertChar(c), s, registry)
      s.copy(uiSurfaces = s.uiSurfaces.map(surf => surf.copy(content = SurfaceContent.CommandPalette(runnerFrom(r.state)))))
    }

    val navigated = CommandRunnerReducer.reduce(RunnerNavigate(Direction.Down), typedState, registry)
    val runner    = runnerFrom(navigated.state)

    runner.activeSubmenu.flatMap(_.editingItemId) shouldNot be(Some("animation-steps"))
    runner.activeSubmenu.map(_.editingText) shouldNot be(runnerFrom(typedState).activeSubmenu.map(_.editingText))
  }
