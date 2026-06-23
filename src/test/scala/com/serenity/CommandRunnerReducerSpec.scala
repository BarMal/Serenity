package com.serenity

import com.serenity.command.*
import com.serenity.config.*
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
    val command  = Command.typed("test", "Test command", CommandIntent.ToggleLineNumbers)
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
        commandToRun.intent shouldBe CommandIntent.ToggleLineNumbers
      case other =>
        fail(s"Expected ExecuteCommand effect, got $other")

    executed.state.commandRunnerSurface shouldBe None
    executed.state.focus shouldBe Focus.EditorPane(PaneId(2))
  }

  it should "surface typed command intents through execute effects" in {
    val command =
      Command.typed("toggle-line-numbers", "Toggle line numbers display on/off", CommandIntent.ToggleLineNumbers)
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

  it should "delete the previous word from the search term" in {
    val registry = CommandRegistry.default
    val state    = activeState(registry)
    val typed = List('a', 'l', 'p', 'h', 'a', ' ', 'b', 'e', 't', 'a').foldLeft(state) { (s, c) =>
      CommandRunnerReducer.reduce(InsertChar(c), s, registry).state
    }

    val result = CommandRunnerReducer.reduce(RunnerDeleteWordBackward, typed, registry)

    runnerFrom(result.state).searchTerm shouldBe "alpha "
  }

  it should "switch categories with tab and reverse-tab while search is empty" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val state             = activeState(registry)

    val movedRight = CommandRunnerReducer.reduce(TabKey, state, registry)
    val runnerAfterRight = movedRight.state.commandRunnerSurface
      .flatMap {
        _.content match
          case SurfaceContent.CommandPalette(runner) => Some(runner)
          case _                                     => None
      }
      .getOrElse(fail("Expected command runner surface"))

    runnerAfterRight.activeCategory shouldBe CommandCategory.File

    val movedLeft = CommandRunnerReducer.reduce(ReverseTabKey, movedRight.state, registry)
    val runnerAfterLeft = movedLeft.state.commandRunnerSurface
      .flatMap {
        _.content match
          case SurfaceContent.CommandPalette(runner) => Some(runner)
          case _                                     => None
      }
      .getOrElse(fail("Expected command runner surface"))

    runnerAfterLeft.activeCategory shouldBe CommandCategory.All
  }

  it should "leave the category unchanged when left and right are pressed on non-option rows" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val state             = activeState(registry)

    val movedRight = CommandRunnerReducer.reduce(RunnerNavigate(Direction.Right), state, registry)
    val runnerAfterRight = movedRight.state.commandRunnerSurface
      .flatMap {
        _.content match
          case SurfaceContent.CommandPalette(runner) => Some(runner)
          case _                                     => None
      }
      .getOrElse(fail("Expected command runner surface"))

    runnerAfterRight.activeCategory shouldBe CommandCategory.All

    val movedLeft = CommandRunnerReducer.reduce(RunnerNavigate(Direction.Left), state, registry)
    val runnerAfterLeft = movedLeft.state.commandRunnerSurface
      .flatMap {
        _.content match
          case SurfaceContent.CommandPalette(runner) => Some(runner)
          case _                                     => None
      }
      .getOrElse(fail("Expected command runner surface"))

    runnerAfterLeft.activeCategory shouldBe CommandCategory.All
  }

  it should "search globally even when opened on a narrower category" in {
    val registry          = CommandRegistry.default
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

  it should "adjust the selected animation option inside the submenu with left and right" in {
    val registry          = CommandRegistry.default
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
    val runnerAfterLeft = movedLeft.state.commandRunnerSurface
      .flatMap {
        _.content match
          case SurfaceContent.CommandPalette(updatedRunner) => Some(updatedRunner)
          case _                                            => None
      }
      .getOrElse(fail("Expected command runner surface"))

    runnerAfterLeft
      .submenuItems("settings-animation")
      .collectFirst {
        case option: CommandSurfaceItem.OptionItem if option.id == "animation-mode" => option.selectedOption
      }
      .shouldBe(Some("Subtle"))

    val movedRight = CommandRunnerReducer.reduce(RunnerNavigate(Direction.Right), movedLeft.state, registry)
    val runnerAfterRight = movedRight.state.commandRunnerSurface
      .flatMap {
        _.content match
          case SurfaceContent.CommandPalette(updatedRunner) => Some(updatedRunner)
          case _                                            => None
      }
      .getOrElse(fail("Expected command runner surface"))

    runnerAfterRight
      .submenuItems("settings-animation")
      .collectFirst {
        case option: CommandSurfaceItem.OptionItem if option.id == "animation-mode" => option.selectedOption
      }
      .shouldBe(Some("Full"))
  }

  it should "adjust the selected background style inside the surface appearance submenu with left and right" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default)
      .withActiveCategory(CommandCategory.Settings)
      .withSelectedItem("settings-surface-appearance")
      .enterSelectedGroup
      .copy(activeSubmenu =
        Some(com.serenity.command.CommandRunnerSubmenuState("settings-surface-appearance", selectedIndex = 0))
      )
    val surface = UiSurface(
      SurfaceId("command-runner"),
      SurfaceContent.CommandPalette(runner),
      SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
    )
    val submenuSurface = UiSurface(
      SurfaceId("command-runner-submenu"),
      SurfaceContent.CommandPaletteSubmenu(runner, "settings-surface-appearance", previewOnly = false),
      SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
    )
    val state = AppState(
      buffers = Map.empty,
      layout = Layout.empty,
      focus = Focus.Surface(submenuSurface.id),
      uiSurfaces = List(surface, submenuSurface)
    )

    val movedLeft = CommandRunnerReducer.reduce(RunnerNavigate(Direction.Left), state, registry)
    val runnerAfterLeft = movedLeft.state.commandRunnerSurface
      .flatMap {
        _.content match
          case SurfaceContent.CommandPalette(updatedRunner) => Some(updatedRunner)
          case _                                            => None
      }
      .getOrElse(fail("Expected command runner surface"))

    runnerAfterLeft
      .submenuItems("settings-surface-appearance")
      .collectFirst {
        case option: CommandSurfaceItem.OptionItem if option.id == "background-style" => option.selectedOption
      }
      .shouldBe(Some("Transparent"))

    val movedRight = CommandRunnerReducer.reduce(RunnerNavigate(Direction.Right), movedLeft.state, registry)
    movedRight.effects.exists {
      case AppEffect.ExecuteCommand(command) =>
        command.intent == CommandIntent.SetBackgroundStyle(BackgroundStyle.Frosted)
      case _ =>
        false
    } shouldBe true
  }

  it should "adjust the selected interface density inside the interface layout submenu" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default)
      .withActiveCategory(CommandCategory.Settings)
      .withSelectedItem("settings-interface-layout")
      .enterSelectedGroup
      .copy(activeSubmenu =
        Some(com.serenity.command.CommandRunnerSubmenuState("settings-interface-layout", selectedIndex = 0))
      )
    val surface = UiSurface(
      SurfaceId("command-runner"),
      SurfaceContent.CommandPalette(runner),
      SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
    )
    val submenuSurface = UiSurface(
      SurfaceId("command-runner-submenu"),
      SurfaceContent.CommandPaletteSubmenu(runner, "settings-interface-layout", previewOnly = false),
      SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
    )
    val state = AppState(
      buffers = Map.empty,
      layout = Layout.empty,
      focus = Focus.Surface(submenuSurface.id),
      uiSurfaces = List(surface, submenuSurface)
    )

    val movedRight = CommandRunnerReducer.reduce(RunnerNavigate(Direction.Right), state, registry)

    movedRight.effects.exists {
      case AppEffect.ExecuteCommand(command) =>
        command.intent == CommandIntent.SetInterfaceDensity(InterfaceDensity.Spacious)
      case _ =>
        false
    } shouldBe true
  }

  it should "open a preview submenu for the selected expandable settings row without moving focus" in {
    val registry          = CommandRegistry.default
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
    val registry          = CommandRegistry.default
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
    val state = CommandRunnerReducer
      .reduce(
        RunnerInsertChar('5'),
        settingsStateOnItem("settings-animation", "animation-duration"),
        registry
      )
      .state

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
    val state    = settingsStateOnItem("settings-animation", "animation-steps")

    val exited    = CommandRunnerReducer.reduce(RunnerDismiss, state, registry)
    val reentered = CommandRunnerReducer.reduce(RunnerSubmit, exited.state, registry)
    val runner    = runnerFrom(reentered.state)

    runner.activeSubmenu.map(_.selectedIndex) shouldBe Some(2)
    runner.activeSubmenu.flatMap(_.selectedItem(runner.submenuItems("settings-animation")).map(_.id)) shouldBe
      Some("animation-steps")
  }

  it should "filter focused submenu items while typing and submit the filtered selection" in {
    val registry = CommandRegistry.default
    val state    = settingsStateOnItem("settings-language", "lang-plain-text")

    val searched = List('j', 'a', 'v', 'a').foldLeft(state) { (s, char) =>
      CommandRunnerReducer.reduce(RunnerInsertChar(char), s, registry).state
    }
    val runner = runnerFrom(searched)

    runner.activeSubmenu.map(_.searchTerm) shouldBe Some("java")
    runner.focusedSubmenuItems.collect { case CommandSurfaceItem.CommandItem(command) => command.label } shouldBe List(
      "Java",
      "JavaScript"
    )

    val submitted = CommandRunnerReducer.reduce(RunnerSubmit, searched, registry)
    submitted.effects.collectFirst { case AppEffect.ExecuteCommand(command) => command.intent } shouldBe Some(
      CommandIntent.SetBufferLanguage(Some(com.serenity.lsp.config.LanguageId.Java))
    )
  }

  it should "open font family picker submenus and submit UI font choices" in {
    val registry = CommandRegistry.default
    val state    = settingsStateOnItem("settings-ui-font", "ui-font")

    val entered = CommandRunnerReducer.reduce(RunnerSubmit, state, registry)
    val runner  = runnerFrom(entered.state)
    val firstUiFontIntent =
      runner.submenuItems("ui-font").collectFirst { case CommandSurfaceItem.CommandItem(command) => command.intent }

    runner.activeSubmenu.map(_.groupId) shouldBe Some("ui-font")
    runner.activeSubmenu.flatMap(_.parentGroupId) shouldBe Some("settings-ui-font")

    val submitted = CommandRunnerReducer.reduce(RunnerSubmit, entered.state, registry)
    submitted.effects.collectFirst { case AppEffect.ExecuteCommand(command) => command.intent } shouldBe
      firstUiFontIntent
  }

  it should "enter the preset options submenu from UI presets" in {
    val registry = CommandRegistry.default
    val state    = settingsStateOnItem("settings-ui-presets", "ui-preset-configure")

    val entered = CommandRunnerReducer.reduce(RunnerSubmit, state, registry)
    val runner  = runnerFrom(entered.state)

    runner.activeSubmenu.map(_.groupId) shouldBe Some("ui-preset-configure")
    runner.activeSubmenu.flatMap(_.parentGroupId) shouldBe Some("settings-ui-presets")
    runner.focusedSubmenuItems.map(_.id) should contain allOf (
      "settings-document-writing",
      "settings-editor-view",
      "settings-typography",
      "settings-appearance-motion"
    )
  }

  it should "clear submenu search with escape before leaving the submenu" in {
    val registry = CommandRegistry.default
    val searched = List('j', 'a').foldLeft(settingsStateOnItem("settings-language", "lang-plain-text")) { (s, char) =>
      CommandRunnerReducer.reduce(RunnerInsertChar(char), s, registry).state
    }

    val cleared = CommandRunnerReducer.reduce(RunnerDismiss, searched, registry)
    val runner  = runnerFrom(cleared.state)

    runner.activeSubmenu.map(_.searchTerm) shouldBe Some("")
    cleared.state.commandRunnerSubmenuSurface shouldBe defined
    cleared.state.focus shouldBe Focus.Surface(SurfaceId("command-runner-submenu"))
  }

  it should "discard in-progress submenu edit text when exiting and re-entering the group" in {
    val registry = CommandRegistry.default
    val editingState = List('5').foldLeft(
      CommandRunnerReducer
        .reduce(RunnerSubmit, settingsStateOnItem("settings-animation", "animation-steps"), registry)
        .state
    )((s, c) => CommandRunnerReducer.reduce(RunnerInsertChar(c), s, registry).state)

    val stoppedEditing = CommandRunnerReducer.reduce(RunnerDismiss, editingState, registry)
    val exited         = CommandRunnerReducer.reduce(RunnerDismiss, stoppedEditing.state, registry)
    val reentered      = CommandRunnerReducer.reduce(RunnerSubmit, exited.state, registry)
    val runner         = runnerFrom(reentered.state)

    runner.activeSubmenu.map(_.selectedIndex) shouldBe Some(2)
    runner.activeSubmenu.flatMap(_.editingItemId) shouldBe None
    runner.activeSubmenu.map(_.editingText) shouldBe Some("")
  }

  private def settingsStateOnItem(
    groupId: String,
    itemId: String,
    config: AppConfig = AppConfig.default
  ): AppState =
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val searchedRunner = CommandRunner.empty
      .activate(registry, config)
      .withActiveCategory(CommandCategory.Settings)
      .updateSearchTerm(settingsGroupSearchTerm(groupId))
    val selectedIndex = searchedRunner.visibleItems.indexWhere(_.id == groupId) match
      case -1    => 0
      case index => index
    val baseRunner = searchedRunner.copy(selectedIndex = selectedIndex)
    val group      = baseRunner.submenuGroup(groupId).getOrElse(fail(s"missing settings group $groupId"))
    val groupIndex =
      group.children.indexWhere(_.id == itemId) match
        case -1    => 0
        case index => index
    val runner = baseRunner.enterSelectedGroup
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

  private def settingsGroupSearchTerm(groupId: String): String =
    groupId.stripPrefix("settings-").replace("-", " ")

  private def runnerFrom(state: AppState): CommandRunner =
    state.commandRunnerSurface
      .flatMap {
        _.content match
          case SurfaceContent.CommandPalette(r) => Some(r)
          case _                                => None
      }
      .getOrElse(fail("Expected command runner surface"))

  it should "select an input item without auto-entering edit mode" in {
    CommandRegistry.default
    val state  = settingsStateOnItem("settings-animation", "animation-duration")
    val runner = runnerFrom(state)

    runner.activeSubmenu.flatMap(_.editingItemId) shouldBe None
    runner.activeSubmenu.map(_.editingText) shouldBe Some("")
  }

  it should "leave a selected submenu input item unchanged when enter is pressed before typing" in {
    val registry = CommandRegistry.default
    val state    = settingsStateOnItem("settings-animation", "animation-duration")

    val submitted = CommandRunnerReducer.reduce(RunnerSubmit, state, registry)
    val runner    = runnerFrom(submitted.state)

    runner.activeSubmenu.flatMap(_.editingItemId) shouldBe None
    runner.activeSubmenu.map(_.editingText) shouldBe Some("")
    submitted.effects shouldBe Nil
  }

  it should "start editing on first typed digit and replace the saved value" in {
    val registry = CommandRegistry.default
    val state    = settingsStateOnItem("settings-animation", "animation-steps")

    val result = CommandRunnerReducer.reduce(RunnerInsertChar('5'), state, registry)
    runnerFrom(result.state).activeSubmenu.flatMap(_.editingItemId) shouldBe Some("animation-steps")
    runnerFrom(result.state).activeSubmenu.map(_.editingText) shouldBe Some("5")
  }

  it should "reject non-numeric characters silently" in {
    val registry = CommandRegistry.default
    val state    = settingsStateOnItem("settings-animation", "animation-steps")

    val result = CommandRunnerReducer.reduce(RunnerInsertChar('x'), state, registry)
    runnerFrom(result.state).activeSubmenu.map(_.editingText) shouldBe runnerFrom(state).activeSubmenu.map(
      _.editingText
    )
  }

  it should "reject a decimal point on an integer InputItem" in {
    val registry = CommandRegistry.default
    val state    = settingsStateOnItem("settings-animation", "animation-steps")

    val result = CommandRunnerReducer.reduce(RunnerInsertChar('.'), state, registry)
    runnerFrom(result.state).activeSubmenu.map(_.editingText) shouldBe runnerFrom(state).activeSubmenu.map(
      _.editingText
    )
  }

  it should "accept a decimal point on a decimal InputItem once dots are cleared" in {
    val registry = CommandRegistry.default
    val state    = settingsStateOnItem("settings-surface-appearance", "blur-radius")

    val after0 = CommandRunnerReducer.reduce(RunnerInsertChar('0'), state, registry)
    val s0 = state.copy(uiSurfaces =
      state.uiSurfaces.map(s => s.copy(content = SurfaceContent.CommandPalette(runnerFrom(after0.state))))
    )

    val afterDot = CommandRunnerReducer.reduce(RunnerInsertChar('.'), s0, registry)
    runnerFrom(afterDot.state).activeSubmenu.map(_.editingText) shouldBe Some("0.")
  }

  it should "reject a second decimal point" in {
    val registry = CommandRegistry.default
    val state    = settingsStateOnItem("settings-surface-appearance", "blur-radius")

    val after0 = runnerFrom(CommandRunnerReducer.reduce(RunnerInsertChar('0'), state, registry).state)
    val s1 = state.copy(uiSurfaces = state.uiSurfaces.map(s => s.copy(content = SurfaceContent.CommandPalette(after0))))
    val afterDot = runnerFrom(CommandRunnerReducer.reduce(RunnerInsertChar('.'), s1, registry).state)
    val s2 =
      state.copy(uiSurfaces = state.uiSurfaces.map(s => s.copy(content = SurfaceContent.CommandPalette(afterDot))))
    val afterSecondDot = runnerFrom(CommandRunnerReducer.reduce(RunnerInsertChar('.'), s2, registry).state)

    afterSecondDot.activeSubmenu.map(_.editingText) shouldBe afterDot.activeSubmenu.map(_.editingText)
  }

  it should "delete the last character on backspace" in {
    val registry = CommandRegistry.default
    val state = CommandRunnerReducer
      .reduce(
        RunnerInsertChar('5'),
        settingsStateOnItem("settings-animation", "animation-steps"),
        registry
      )
      .state
    val runner     = runnerFrom(state)
    val textBefore = runner.activeSubmenu.map(_.editingText).getOrElse("")

    val result = CommandRunnerReducer.reduce(RunnerDeleteBackward, state, registry)
    runnerFrom(result.state).activeSubmenu.map(_.editingText) shouldBe Some(textBefore.dropRight(1))
  }

  it should "fire SetAnimationSteps intent on Enter with valid value" in {
    val registry = CommandRegistry.default
    val state    = settingsStateOnItem("settings-animation", "animation-steps")

    val typed = List('2', '0').foldLeft(state) { (s, c) =>
      val r = CommandRunnerReducer.reduce(RunnerInsertChar(c), s, registry)
      s.copy(uiSurfaces =
        s.uiSurfaces.map(surf => surf.copy(content = SurfaceContent.CommandPalette(runnerFrom(r.state))))
      )
    }

    val result = CommandRunnerReducer.reduce(RunnerSubmit, typed, registry)
    result.effects.exists {
      case AppEffect.ExecuteCommand(command) =>
        command.intent == CommandIntent.SetAnimationSteps(20)
      case _ =>
        false
    } shouldBe true
  }

  it should "emit authored document comment text from the navigation input item" in {
    val registry = CommandRegistry.default
    val state    = settingsStateOnItem("settings-navigation", "document-comment")

    val typed =
      "Tighten this opening".foldLeft(state)((s, char) =>
        CommandRunnerReducer.reduce(RunnerInsertChar(char), s, registry).state
      )

    val result = CommandRunnerReducer.reduce(RunnerSubmit, typed, registry)

    result.effects.collectFirst { case AppEffect.ExecuteCommand(command) => command.intent } shouldBe Some(
      CommandIntent.AddDocumentComment("Tighten this opening")
    )
  }

  it should "be a no-op on Enter when the value is out of bounds" in {
    val registry = CommandRegistry.default
    val state    = settingsStateOnItem("settings-animation", "animation-steps")

    val typedOutOfBounds = List('9', '9', '9').foldLeft(state) { (s, c) =>
      val r = CommandRunnerReducer.reduce(RunnerInsertChar(c), s, registry)
      s.copy(uiSurfaces =
        s.uiSurfaces.map(surf => surf.copy(content = SurfaceContent.CommandPalette(runnerFrom(r.state))))
      )
    }

    val result = CommandRunnerReducer.reduce(RunnerSubmit, typedOutOfBounds, registry)
    result.effects shouldBe Nil
  }

  it should "discard editing text when navigating to a different item" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val state             = settingsStateOnItem("settings-animation", "animation-steps")

    val typedState = List('5').foldLeft(state) { (s, c) =>
      val r = CommandRunnerReducer.reduce(RunnerInsertChar(c), s, registry)
      s.copy(uiSurfaces =
        s.uiSurfaces.map(surf => surf.copy(content = SurfaceContent.CommandPalette(runnerFrom(r.state))))
      )
    }

    val navigated = CommandRunnerReducer.reduce(RunnerNavigate(Direction.Down), typedState, registry)
    val runner    = runnerFrom(navigated.state)

    runner.activeSubmenu.flatMap(_.editingItemId) shouldNot be(Some("animation-steps"))
    runner.activeSubmenu.map(_.editingText) shouldNot be(runnerFrom(typedState).activeSubmenu.map(_.editingText))
  }

  it should "restore the saved value when escape cancels a pending submenu edit" in {
    val registry = CommandRegistry.default
    val state = List('5').foldLeft(settingsStateOnItem("settings-animation", "animation-steps")) { (s, c) =>
      val r = CommandRunnerReducer.reduce(RunnerInsertChar(c), s, registry)
      s.copy(uiSurfaces =
        s.uiSurfaces.map(surf => surf.copy(content = SurfaceContent.CommandPalette(runnerFrom(r.state))))
      )
    }

    val cancelled = CommandRunnerReducer.reduce(Escape, state, registry)
    val runner    = runnerFrom(cancelled.state)
    val restoredValue = runner
      .submenuItems("settings-animation")
      .collectFirst { case item: CommandSurfaceItem.InputItem if item.id == "animation-steps" => item.currentValue }

    runner.activeSubmenu.flatMap(_.editingItemId) shouldBe None
    runner.activeSubmenu.map(_.editingText) shouldBe Some("")
    restoredValue shouldBe Some("12")
  }

  it should "edit keymap binding text and emit a focused keymap update intent" in {
    val registry = CommandRegistry.default
    val state    = settingsStateOnItem("settings-keymap", "keymap-command-runner-submit")

    val typed =
      "ctrl+enter".foldLeft(state)((s, char) => CommandRunnerReducer.reduce(RunnerInsertChar(char), s, registry).state)

    val result = CommandRunnerReducer.reduce(RunnerSubmit, typed, registry)

    result.effects.collectFirst { case AppEffect.ExecuteCommand(command) => command.intent } shouldBe Some(
      CommandIntent.SetCommandRunnerKeyBinding(CommandRunnerKeyAction.Submit, "ctrl+enter")
    )
  }

  it should "emit a keymap reset intent when a binding field is set to reset" in {
    val registry = CommandRegistry.default
    val config = AppConfig.default
      .withCommandRunnerKeyOverride(CommandRunnerKeyAction.Submit, "ctrl+enter")
    val state = settingsStateOnItem("settings-keymap", "keymap-command-runner-submit", config)

    val typed =
      "reset".foldLeft(state)((s, char) => CommandRunnerReducer.reduce(RunnerInsertChar(char), s, registry).state)

    val result = CommandRunnerReducer.reduce(RunnerSubmit, typed, registry)

    result.effects.collectFirst { case AppEffect.ExecuteCommand(command) => command.intent } shouldBe Some(
      CommandIntent.ResetCommandRunnerKeyBinding(CommandRunnerKeyAction.Submit)
    )
  }

  it should "keep keymap edit mode open with a status message for invalid binding text" in {
    val registry = CommandRegistry.default
    val state    = settingsStateOnItem("settings-keymap", "keymap-command-runner-submit")

    val typed =
      "ctrl".foldLeft(state)((s, char) => CommandRunnerReducer.reduce(RunnerInsertChar(char), s, registry).state)

    val result = CommandRunnerReducer.reduce(RunnerSubmit, typed, registry)
    val runner = runnerFrom(result.state)

    result.effects shouldBe Nil
    runner.statusMessage shouldBe Some("Invalid binding: ctrl")
    runner.activeSubmenu.flatMap(_.editingItemId) shouldBe Some("keymap-command-runner-submit")
    runner.activeSubmenu.map(_.editingText) shouldBe Some("ctrl")
  }

  it should "edit text area inset percentages and emit a layout update intent" in {
    val registry = CommandRegistry.default
    val state    = settingsStateOnItem("settings-text-area", "text-area-left")

    val typed =
      "22.5".foldLeft(state)((s, char) => CommandRunnerReducer.reduce(RunnerInsertChar(char), s, registry).state)

    val result = CommandRunnerReducer.reduce(RunnerSubmit, typed, registry)

    result.effects.collectFirst { case AppEffect.ExecuteCommand(command) => command.intent } shouldBe Some(
      CommandIntent.SetTextAreaLeftInset(0.225)
    )
  }
