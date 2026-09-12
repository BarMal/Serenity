package com.serenity

import com.serenity.command.*
import com.serenity.config.*
import com.serenity.config.AppConfigMotionOps.*
import com.serenity.keystroke.events.*
import com.serenity.state.models.*
import com.serenity.state.reducers.{AppEffect, CommandRunnerReducer}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** `CommandRunnerReducer`'s settings-submenu entry/exit -- drilling into a group via submit or search, popping back
  * out via dismiss, and breadcrumb/ancestry tracking -- split out of `CommandRunnerReducerSpec` to keep each file
  * focused on one concern.
  */
class CommandRunnerReducerSubmenuNavigationSpec extends AnyFlatSpec with Matchers:

  extension (runner: CommandRunner)
    private def activeSubmenuGroupId: Option[String]    = runner.activeSettingsSurface.map(_.current.groupId)
    private def activeSubmenuSearchTerm: Option[String] = runner.activeSettingsSurface.map(_.current.searchTerm)
    private def activeSubmenuEditingItemId: Option[String] =
      runner.activeSettingsSurface.flatMap(_.current.editingItemId)
    private def activeSubmenuEditingText: Option[String] = runner.activeSettingsSurface.map(_.current.draftText)
    private def activeSubmenuSelectedIndex: Option[Int] =
      runner.activeSettingsSurface.map(_ => runner.settingsSurfaceSelectedIndex)
    private def activeSubmenuParentGroupId: Option[String] =
      runner.activeSettingsSurface.flatMap(_.ancestors.headOption.map(_.groupId))
    private def activeSubmenuAncestorGroupIds: Option[List[String]] =
      runner.activeSettingsSurface.map(_.ancestors.reverse.map(_.groupId))
    private def activeSubmenuSelectedItem: Option[CommandSurfaceItem] =
      runner.focusedSubmenuItems.lift(runner.settingsSurfaceSelectedIndex)

  private def runnerFrom(state: AppState): CommandRunner =
    state.commandRunnerSurface
      .flatMap {
        _.content match
          case SurfaceContent.CommandPalette(r) => Some(r)
          case _                                => None
      }
      .getOrElse(fail("Expected command runner surface"))

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

  private def settingsStateOnItem(
    groupId: String,
    itemId: String,
    config: AppConfig = AppConfig.default
  ): AppState =
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val effectiveConfig =
      if itemId == "animation-duration" || itemId == "animation-steps" then config.withMotionPreset(MotionPreset.Custom)
      else config
    val searchedRunner = CommandRunner.empty
      .activate(registry, effectiveConfig)
      .openSettings
      .updateSearchTerm(settingsGroupSearchTerm(groupId))
    val selectedIndex = searchedRunner.visibleItems.indexWhere(_.id == groupId) match
      case -1    => 0
      case index => index
    val baseRunner = searchedRunner.withSelectedVisibleIndex(selectedIndex)
    val group      = baseRunner.submenuGroup(groupId).getOrElse(fail(s"missing settings group $groupId"))
    val groupIndex =
      group.children.indexWhere(_.id == itemId) match
        case -1    => 0
        case index => index
    // issue #1059: a drilled-in settings group renders on the one command-runner surface now -- no more second
    // floating submenu surface or a separate focus target for it.
    val runner = baseRunner.enterSelectedGroup
      .withDrilledSettingsSurface(SettingsSurfaceState(SettingsPage.Group(groupId, groupIndex)))
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
      runtime = Runtime(uiSurfaces = List(surface))
    )

  private def settingsGroupSearchTerm(groupId: String): String =
    groupId.stripPrefix("settings-").replace("-", " ")

  "CommandRunnerReducer" should "open the exact settings leaf selected from search" in {
    val registry = CommandRegistry.default
    val searched = List('a', 'n', 'i', 'm', 'a', 't', 'i', 'o', 'n', ' ', 'd', 'u', 'r', 'a', 't', 'i', 'o', 'n')
      .foldLeft(activeState(registry, AppConfig.default.withMotionPreset(MotionPreset.Custom))) { (state, char) =>
        CommandRunnerReducer.reduce(RunnerInsertChar(char), state, registry).state
      }

    val opened = CommandRunnerReducer.reduce(RunnerSubmit, searched, registry).state
    val runner = runnerFrom(opened)

    runner.activeSubmenuGroupId shouldBe Some("settings-animation")
    runner.activeSubmenuSelectedItem.map(_.id) shouldBe Some("animation-duration")
  }

  it should "execute the global intent described by a direct setting search result" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default.withShowAllSettingsRegardlessOfMode(true))
      .copy(editingPresetName = Some("Review"))
      .updateSearchTerm("default document")
    val activated = activeState(registry)
    val state = activated.copy(
      runtime = activated.runtime.copy(uiSurfaces =
        List(
          UiSurface(
            SurfaceId("command-runner"),
            SurfaceContent.CommandPalette(runner),
            SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
          )
        )
      )
    )

    runner.selectedItem.collect {
      case item: CommandSurfaceItem.SettingSearchItem => (item.targetGroupId, item.sourceScope)
    } shouldBe Some(("settings-document-defaults", "Global"))

    val opened   = CommandRunnerReducer.reduce(RunnerSubmit, state, registry)
    val executed = CommandRunnerReducer.reduce(RunnerSubmit, opened.state, registry)

    executed.effects shouldBe List(
      AppEffect.ExecuteCommand(
        Command.typed(
          "default-document-mode",
          "Default Document",
          CommandIntent.View(ViewIntent.SetDefaultDocumentMode(DefaultDocumentMode.PlainText)),
          CommandCategory.Settings
        )
      )
    )
  }

  it should "open a unique preset action selected from direct search" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default)
      .updateSearchTerm("overwrite preset")
    val activated = activeState(registry)
    val state = activated.copy(
      runtime = activated.runtime.copy(uiSurfaces =
        List(
          UiSurface(
            SurfaceId("command-runner"),
            SurfaceContent.CommandPalette(runner),
            SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
          )
        )
      )
    )

    runner.selectedItem.collect {
      case item: CommandSurfaceItem.SettingSearchItem =>
        (item.targetGroupId, item.targetItemId, item.sourceScope)
    } shouldBe Some(("settings-preset-actions", "ui-preset-overwrite", "Preset"))

    val opened       = CommandRunnerReducer.reduce(RunnerSubmit, state, registry).state
    val openedRunner = runnerFrom(opened)

    openedRunner.activeSubmenuGroupId shouldBe Some("settings-preset-actions")
    openedRunner.activeSubmenuSelectedItem.map(_.id) shouldBe Some("ui-preset-overwrite")
  }

  // issue #1059: entering a settings group used to move focus to a second floating submenu surface; it now stays on
  // the one command-runner surface throughout (activeSettingsSurface is the signal, not a focus/surface change), and
  // Escape pops back out rather than "returning to a parent surface" that no longer exists.
  // issue #931: category tabs are retired -- browsing settings groups with no search now only happens via the
  // dedicated Settings surface (`.openSettings`), not by switching the palette's category.
  it should "enter the settings group on submit and pop back out on escape, staying on the one surface" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default)
      .openSettings
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

    val entered = CommandRunnerReducer.reduce(RunnerSubmit, state, registry)

    entered.state.commandRunnerSurface shouldBe defined
    entered.state.runtime.uiSurfaces should have size 1
    entered.state.persisted.focus shouldBe Focus.Surface(SurfaceId("command-runner"))
    runnerFrom(entered.state).activeSettingsSurface shouldBe defined

    val exited = CommandRunnerReducer.reduce(RunnerDismiss, entered.state, registry)

    exited.state.commandRunnerSurface shouldBe defined
    exited.state.runtime.uiSurfaces should have size 1
    exited.state.persisted.focus shouldBe Focus.Surface(SurfaceId("command-runner"))
    runnerFrom(exited.state).activeSettingsSurface shouldBe None
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

    runnerFrom(state).activeSubmenuEditingItemId shouldBe Some("animation-duration")

    val escaped = CommandRunnerReducer.reduce(RunnerDismiss, state, registry)
    val runner  = runnerFrom(escaped.state)

    runner.activeSubmenuEditingItemId shouldBe None
    runner.activeSubmenuEditingText shouldBe Some("")
    escaped.state.runtime.uiSurfaces should have size 1
    escaped.state.persisted.focus shouldBe Focus.Surface(SurfaceId("command-runner"))
  }

  it should "preserve submenu selection when exiting to the parent and re-entering the same group" in {
    val registry = CommandRegistry.default
    val state    = settingsStateOnItem("settings-animation", "animation-steps")

    val exited    = CommandRunnerReducer.reduce(RunnerDismiss, state, registry)
    val reentered = CommandRunnerReducer.reduce(RunnerSubmit, exited.state, registry)
    val runner    = runnerFrom(reentered.state)

    runner.activeSubmenuSelectedIndex shouldBe Some(16)
    runner.activeSubmenuSelectedItem.map(_.id) shouldBe Some("animation-steps")
  }

  // issue #1057: this used to filter within the (now-removed) "settings-language" group. The underlying mechanism --
  // typing filters a submenu's focused CommandItems, and submitting the (now singleton) filtered selection executes
  // it -- is generic to any submenu of CommandItems, so this retargets to "ui-font"'s font-family group (still real,
  // still present) instead, deriving the expected filtered content from the actual family list rather than
  // hardcoding font names (which are environment-dependent).
  it should "filter focused submenu items while typing and submit the filtered selection" in {
    val registry = CommandRegistry.default
    val entered =
      CommandRunnerReducer.reduce(RunnerSubmit, settingsStateOnItem("settings-ui-font", "ui-font"), registry).state
    val allFamilies = runnerFrom(entered).focusedSubmenuItems.collect {
      case CommandSurfaceItem.CommandItem(command) => command
    }
    val firstFamily = allFamilies.headOption.getOrElse(fail("no UI font families available"))
    val needle      = firstFamily.label.take(2)

    val searched =
      needle.foldLeft(entered)((s, char) => CommandRunnerReducer.reduce(RunnerInsertChar(char), s, registry).state)
    val runner = runnerFrom(searched)

    runner.activeSubmenuSearchTerm shouldBe Some(needle)
    val filtered = runner.focusedSubmenuItems.collect { case CommandSurfaceItem.CommandItem(command) => command }
    filtered should not be empty
    filtered.foreach(_.label.toLowerCase should include(needle.toLowerCase))
    filtered.head shouldBe firstFamily

    val submitted = CommandRunnerReducer.reduce(RunnerSubmit, searched, registry)
    submitted.effects.collectFirst { case AppEffect.ExecuteCommand(command) => command.intent } shouldBe Some(
      firstFamily.intent
    )
  }

  // issue #1057/#931: "settings-language" is gone and category-switching no longer affects what's shown (category
  // tabs are retired), so this now searches straight from a fresh palette -- text search alone reaches settings
  // regardless of any prior category, which is the whole point of #931's "fold into text search". "accessibility"
  // exact-matches the still-present "Accessibility" settings group by label -- "markdown" was tried first, but
  // collides with the newly-registered "lang-markdown" command's own label ("Markdown"), which now wins the exact
  // match instead.
  it should "open an exact single-word setting search at its target" in {
    val registry = CommandRegistry.default
    val searched = "accessibility".foldLeft(activeState(registry)) { (state, char) =>
      CommandRunnerReducer.reduce(RunnerInsertChar(char), state, registry).state
    }

    val opened = CommandRunnerReducer.reduce(RunnerSubmit, searched, registry)
    val runner = runnerFrom(opened.state)

    runner.activeSubmenuGroupId shouldBe Some("settings-accessibility")
    runner.activeSubmenuSelectedItem.map(_.id) shouldBe Some("motion-accessibility")
    runner.activeSubmenuSearchTerm shouldBe Some("")
  }

  it should "open font family picker submenus and submit UI font choices" in {
    val registry = CommandRegistry.default
    val state    = settingsStateOnItem("settings-ui-font", "ui-font")

    val entered = CommandRunnerReducer.reduce(RunnerSubmit, state, registry)
    val runner  = runnerFrom(entered.state)
    val firstUiFontIntent =
      runner.submenuItems("ui-font").collectFirst { case CommandSurfaceItem.CommandItem(command) => command.intent }

    runner.activeSubmenuGroupId shouldBe Some("ui-font")
    runner.activeSubmenuParentGroupId shouldBe Some("settings-ui-font")

    val submitted = CommandRunnerReducer.reduce(RunnerSubmit, entered.state, registry)
    submitted.effects.collectFirst { case AppEffect.ExecuteCommand(command) => command.intent } shouldBe
      firstUiFontIntent
  }

  it should "enter the edit preset submenu from UI presets" in {
    val registry = CommandRegistry.default
    val state    = settingsStateOnItem("settings-ui-presets", "settings-preset-edit")

    val entered = CommandRunnerReducer.reduce(RunnerSubmit, state, registry)
    val runner  = runnerFrom(entered.state)

    runner.activeSubmenuGroupId shouldBe Some("settings-preset-edit")
    runner.activeSubmenuParentGroupId shouldBe Some("settings-ui-presets")
    runner.focusedSubmenuItems.map(_.id) should contain allOf (
      "settings-preset-name",
      "settings-preset-actions",
      "settings-preset-active-panels",
      "settings-preset-theme",
      "settings-preset-animations",
      "settings-preset-fonts",
      "settings-preset-document-defaults"
    )
  }

  it should "preserve nested submenu ancestry for settings breadcrumbs" in {
    val registry = CommandRegistry.default
    val state    = settingsStateOnItem("settings-ui-presets", "settings-preset-edit")

    val presetOptions = CommandRunnerReducer.reduce(RunnerSubmit, state, registry).state
    val typographySelected = CommandRunnerReducer
      .reduce(RunnerSelectSubmenuItem(5), presetOptions, registry)
      .state
    val typography = CommandRunnerReducer.reduce(RunnerSubmit, typographySelected, registry)
    val runner     = runnerFrom(typography.state)

    runner.activeSubmenuGroupId shouldBe Some("settings-preset-fonts")
    runner.activeSubmenuParentGroupId shouldBe Some("settings-preset-edit")
    runner.activeSubmenuAncestorGroupIds shouldBe Some(List("settings-ui-presets", "settings-preset-edit"))
    runner.submenuBreadcrumbLabels("settings-preset-fonts") shouldBe List(
      "UI Presets",
      "Edit Preset: Writing",
      "Fonts"
    )
  }

  // issue #1059: Escape now uniformly pops one settings level at a time regardless of entry point (the settings
  // category tab inside the palette, exercised here, vs. the dedicated Settings surface, exercised in
  // SettingsSurfaceSpec) -- no more branching on `isSettingsSurface` to always fully deactivate instead of popping.
  it should "pop one settings level at a time on Escape, matching the dedicated Settings surface" in {
    val registry = CommandRegistry.default
    val state    = settingsStateOnItem("settings-ui-presets", "settings-preset-edit")

    val presetOptions      = CommandRunnerReducer.reduce(RunnerSubmit, state, registry).state
    val typographySelected = CommandRunnerReducer.reduce(RunnerSelectSubmenuItem(5), presetOptions, registry).state
    val typography         = CommandRunnerReducer.reduce(RunnerSubmit, typographySelected, registry).state
    runnerFrom(typography).activeSubmenuGroupId shouldBe Some("settings-preset-fonts")

    val backOnce = CommandRunnerReducer.reduce(Escape, typography, registry)
    runnerFrom(backOnce.state).activeSubmenuGroupId shouldBe Some("settings-preset-edit")

    val backTwice = CommandRunnerReducer.reduce(Escape, backOnce.state, registry)
    runnerFrom(backTwice.state).activeSubmenuGroupId shouldBe Some("settings-ui-presets")

    val backToRoot = CommandRunnerReducer.reduce(Escape, backTwice.state, registry)
    val rootRunner = runnerFrom(backToRoot.state)
    rootRunner.activeSettingsSurface shouldBe None
    rootRunner.isActive shouldBe true // the palette itself stays open -- this Escape only closed the submenu stack
  }

  it should "preserve preset submenu ancestry when entering a nested settings group from search results" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val searchedRunner = CommandRunner.empty
      .activate(registry, AppConfig.default)
      .openSettings
      .updateSearchTerm("fonts")
    val runner = searchedRunner.withSelectedItem("settings-preset-fonts")
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

    runner.selectedItem.map(_.id) shouldBe Some("settings-preset-fonts")

    val entered       = CommandRunnerReducer.reduce(RunnerSubmit, state, registry)
    val enteredRunner = runnerFrom(entered.state)

    enteredRunner.activeSubmenuGroupId shouldBe Some("settings-preset-fonts")
    enteredRunner.activeSubmenuParentGroupId shouldBe Some("settings-preset-edit")
    enteredRunner.activeSubmenuAncestorGroupIds shouldBe Some(
      List("settings-ui-presets", "settings-preset-edit")
    )
    enteredRunner.submenuBreadcrumbLabels("settings-preset-fonts") shouldBe List(
      "UI Presets",
      "Edit Preset: Writing",
      "Fonts"
    )
  }

  it should "open the matched settings leaf without filtering away its context" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val searchedRunner = CommandRunner.empty
      .activate(registry, AppConfig.default)
      .updateSearchTerm("UI Outline Thickness")
    val surface = UiSurface(
      SurfaceId("command-runner"),
      SurfaceContent.CommandPalette(searchedRunner),
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

    searchedRunner.selectedItem.map(_.id) shouldBe Some("settings-search:ui-outline-thickness")

    val entered = CommandRunnerReducer.reduce(RunnerSubmit, state, registry)
    val runner  = runnerFrom(entered.state)

    runner.activeSubmenuGroupId shouldBe Some("settings-interface-layout")
    runner.activeSubmenuSearchTerm shouldBe Some("")
    runner.searchTerm shouldBe "UI Outline Thickness"
    runner.activeSubmenuSelectedItem.map(_.id) shouldBe Some("ui-outline-thickness")
  }

  // issue #1057: "settings-language" is gone -- retargeted to "settings-cursor" (see the backspace test above).
  it should "clear submenu search with escape before leaving the submenu" in {
    val registry = CommandRegistry.default
    val searched = List('j', 'a').foldLeft(settingsStateOnItem("settings-cursor", "cursor-mode")) { (s, char) =>
      CommandRunnerReducer.reduce(RunnerInsertChar(char), s, registry).state
    }

    val cleared = CommandRunnerReducer.reduce(RunnerDismiss, searched, registry)
    val runner  = runnerFrom(cleared.state)

    runner.activeSubmenuSearchTerm shouldBe Some("")
    cleared.state.runtime.uiSurfaces should have size 1
    cleared.state.persisted.focus shouldBe Focus.Surface(SurfaceId("command-runner"))
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

    runner.activeSubmenuSelectedIndex shouldBe Some(16)
    runner.activeSubmenuEditingItemId shouldBe None
    runner.activeSubmenuEditingText shouldBe Some("")
  }
