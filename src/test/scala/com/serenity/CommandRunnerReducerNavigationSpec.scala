package com.serenity

import com.serenity.command.*
import com.serenity.config.*
import com.serenity.config.AppConfigMotionOps.*
import com.serenity.keystroke.events.*
import com.serenity.state.models.*
import com.serenity.state.reducers.{AppEffect, CommandRunnerReducer}
import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** `CommandRunnerReducer`'s Up/Down/Left/Right navigation -- root-level movement, left/right option cycling inside a
  * submenu, and the inline group-preview behavior -- split out of `CommandRunnerReducerSpec` to keep each file
  * focused on one concern.
  */
class CommandRunnerReducerNavigationSpec extends AnyFlatSpec with Matchers:

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

  "CommandRunnerReducer" should "leave the state unchanged when left and right are pressed on non-option rows" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val state             = activeState(registry)

    val movedRight = CommandRunnerReducer.reduce(RunnerNavigate(Direction.Right), state, registry)
    movedRight.state shouldBe state

    val movedLeft = CommandRunnerReducer.reduce(RunnerNavigate(Direction.Left), state, registry)
    movedLeft.state shouldBe state
  }

  it should "adjust the selected motion accessibility option inside the submenu with left and right" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val state             = settingsStateOnItem("settings-animation", "motion-accessibility")

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
        case option: CommandSurfaceItem.OptionItem if option.id == "motion-accessibility" => option.selectedOption
      }
      .shouldBe(Some("Off"))

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
        case option: CommandSurfaceItem.OptionItem if option.id == "motion-accessibility" => option.selectedOption
      }
      .shouldBe(Some("Standard"))
  }

  it should "adjust the selected background style inside the surface appearance submenu with left and right" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    // "settings-surface-appearance" nests one level under the top-level "settings-appearance-motion" group, so it's
    // reached directly here rather than through withSelectedItem/enterSelectedGroup (which only resolve a top-level
    // selection) -- this test is about adjustSelectedSubmenuOption's Left/Right behavior once inside a group, not
    // about the navigation path to reach it.
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default)
      .openSettings
      .withDrilledSettingsSurface(SettingsSurfaceState(SettingsPage.Group("settings-surface-appearance")))
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
        command.intent == CommandIntent.Settings(
          SettingsIntent.General(GeneralSettingsIntent.SetBackgroundStyle(BackgroundStyle.Frosted))
        )
      case _ =>
        false
    } shouldBe true
  }

  it should "adjust the selected interface density inside the interface layout submenu" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    // "settings-interface-layout" nests one level under the top-level "settings-appearance-motion" group -- see the
    // same note on the background-style test above.
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default)
      .openSettings
      .withDrilledSettingsSurface(SettingsSurfaceState(SettingsPage.Group("settings-interface-layout")))
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

    val movedRight = CommandRunnerReducer.reduce(RunnerNavigate(Direction.Right), state, registry)

    movedRight.effects.exists {
      case AppEffect.ExecuteCommand(command) =>
        command.intent == CommandIntent.Settings(
          SettingsIntent.PanelChrome(PanelChromeIntent.SetInterfaceDensity(InterfaceDensity.Spacious))
        )
      case _ =>
        false
    } shouldBe true
  }

  // issue #1059: a hovered-but-not-entered expandable settings row used to preview its children on a second floating
  // surface without moving focus. It previews inline in the same list now (SurfaceContentResolver's capped,
  // expand-in-place group preview) -- still without moving focus, but with no second surface at all.
  // issue #931: category tabs are retired -- browsing settings groups with no search now only happens via the
  // dedicated Settings surface (`.openSettings`), not by switching the palette's category.
  it should "preview an expandable settings row's children inline without moving focus" in {
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

    val previewed       = CommandRunnerReducer.reduce(RunnerNavigate(Direction.Down), state, registry)
    val previewedRunner = runnerFrom(previewed.state)

    previewed.state.commandRunnerSurface shouldBe defined
    previewed.state.runtime.uiSurfaces should have size 1
    previewed.state.persisted.focus shouldBe Focus.Surface(surface.id)

    val selectedGroup = previewedRunner.selectedItem
      .collect { case group: CommandSurfaceItem.GroupItem => group }
      .getOrElse(fail("Expected the second row to be an expandable settings group"))
    selectedGroup.children should not be empty

    val resolved = SurfaceContentResolver.resolve(
      SurfaceContent.CommandPalette(previewedRunner),
      LayoutRect(0, 0, 90, 30),
      SurfaceRenderMode.Floating
    )
    val selectedRowIndex = resolved.rows.indexWhere(_.selected)
    selectedRowIndex should be >= 0
    resolved.rows.lift(selectedRowIndex + 1).map(_.leadingPadding) shouldBe Some(2)
  }
