package com.serenity

import com.serenity.command.*
import com.serenity.config.AppConfig
import com.serenity.keystroke.events.*
import com.serenity.state.models.*
import com.serenity.state.reducers.CommandRunnerReducer
import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SettingsSurfaceSpec extends AnyFlatSpec with Matchers:

  private val registry = CommandRegistry.default

  private def stateFor(runner: CommandRunner): AppState =
    val surface = UiSurface(
      SurfaceId("command-runner"),
      SurfaceContent.CommandPalette(runner),
      SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
    )
    AppState(
      persisted = Persisted(
        buffers = Map.empty,
        layout = Layout.empty,
        focus = Focus.Surface(surface.id)
      ),
      runtime = Runtime(
        uiSurfaces = List(surface),
        focusHistory = List(Focus.EditorPane(PaneId(1)))
      )
    )

  "Settings surface" should "show peer categories and search leaves with their current values and paths" in {
    given CommandRegistry = registry
    val runner =
      CommandRunner.empty.activate(registry, AppConfig.default.withShowAllSettingsRegardlessOfMode(true)).openSettings

    runner.settingsSurfaceItems.collect {
      case group: CommandSurfaceItem.GroupItem => group.label
    } should contain allOf (
      "Document Writing",
      "Editor View",
      "Panels & Workspace",
      "Appearance & Motion",
      "Accessibility"
    )

    val searched = runner.updateSettingsSearch("default document")
    val result = searched.settingsSurfaceItems
      .collectFirst { case item: CommandSurfaceItem.SettingSearchItem => item }
      .getOrElse(fail("Expected matching setting"))
    result.effectiveValue shouldBe Some("Plain Text")
    result.breadcrumb should include("Document Writing")
  }

  // Rewritten for issue #1059: Backspace navigating up a level (the previous version of this test) was exactly the
  // overloaded-Backspace bug the page-stack migration fixes. Backspace now only ever deletes text -- it is a no-op
  // with nothing to delete, and never navigates -- while Escape uniformly does "up one level, or close" at every
  // depth, ending in a full dismiss once there is nothing left to pop.
  it should "make Backspace a no-op with no text to delete, and Escape go up one level at a time to dismiss" in {
    val opened = CommandRunner.empty
      .activate(registry, AppConfig.default.withShowAllSettingsRegardlessOfMode(true))
      .openSettings
      .withSelectedItem("settings-document-writing")
      .enterSelectedGroup
      .withSelectedFocusedSubmenuIndex(0)
      .enterSelectedSubmenuGroup

    opened.activeSettingsSurface.map(_.current.groupId) shouldBe Some("settings-navigation")

    val afterBackspace = CommandRunnerReducer.reduce(RunnerDeleteBackward, stateFor(opened), registry)
    runnerFrom(afterBackspace.state).activeSettingsSurface.map(_.current.groupId) shouldBe Some("settings-navigation")

    val back = CommandRunnerReducer.reduce(Escape, stateFor(opened), registry)
    runnerFrom(back.state).activeSettingsSurface.map(_.current.groupId) shouldBe Some("settings-document-writing")

    val backToRoot = CommandRunnerReducer.reduce(Escape, back.state, registry)
    runnerFrom(backToRoot.state).activeSettingsSurface shouldBe None
    runnerFrom(backToRoot.state).isActive shouldBe true

    val dismissed = CommandRunnerReducer.reduce(Escape, backToRoot.state, registry)
    dismissed.state.commandRunnerSurface shouldBe None
  }

  it should "open settings from the command runner without creating a submenu stack" in {
    given CommandRegistry = registry
    val palette = CommandRunner.empty
      .activate(registry, AppConfig.default)
      .updateSearchTerm("open settings")
      .withSelectedItem("open-settings")

    val opened = CommandRunnerReducer.reduce(Enter, stateFor(palette), registry)

    runnerFrom(opened.state).isSettingsSurface shouldBe true
    opened.state.runtime.uiSurfaces should have size 1
  }

  it should "render a single searchable settings surface with breadcrumbs and visible edit state" in {
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default)
      .openSettings
      .withSelectedItem("settings-appearance-motion")
      .enterSelectedGroup
      .withSelectedFocusedSubmenuIndex(1)
      .enterSelectedSubmenuGroup
      .withSelectedFocusedSubmenuIndex(4)
      .beginSubmenuEditMode

    val resolved = SurfaceContentResolver.resolve(
      SurfaceContent.CommandPalette(runner),
      LayoutRect(0, 0, 90, 16),
      SurfaceRenderMode.Floating
    )

    resolved.title shouldBe Some("Settings")
    resolved.header.map(_.plainText) shouldBe Some("Settings > Appearance & Motion > Surface Appearance")
    resolved.rows.exists(_.cursorColumn.nonEmpty) shouldBe true
    resolved.footer.map(_.plainText).getOrElse(fail("Expected settings footer")) should include("Back")
  }

  it should "describe the selected group, option, and input action in its footer" in {
    val root =
      CommandRunner.empty.activate(registry, AppConfig.default.withShowAllSettingsRegardlessOfMode(true)).openSettings
    val option = root.withDrilledSettingsSurface(
      SettingsSurfaceState(SettingsPage.Group("settings-surface-appearance"))
    )
    val input = option.withDrilledSettingsSurface(
      SettingsSurfaceState(SettingsPage.Group("settings-surface-appearance", selectedIndex = 4))
    )
    val editing = input.withDrilledSettingsSurface(
      SettingsSurfaceState(
        SettingsPage.Editing(groupId = "settings-surface-appearance", itemId = "blur-radius", draftText = "1")
      )
    )

    footerText(root) should include("Open")
    footerText(option) should include("Apply")
    footerText(input) should include("Edit")
    footerText(editing) should include("Save")
    List(root, option, input, editing).foreach { runner =>
      val footer = footerText(runner)
      footer should not include "Enter"
      footer should not include "Backspace"
      footer should not include "Esc"
      footer should not include "↑"
    }
  }

  private def footerText(runner: CommandRunner): String =
    SurfaceContentResolver
      .resolve(
        SurfaceContent.CommandPalette(runner),
        LayoutRect(0, 0, 90, 16),
        SurfaceRenderMode.Floating
      )
      .footer
      .map(_.plainText)
      .getOrElse(fail("Expected settings footer"))

  private def runnerFrom(state: AppState): CommandRunner =
    state.commandRunnerSurface
      .flatMap {
        _.content match
          case SurfaceContent.CommandPalette(runner) => Some(runner)
          case _                                     => None
      }
      .getOrElse(fail("Expected settings surface"))
