package com.serenity

import com.serenity.command.*
import com.serenity.config.*
import com.serenity.config.AppConfigMotionOps.*
import com.serenity.keystroke.events.*
import com.serenity.state.models.*
import com.serenity.state.reducers.{AppEffect, CommandRunnerReducer}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** `CommandRunnerReducer`'s character-level text editing -- insert-char, delete/delete-word variants, and paste, at
  * both the root search box and inside a submenu's search/input text -- split out of `CommandRunnerReducerSpec` to
  * keep each file focused on one concern.
  */
class CommandRunnerReducerTextEditingSpec extends AnyFlatSpec with Matchers:

  extension (runner: CommandRunner)
    private def activeSubmenuSearchTerm: Option[String] = runner.activeSettingsSurface.map(_.current.searchTerm)
    private def activeSubmenuEditingItemId: Option[String] =
      runner.activeSettingsSurface.flatMap(_.current.editingItemId)
    private def activeSubmenuEditingText: Option[String] = runner.activeSettingsSurface.map(_.current.draftText)

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

  private def runnerFrom(state: AppState): CommandRunner =
    state.commandRunnerSurface
      .flatMap {
        _.content match
          case SurfaceContent.CommandPalette(r) => Some(r)
          case _                                => None
      }
      .getOrElse(fail("Expected command runner surface"))

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

  "CommandRunnerReducer" should "delete the previous word from the search term" in {
    val registry = CommandRegistry.default
    val state    = activeState(registry)
    val typed = List('a', 'l', 'p', 'h', 'a', ' ', 'b', 'e', 't', 'a').foldLeft(state) { (s, c) =>
      CommandRunnerReducer.reduce(RunnerInsertChar(c), s, registry).state
    }

    val result = CommandRunnerReducer.reduce(RunnerDeleteWordBackward, typed, registry)

    runnerFrom(result.state).searchTerm shouldBe "alpha "
  }

  it should "paste clipboard text into the command search when active" in {
    val registry = CommandRegistry.default
    val base     = activeState(registry)
    val state    = base.copy(runtime = base.runtime.copy(clipboard = Some("UI Outline Thickness")))

    val result = CommandRunnerReducer.reduce(Paste, state, registry)

    runnerFrom(result.state).searchTerm shouldBe "UI Outline Thickness"
  }

  it should "select an input item without auto-entering edit mode" in {
    CommandRegistry.default
    val state  = settingsStateOnItem("settings-animation", "animation-duration")
    val runner = runnerFrom(state)

    runner.activeSubmenuEditingItemId shouldBe None
    runner.activeSubmenuEditingText shouldBe Some("")
  }

  it should "start editing on first typed digit and replace the saved value" in {
    val registry = CommandRegistry.default
    val state    = settingsStateOnItem("settings-animation", "animation-steps")

    val result = CommandRunnerReducer.reduce(RunnerInsertChar('5'), state, registry)
    runnerFrom(result.state).activeSubmenuEditingItemId shouldBe Some("animation-steps")
    runnerFrom(result.state).activeSubmenuEditingText shouldBe Some("5")
  }

  it should "reject non-numeric characters silently" in {
    val registry = CommandRegistry.default
    val state    = settingsStateOnItem("settings-animation", "animation-steps")

    val result = CommandRunnerReducer.reduce(RunnerInsertChar('x'), state, registry)
    runnerFrom(result.state).activeSubmenuEditingText shouldBe runnerFrom(state).activeSubmenuEditingText
  }

  it should "reject a decimal point on an integer InputItem" in {
    val registry = CommandRegistry.default
    val state    = settingsStateOnItem("settings-animation", "animation-steps")

    val result = CommandRunnerReducer.reduce(RunnerInsertChar('.'), state, registry)
    runnerFrom(result.state).activeSubmenuEditingText shouldBe runnerFrom(state).activeSubmenuEditingText
  }

  it should "accept a decimal point on a decimal InputItem once dots are cleared" in {
    val registry = CommandRegistry.default
    val state    = settingsStateOnItem("settings-surface-appearance", "blur-radius")

    val after0 = CommandRunnerReducer.reduce(RunnerInsertChar('0'), state, registry)
    val s0 = state.copy(runtime =
      state.runtime.copy(uiSurfaces =
        state.runtime.uiSurfaces.map(s => s.copy(content = SurfaceContent.CommandPalette(runnerFrom(after0.state))))
      )
    )

    val afterDot = CommandRunnerReducer.reduce(RunnerInsertChar('.'), s0, registry)
    runnerFrom(afterDot.state).activeSubmenuEditingText shouldBe Some("0.")
  }

  it should "reject a second decimal point" in {
    val registry = CommandRegistry.default
    val state    = settingsStateOnItem("settings-surface-appearance", "blur-radius")

    val after0 = runnerFrom(CommandRunnerReducer.reduce(RunnerInsertChar('0'), state, registry).state)
    val s1 = state.copy(runtime =
      state.runtime.copy(uiSurfaces =
        state.runtime.uiSurfaces.map(s => s.copy(content = SurfaceContent.CommandPalette(after0)))
      )
    )
    val afterDot = runnerFrom(CommandRunnerReducer.reduce(RunnerInsertChar('.'), s1, registry).state)
    val s2 =
      state.copy(runtime =
        state.runtime.copy(uiSurfaces =
          state.runtime.uiSurfaces.map(s => s.copy(content = SurfaceContent.CommandPalette(afterDot)))
        )
      )
    val afterSecondDot = runnerFrom(CommandRunnerReducer.reduce(RunnerInsertChar('.'), s2, registry).state)

    afterSecondDot.activeSubmenuEditingText shouldBe afterDot.activeSubmenuEditingText
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
    val textBefore = runner.activeSubmenuEditingText.getOrElse("")

    val result = CommandRunnerReducer.reduce(RunnerDeleteBackward, state, registry)
    runnerFrom(result.state).activeSubmenuEditingText shouldBe Some(textBefore.dropRight(1))
  }

  // issue #1059: Backspace only ever deletes text now -- it never falls back to navigating up a level once text is
  // already empty, which is the bug this migration fixes (see the rewritten SettingsSurfaceSpec test of the same
  // shape for the dedicated Settings surface).
  it should "be a no-op on backspace when there is no text to delete, never navigating up a level" in {
    val registry = CommandRegistry.default
    val state    = settingsStateOnItem("settings-animation", "animation-steps")
    val before   = runnerFrom(state).activeSettingsSurface

    before.flatMap(_.current.editingItemId) shouldBe None
    before.map(_.current.searchTerm) shouldBe Some("")

    val result = CommandRunnerReducer.reduce(RunnerDeleteBackward, state, registry)

    runnerFrom(result.state).activeSettingsSurface shouldBe before
  }

  // issue #1057: "settings-language" is gone -- retargeted to "settings-cursor", another still-present group; this
  // mechanism (delete one character of the submenu's search text) is generic to any group's search box, independent
  // of what's in it.
  it should "delete a character from the submenu search term via backspace, never navigating up" in {
    val registry = CommandRegistry.default
    val state    = settingsStateOnItem("settings-cursor", "cursor-mode")
    val searched = List('j', 'a', 'v').foldLeft(state) { (s, char) =>
      CommandRunnerReducer.reduce(RunnerInsertChar(char), s, registry).state
    }
    runnerFrom(searched).activeSubmenuSearchTerm shouldBe Some("jav")

    val afterBackspace = CommandRunnerReducer.reduce(RunnerDeleteBackward, searched, registry)
    val runner         = runnerFrom(afterBackspace.state)

    runner.activeSubmenuSearchTerm shouldBe Some("ja")
    runner.activeSubmenuEditingItemId shouldBe None
  }

  it should "paste clipboard text into a selected submenu input item" in {
    val registry  = CommandRegistry.default
    val baseState = settingsStateOnItem("settings-interface-layout", "ui-outline-thickness")
    val state     = baseState.copy(runtime = baseState.runtime.copy(clipboard = Some("4")))

    val pasted = CommandRunnerReducer.reduce(Paste, state, registry)
    val runner = runnerFrom(pasted.state)

    runner.activeSubmenuEditingItemId shouldBe Some("ui-outline-thickness")
    runner.activeSubmenuEditingText shouldBe Some("4")
  }

  it should "discard editing text when navigating to a different item" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val state             = settingsStateOnItem("settings-animation", "animation-steps")

    val typedState = List('5').foldLeft(state) { (s, c) =>
      val r = CommandRunnerReducer.reduce(RunnerInsertChar(c), s, registry)
      s.copy(runtime =
        s.runtime.copy(uiSurfaces =
          s.runtime.uiSurfaces.map(surf => surf.copy(content = SurfaceContent.CommandPalette(runnerFrom(r.state))))
        )
      )
    }

    val navigated = CommandRunnerReducer.reduce(RunnerNavigate(Direction.Down), typedState, registry)
    val runner    = runnerFrom(navigated.state)

    runner.activeSubmenuEditingItemId shouldNot be(Some("animation-steps"))
    runner.activeSubmenuEditingText shouldNot be(runnerFrom(typedState).activeSubmenuEditingText)
  }

  it should "restore the saved value when escape cancels a pending submenu edit" in {
    val registry = CommandRegistry.default
    val state = List('5').foldLeft(settingsStateOnItem("settings-animation", "animation-steps")) { (s, c) =>
      val r = CommandRunnerReducer.reduce(RunnerInsertChar(c), s, registry)
      s.copy(runtime =
        s.runtime.copy(uiSurfaces =
          s.runtime.uiSurfaces.map(surf => surf.copy(content = SurfaceContent.CommandPalette(runnerFrom(r.state))))
        )
      )
    }

    val cancelled = CommandRunnerReducer.reduce(Escape, state, registry)
    val runner    = runnerFrom(cancelled.state)
    val restoredValue = runner
      .submenuItems("settings-animation")
      .collectFirst { case item: CommandSurfaceItem.InputItem if item.id == "animation-steps" => item.currentValue }

    runner.activeSubmenuEditingItemId shouldBe None
    runner.activeSubmenuEditingText shouldBe Some("")
    restoredValue shouldBe Some("0")
  }
