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

/** `CommandRunnerReducer`'s Enter/submit behavior once inside a settings submenu -- committing an input item's text,
  * cycling an option, running a command, and the various validation/status-message outcomes -- split out of
  * `CommandRunnerReducerSpec` to keep each file focused on one concern.
  */
class CommandRunnerReducerSubmenuSubmitSpec extends AnyFlatSpec with Matchers:

  extension (runner: CommandRunner)
    private def activeSubmenuGroupId: Option[String] = runner.activeSettingsSurface.map(_.current.groupId)
    private def activeSubmenuEditingItemId: Option[String] =
      runner.activeSettingsSurface.flatMap(_.current.editingItemId)
    private def activeSubmenuEditingText: Option[String] = runner.activeSettingsSurface.map(_.current.draftText)

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

  "CommandRunnerReducer" should "leave a selected submenu input item unchanged when enter is pressed before typing" in {
    val registry = CommandRegistry.default
    val state    = settingsStateOnItem("settings-animation", "animation-duration")

    val submitted = CommandRunnerReducer.reduce(RunnerSubmit, state, registry)
    val runner    = runnerFrom(submitted.state)

    runner.activeSubmenuEditingItemId shouldBe None
    runner.activeSubmenuEditingText shouldBe Some("")
    submitted.effects shouldBe Nil
  }

  // #1298: an ordinary settings CommandItem still closes the whole command-runner overlay on submit -- only a
  // command explicitly marked `keepMenuOpenOnSubmit` (the info bar segment movers, below) keeps it open.
  it should "close the command-runner overlay when an ordinary settings command is submitted from a submenu" in {
    val registry = CommandRegistry.default
    // "ui-font" is itself a nested group of font-family choices -- entering it is a navigation step, not yet a
    // command submit, exactly as in "open font family picker submenus and submit UI font choices" above.
    val entered =
      CommandRunnerReducer.reduce(RunnerSubmit, settingsStateOnItem("settings-ui-font", "ui-font"), registry)

    val submitted = CommandRunnerReducer.reduce(RunnerSubmit, entered.state, registry)

    submitted.state.commandRunnerSurface shouldBe None
  }

  // #1298: reordering the cursor info bar's segments used to close the whole settings menu on every single move,
  // forcing a full re-open/re-navigate round trip to nudge one segment more than one step. Moving a segment now
  // leaves the "Cursor" group open so the next move can be submitted immediately.
  it should "keep the settings submenu open after moving a cursor info bar segment" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val config = AppConfig.default.withCursorInfoBarSegments(
      List(CursorInfoBarSegment.Position, CursorInfoBarSegment.Title)
    )
    // Mirrors settingsStateOnItem's own navigation: search narrows visibleItems to the target group before
    // indexing into it, exactly as reaching any other settings group does elsewhere in this spec.
    val searched = CommandRunner.empty
      .activate(registry, config)
      .openSettings
      .updateSearchTerm(settingsGroupSearchTerm("settings-cursor"))
    val groupIndex = searched.visibleItems.indexWhere(_.id == "settings-cursor")
    val entered    = searched.withSelectedVisibleIndex(groupIndex).enterSelectedGroup
    val moveIndex  = entered.submenuItems("settings-cursor").indexWhere(_.id == "move-cursor-info-bar-position-later")
    val positioned = entered.withSelectedFocusedSubmenuIndex(moveIndex)
    val surface = UiSurface(
      SurfaceId("command-runner"),
      SurfaceContent.CommandPalette(positioned),
      SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
    )
    val state = AppState(
      persisted = Persisted(layout = Layout.empty, buffers = Map.empty, focus = Focus.Surface(surface.id)),
      runtime = Runtime(uiSurfaces = List(surface), focusHistory = List(Focus.EditorPane(PaneId(2))))
    )

    val submitted = CommandRunnerReducer.reduce(RunnerSubmit, state, registry)

    submitted.state.commandRunnerSurface shouldBe defined
    runnerFrom(submitted.state).activeSubmenuGroupId shouldBe Some("settings-cursor")
    submitted.effects.collectFirst { case AppEffect.ExecuteCommand(command) => command.name } shouldBe
      Some("move-cursor-info-bar-position-later")
  }

  it should "fire SetAnimationSteps intent on Enter with valid value" in {
    val registry = CommandRegistry.default
    val state    = settingsStateOnItem("settings-animation", "animation-steps")

    val typed = List('2', '0').foldLeft(state) { (s, c) =>
      val r = CommandRunnerReducer.reduce(RunnerInsertChar(c), s, registry)
      s.copy(runtime =
        s.runtime.copy(uiSurfaces =
          s.runtime.uiSurfaces.map(surf => surf.copy(content = SurfaceContent.CommandPalette(runnerFrom(r.state))))
        )
      )
    }

    val result = CommandRunnerReducer.reduce(RunnerSubmit, typed, registry)
    result.effects.exists {
      case AppEffect.ExecuteCommand(command) =>
        command.intent == CommandIntent.Settings(SettingsIntent.General(GeneralSettingsIntent.SetAnimationSteps(20)))
      case _ =>
        false
    } shouldBe true
  }

  it should "emit authored document comment text from the navigation input item" in {
    val registry = CommandRegistry.default
    val state = settingsStateOnItem(
      "settings-navigation",
      "document-comment",
      config = AppConfig.default.withShowAllSettingsRegardlessOfMode(true)
    )

    val typed =
      "Tighten this opening".foldLeft(state)((s, char) =>
        CommandRunnerReducer.reduce(RunnerInsertChar(char), s, registry).state
      )

    val result = CommandRunnerReducer.reduce(RunnerSubmit, typed, registry)

    result.effects.collectFirst { case AppEffect.ExecuteCommand(command) => command.intent } shouldBe Some(
      CommandIntent.Comments(CommentsIntent.AddDocumentComment("Tighten this opening"))
    )
  }

  it should "be a no-op on Enter when the value is out of bounds" in {
    val registry = CommandRegistry.default
    val state    = settingsStateOnItem("settings-animation", "animation-steps")

    val typedOutOfBounds = List('9', '9', '9').foldLeft(state) { (s, c) =>
      val r = CommandRunnerReducer.reduce(RunnerInsertChar(c), s, registry)
      s.copy(runtime =
        s.runtime.copy(uiSurfaces =
          s.runtime.uiSurfaces.map(surf => surf.copy(content = SurfaceContent.CommandPalette(runnerFrom(r.state))))
        )
      )
    }

    val result = CommandRunnerReducer.reduce(RunnerSubmit, typedOutOfBounds, registry)
    result.effects shouldBe Nil
  }

  it should "edit keymap binding text and emit a focused keymap update intent" in {
    val registry = CommandRegistry.default
    val state    = settingsStateOnItem("settings-keymap", "keymap-command-runner-submit")

    val typed =
      "ctrl+enter".foldLeft(state)((s, char) => CommandRunnerReducer.reduce(RunnerInsertChar(char), s, registry).state)

    val result = CommandRunnerReducer.reduce(RunnerSubmit, typed, registry)

    result.effects.collectFirst { case AppEffect.ExecuteCommand(command) => command.intent } shouldBe Some(
      CommandIntent.Keybindings(
        KeybindingsIntent.SetCommandRunnerKeyBinding(CommandRunnerKeyAction.Submit, "ctrl+enter")
      )
    )
  }

  it should "emit a keymap reset intent when a binding field is set to reset" in {
    val registry = CommandRegistry.default
    val config = AppConfig.default
      .withKeymapBinding(KeymapGroup.CommandRunner)(CommandRunnerKeyAction.Submit, "ctrl+enter")
    val state = settingsStateOnItem("settings-keymap", "keymap-command-runner-submit", config)

    val typed =
      "reset".foldLeft(state)((s, char) => CommandRunnerReducer.reduce(RunnerInsertChar(char), s, registry).state)

    val result = CommandRunnerReducer.reduce(RunnerSubmit, typed, registry)

    result.effects.collectFirst { case AppEffect.ExecuteCommand(command) => command.intent } shouldBe Some(
      CommandIntent.Keybindings(KeybindingsIntent.ResetCommandRunnerKeyBinding(CommandRunnerKeyAction.Submit))
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
    runner.activeSubmenuEditingItemId shouldBe Some("keymap-command-runner-submit")
    runner.activeSubmenuEditingText shouldBe Some("ctrl")
  }

  it should "edit text area inset percentages and emit a layout update intent" in {
    val registry = CommandRegistry.default
    val state    = settingsStateOnItem("settings-text-area", "text-area-top")

    val typed =
      "22.5".foldLeft(state)((s, char) => CommandRunnerReducer.reduce(RunnerInsertChar(char), s, registry).state)

    val result = CommandRunnerReducer.reduce(RunnerSubmit, typed, registry)

    result.effects.collectFirst { case AppEffect.ExecuteCommand(command) => command.intent } shouldBe Some(
      CommandIntent.Settings(SettingsIntent.PanelChrome(PanelChromeIntent.SetTextAreaTopInset(0.225)))
    )
  }
