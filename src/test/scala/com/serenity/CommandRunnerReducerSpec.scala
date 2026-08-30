package com.serenity

import com.serenity.command.*
import com.serenity.config.*
import com.serenity.keystroke.events.*
import com.serenity.keystroke.{InputKey, KeyStrokeInfo, KeyboardFidelityTier, Modifier}
import com.serenity.state.models.*
import com.serenity.state.reducers.{AppEffect, CommandRunnerReducer}
import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CommandRunnerReducerSpec extends AnyFlatSpec with Matchers:

  /** Read-only conveniences mirroring the old flat `CommandRunnerSubmenuState`'s accessors, on top of the page-stack
    * `activeSettingsSurface` that replaced it (issue #1059) -- kept local to this spec since production code has no
    * need for them (it reads through `settingsSurfaceItems`/`settingsSurfaceSelectedIndex`/`focusedSubmenuItems`
    * instead).
    */
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
    private def activeSubmenuRecordingItemId: Option[String] =
      runner.activeSettingsSurface.flatMap(_.current.recording).map(_.itemId)
    private def activeSubmenuPendingRecordedBinding: Option[(KeyStrokeInfo, Long)] =
      runner.activeSettingsSurface.flatMap(_.current.recording).flatMap(_.pendingRecordedBinding)
    private def activeSubmenuSelectedItem: Option[CommandSurfaceItem] =
      runner.focusedSubmenuItems.lift(runner.settingsSurfaceSelectedIndex)

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

  // issue #931: category tabs -- and the `RunnerSelectCategory` event only they ever produced -- are retired outright;
  // there is nothing left for this test to exercise (see `CommandRunnerOneShotActionsSpec`/the PR notes for the
  // broader cleanup).

  it should "delete the previous word from the search term" in {
    val registry = CommandRegistry.default
    val state    = activeState(registry)
    val typed = List('a', 'l', 'p', 'h', 'a', ' ', 'b', 'e', 't', 'a').foldLeft(state) { (s, c) =>
      CommandRunnerReducer.reduce(InsertChar(c), s, registry).state
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

  it should "enter binding recording mode for a selected keymap input" in {
    val registry = CommandRegistry.default
    val base     = CommandRunner.empty.activate(registry, AppConfig.default).openSettings
    val items    = base.submenuItems("settings-keymap")
    val runner = base.withDrilledSettingsSurface(
        SettingsSurfaceState(
          SettingsPage.Group("settings-keymap", items.indexWhere(_.id == "keymap-global-find"))
        )
      )
    val activated = activeState(registry)
    val state = activated.copy(
      persisted = activated.persisted.copy(focus = Focus.Surface(SurfaceId("command-runner"))),
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

    val result = CommandRunnerReducer.reduce(Enter, state, registry)

    runnerFrom(result.state).activeSubmenuRecordingItemId shouldBe Some("keymap-global-find")
    runnerFrom(result.state).statusMessage shouldBe Some("Press a key or shortcut to assign")
  }

  it should "assign a recorded key and submit its setting intent" in {
    val registry = CommandRegistry.default
    val base     = CommandRunner.empty.activate(registry, AppConfig.default).openSettings
    val runner = base.withDrilledSettingsSurface(
        SettingsSurfaceState(
          SettingsPage.Editing(
            groupId = "settings-keymap",
            itemId = "keymap-global-find",
            draftText = "",
            recording = Some(RecordingState("keymap-global-find"))
          )
        )
      )
    val activated = activeState(registry)
    val state = activated.copy(
      persisted = activated.persisted.copy(focus = Focus.Surface(SurfaceId("command-runner"))),
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

    val result = CommandRunnerReducer.reduce(
      RunnerRecordBinding(
        KeyStrokeInfo(InputKey.Ctrl, None, Set.empty),
        1_000L
      ),
      state,
      registry
    )

    result.effects shouldBe List(AppEffect.ScheduleCommandRunnerBindingExpiry(1_000L))
    runnerFrom(result.state).activeSubmenuRecordingItemId shouldBe Some("keymap-global-find")
    runnerFrom(result.state).activeSubmenuPendingRecordedBinding.map(_._1) shouldBe
      Some(KeyStrokeInfo(InputKey.Ctrl, None, Set.empty))

    val completed = CommandRunnerReducer.reduce(
      RunnerRecordBinding(
        KeyStrokeInfo(InputKey.Ctrl, None, Set.empty),
        1_200L
      ),
      result.state,
      registry
    )

    completed.effects.head match
      case AppEffect.ExecuteCommand(command) =>
        command.intent shouldBe CommandIntent.Keybindings(
          KeybindingsIntent.SetGlobalHotkey(HotkeyAction.Find, "ctrl+ctrl")
        )
      case other => fail(s"Expected setting command, got $other")
    runnerFrom(completed.state).activeSubmenuRecordingItemId shouldBe None
  }

  it should "assign a modifier double tap when the matching second stroke arrives within 200ms" in {
    val registry = CommandRegistry.default
    val base     = CommandRunner.empty.activate(registry, AppConfig.default).openSettings
    val runner = base.withDrilledSettingsSurface(
        SettingsSurfaceState(
          SettingsPage.Editing(
            groupId = "settings-keymap",
            itemId = "keymap-global-find",
            draftText = "",
            recording = Some(RecordingState("keymap-global-find"))
          )
        )
      )
    val activated = activeState(registry)
    val state = activated.copy(
      persisted = activated.persisted.copy(focus = Focus.Surface(SurfaceId("command-runner"))),
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

    val first = CommandRunnerReducer.reduce(
      RunnerRecordBinding(KeyStrokeInfo(InputKey.Ctrl, None, Set.empty), 1_000L),
      state,
      registry
    )
    first.effects.collectFirst { case AppEffect.ExecuteCommand(_) => true } shouldBe None
    runnerFrom(first.state).activeSubmenuPendingRecordedBinding.map(_._2) shouldBe Some(1_000L)

    val result = CommandRunnerReducer.reduce(
      RunnerRecordBinding(KeyStrokeInfo(InputKey.Ctrl, None, Set.empty), 1_200L),
      first.state,
      registry
    )

    result.effects.collectFirst { case AppEffect.ExecuteCommand(command) => command.intent } shouldBe Some(
      CommandIntent.Keybindings(KeybindingsIntent.SetGlobalHotkey(HotkeyAction.Find, "ctrl+ctrl"))
    )
  }

  it should "finalize a pending single key after the double-tap window expires" in {
    val registry = CommandRegistry.default
    val base     = CommandRunner.empty.activate(registry, AppConfig.default).openSettings
    val runner = base.withDrilledSettingsSurface(
        SettingsSurfaceState(
          SettingsPage.Editing(
            groupId = "settings-keymap",
            itemId = "keymap-global-find",
            draftText = "",
            recording = Some(
              RecordingState(
                "keymap-global-find",
                pendingRecordedBinding = Some(KeyStrokeInfo(InputKey.Character, Some('k'), Set.empty) -> 1_000L)
              )
            )
          )
        )
      )
    val activated = activeState(registry)
    val state = activated.copy(
      persisted = activated.persisted.copy(focus = Focus.Surface(SurfaceId("command-runner"))),
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

    val result = CommandRunnerReducer.reduce(
      RunnerRecordBinding(KeyStrokeInfo(InputKey.Character, Some('k'), Set.empty), 1_201L),
      state,
      registry
    )

    result.effects.collectFirst { case AppEffect.ExecuteCommand(command) => command.intent } shouldBe Some(
      CommandIntent.Keybindings(KeybindingsIntent.SetGlobalHotkey(HotkeyAction.Find, "k"))
    )
    runnerFrom(result.state).activeSubmenuRecordingItemId shouldBe None
  }

  /** Builds active state for the settings keymap submenu, mid-recording, at a chosen tier -- shared by the
    * tier-fidelity warning specs below (issue #1194).
    */
  private def recordingState(
    registry: CommandRegistry,
    isTuiMode: Boolean,
    keyboardFidelityTier: KeyboardFidelityTier
  ): AppState =
    val base = CommandRunner.empty
      .activate(registry, AppConfig.default, isTuiMode = isTuiMode, keyboardFidelityTier = keyboardFidelityTier)
      .openSettings
    val runner = base.withDrilledSettingsSurface(
        SettingsSurfaceState(
          SettingsPage.Editing(
            groupId = "settings-keymap",
            itemId = "keymap-global-find",
            draftText = "",
            recording = Some(RecordingState("keymap-global-find"))
          )
        )
      )
    AppState(
      persisted = Persisted(
        layout = Layout.empty,
        buffers = Map.empty,
        focus = Focus.Surface(SurfaceId("command-runner"))
      ),
      runtime = Runtime(
        uiSurfaces = List(
          UiSurface(
            SurfaceId("command-runner"),
            SurfaceContent.CommandPalette(runner),
            SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
          )
        ),
        isTuiMode = isTuiMode,
        keyboardFidelityTier = keyboardFidelityTier
      )
    )

  it should
    "warn that a recorded bare-modifier double tap won't fire on a TUI session capped at the ModifyOtherKeys tier" in {
      val registry = CommandRegistry.default
      val state =
        recordingState(registry, isTuiMode = true, keyboardFidelityTier = KeyboardFidelityTier.ModifyOtherKeys)

      val first = CommandRunnerReducer.reduce(
        RunnerRecordBinding(KeyStrokeInfo(InputKey.Ctrl, None, Set.empty), 1_000L),
        state,
        registry
      )
      val result = CommandRunnerReducer.reduce(
        RunnerRecordBinding(KeyStrokeInfo(InputKey.Ctrl, None, Set.empty), 1_100L),
        first.state,
        registry
      )

      result.effects.collectFirst { case AppEffect.ExecuteCommand(command) => command.intent } shouldBe Some(
        CommandIntent.Keybindings(KeybindingsIntent.SetGlobalHotkey(HotkeyAction.Find, "ctrl+ctrl"))
      )
      runnerFrom(result.state).statusMessage shouldBe Some(
        "\"ctrl+ctrl\" recorded, but won't fire -- this terminal can't send a bare-modifier key event " +
          "at its negotiated keyboard protocol tier"
      )
    }

  it should "not warn when a recorded bare-modifier double tap is captured at the full-fidelity kitty tier" in {
    val registry = CommandRegistry.default
    val state    = recordingState(registry, isTuiMode = true, keyboardFidelityTier = KeyboardFidelityTier.Full)

    val first = CommandRunnerReducer.reduce(
      RunnerRecordBinding(KeyStrokeInfo(InputKey.Ctrl, None, Set.empty), 1_000L),
      state,
      registry
    )
    val result = CommandRunnerReducer.reduce(
      RunnerRecordBinding(KeyStrokeInfo(InputKey.Ctrl, None, Set.empty), 1_100L),
      first.state,
      registry
    )

    runnerFrom(result.state).statusMessage shouldBe None
  }

  it should "not warn when a recorded binding is an ordinary combo, even under the ModifyOtherKeys tier" in {
    val registry = CommandRegistry.default
    val state = recordingState(registry, isTuiMode = true, keyboardFidelityTier = KeyboardFidelityTier.ModifyOtherKeys)

    val pending = CommandRunnerReducer.reduce(
      RunnerRecordBinding(KeyStrokeInfo(InputKey.Character, Some('k'), Set(Modifier.Ctrl)), 1_000L),
      state,
      registry
    )
    val result = CommandRunnerReducer.reduce(RunnerBindingRecordingExpired(1_000L), pending.state, registry)

    result.effects.collectFirst { case AppEffect.ExecuteCommand(command) => command.intent } shouldBe Some(
      CommandIntent.Keybindings(KeybindingsIntent.SetGlobalHotkey(HotkeyAction.Find, "ctrl+k"))
    )
    runnerFrom(result.state).statusMessage shouldBe None
  }

  it should "ignore an expiry event for a replaced pending recording" in {
    val registry = CommandRegistry.default
    val base     = CommandRunner.empty.activate(registry, AppConfig.default).openSettings
    val runner = base.withDrilledSettingsSurface(
        SettingsSurfaceState(
          SettingsPage.Editing(
            groupId = "settings-keymap",
            itemId = "keymap-global-find",
            draftText = "",
            recording = Some(
              RecordingState(
                "keymap-global-find",
                pendingRecordedBinding = Some(KeyStrokeInfo(InputKey.Character, Some('j'), Set.empty) -> 2_000L)
              )
            )
          )
        )
      )
    val activated = activeState(registry)
    val state = activated.copy(
      persisted = activated.persisted.copy(focus = Focus.Surface(SurfaceId("command-runner"))),
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

    val result = CommandRunnerReducer.reduce(RunnerBindingRecordingExpired(1_000L), state, registry)

    result.state shouldBe state
    result.effects shouldBe Nil
  }

  // issue #931: category tabs -- and the `activeCategory` field they drove -- are retired outright; Tab/Shift+Tab
  // translate to nothing for the command runner now (`SurfaceInputTranslationSpec` covers that), so there is nothing
  // left here to exercise.

  it should "leave the state unchanged when left and right are pressed on non-option rows" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val state             = activeState(registry)

    val movedRight = CommandRunnerReducer.reduce(RunnerNavigate(Direction.Right), state, registry)
    movedRight.state shouldBe state

    val movedLeft = CommandRunnerReducer.reduce(RunnerNavigate(Direction.Left), state, registry)
    movedLeft.state shouldBe state
  }

  it should "open the exact settings leaf selected from search" in {
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
      .activate(registry, AppConfig.default)
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

  // issue #931: category tabs are retired -- there is no category left to narrow by, so search is unconditionally
  // global now (this was already true once typing began, before this migration; it's simply the only behavior left).
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

  // issue #1059: entering a settings group used to move focus to a second floating submenu surface; it now stays on
  // the one command-runner surface throughout (activeSettingsSurface is the signal, not a focus/surface change), and
  // Escape pops back out rather than "returning to a parent surface" that no longer exists.
  // issue #931: category tabs are retired -- see the previous test's note on `.openSettings`.
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
    val needle       = firstFamily.label.take(2)

    val searched = needle.foldLeft(entered) { (s, char) =>
      CommandRunnerReducer.reduce(RunnerInsertChar(char), s, registry).state
    }
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
    runner.activeSubmenuGroupId shouldBe Some("settings-cursor")
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

    runner.activeSubmenuEditingItemId shouldBe None
    runner.activeSubmenuEditingText shouldBe Some("")
  }

  it should "leave a selected submenu input item unchanged when enter is pressed before typing" in {
    val registry = CommandRegistry.default
    val state    = settingsStateOnItem("settings-animation", "animation-duration")

    val submitted = CommandRunnerReducer.reduce(RunnerSubmit, state, registry)
    val runner    = runnerFrom(submitted.state)

    runner.activeSubmenuEditingItemId shouldBe None
    runner.activeSubmenuEditingText shouldBe Some("")
    submitted.effects shouldBe Nil
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
    val state    = settingsStateOnItem("settings-navigation", "document-comment")

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

  it should "paste clipboard text into a selected submenu input item" in {
    val registry  = CommandRegistry.default
    val baseState = settingsStateOnItem("settings-interface-layout", "ui-outline-thickness")
    val state     = baseState.copy(runtime = baseState.runtime.copy(clipboard = Some("4")))

    val pasted = CommandRunnerReducer.reduce(Paste, state, registry)
    val runner = runnerFrom(pasted.state)

    runner.activeSubmenuEditingItemId shouldBe Some("ui-outline-thickness")
    runner.activeSubmenuEditingText shouldBe Some("4")
  }
