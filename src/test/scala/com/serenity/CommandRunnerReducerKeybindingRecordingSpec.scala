package com.serenity

import com.serenity.command.*
import com.serenity.config.*
import com.serenity.keystroke.events.*
import com.serenity.keystroke.{InputKey, KeyStrokeInfo, KeyboardFidelityTier, Modifier}
import com.serenity.state.models.*
import com.serenity.state.reducers.{AppEffect, CommandRunnerReducer}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** `CommandRunnerReducer`'s keymap-binding recording flow (double-tap detection, expiry, and the bare-modifier
  * fidelity warning) -- split out of `CommandRunnerReducerSpec` to keep each file focused on one concern.
  */
class CommandRunnerReducerKeybindingRecordingSpec extends AnyFlatSpec with Matchers:

  extension (runner: CommandRunner)
    private def activeSubmenuRecordingItemId: Option[String] =
      runner.activeSettingsSurface.flatMap(_.current.recording).map(_.itemId)
    private def activeSubmenuPendingRecordedBinding: Option[(KeyStrokeInfo, Long)] =
      runner.activeSettingsSurface.flatMap(_.current.recording).flatMap(_.pendingRecordedBinding)

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

  "CommandRunnerReducer" should "enter binding recording mode for a selected keymap input" in {
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
