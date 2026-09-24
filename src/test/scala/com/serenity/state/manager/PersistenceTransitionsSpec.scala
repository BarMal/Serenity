package com.serenity.state.manager

import com.serenity.command.*
import com.serenity.config.AppConfigMotionOps.*
import com.serenity.config.{AppConfig, HotkeyAction, VisualFlairLevel}
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.presets.UiPreset
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The pure decisions behind config, UI-preset and keybinding effects (#1697 Wave 3). Each used to be a bare
  * `stateRef.update`; now each is committed through `AppStateValidation`, so each is checked here to produce a state
  * that validation accepts.
  */
class PersistenceTransitionsSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val paletteId = SurfaceId("palette")

  private def paletteState(runner: CommandRunner = CommandRunner.empty): AppState =
    AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(focus = Focus.Surface(paletteId)),
      runtime = AppState.initial.runtime.copy(uiSurfaces =
        List(
          UiSurface(
            paletteId,
            SurfaceContent.CommandPalette(runner),
            SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
          )
        )
      )
    )

  private def runner(state: AppState): CommandRunner =
    state.surfaceById(paletteId).map(_.content) match
      case Some(SurfaceContent.CommandPalette(runner)) => runner
      case other                                       => fail(s"Expected a CommandPalette surface, got $other")

  private def shouldValidate(state: AppState): Unit =
    AppStateValidation.validated(state).isRight shouldBe true

  "A config update" should "land the new config and refresh the live command runner in one validated state" in {
    val updated = StateManagerConfigEffects.configUpdated(paletteState(), _.withWheelScrollLines(11))

    updated.persisted.config.inputConfig.wheelScrollLines shouldBe 11
    shouldValidate(updated)
  }

  "The companion sprite panel sync" should "dock the panel when enabled and remove it when flair is off, validly" in {
    val enabledConfig =
      AppConfig.default.withCompanionSpriteConfig(AppConfig.default.companionSpriteConfig.copy(enabled = true))
    val shown = StateManagerConfigEffects.withCompanionSpritePanel(
      AppState.initial.copy(persisted = AppState.initial.persisted.copy(config = enabledConfig)),
      enabledConfig
    )
    val flairOff = enabledConfig.withVisualFlairLevel(VisualFlairLevel.Off)
    val hidden   = StateManagerConfigEffects.withCompanionSpritePanel(shown, flairOff)

    shown.surfaceById(SurfaceId.CompanionSprite) should not be empty
    hidden.surfaceById(SurfaceId.CompanionSprite) shouldBe empty
    shouldValidate(shown)
    shouldValidate(hidden)
  }

  private def drilledRunner(itemId: String): CommandRunner =
    CommandRunner(
      isActive = true,
      surface = CommandRunnerSurface.Settings(
        root = CommandPaletteState(),
        drilled = Some(SettingsSurfaceState(SettingsPage.Editing(groupId = "keybindings", itemId, "")))
      )
    )

  "A focused keymap conflict" should "prompt on the live command runner, validly" in {
    val prompted = StateManagerKeybindingEffects.withFocusedKeymapConflictMessage("keymap-editor-move_right", "ctrl+j")(
      paletteState(drilledRunner("irrelevant"))
    )

    runner(prompted).statusMessage.exists(_.startsWith("Binding is already assigned")) shouldBe true
    shouldValidate(prompted)
  }

  "A global hotkey conflict" should "prompt on the live command runner, validly" in {
    val prompted = StateManagerKeybindingEffects.withGlobalKeymapConflictMessage(HotkeyAction.Quit, "ctrl+s")(
      paletteState(drilledRunner("irrelevant"))
    )

    runner(prompted).statusMessage.exists(_.startsWith("Binding is already assigned")) shouldBe true
    shouldValidate(prompted)
  }

  private val preset = UiPreset(name = "Custom", config = AppConfig.default, themeName = Theme.light.name)

  "Preset store feedback" should "refresh the previews and report a status, validly" in {
    val reported = UiPresetTransitions.withFeedback(
      paletteState(),
      Some(List(UiPreset.Preview.fromPreset(preset))),
      UiPresetContext.Status(Some("Custom"), "Preset overwritten. Configure Custom.")
    )

    runner(reported).statusMessage shouldBe Some("Preset overwritten. Configure Custom.")
    runner(reported).editingPresetName shouldBe Some("Custom")
    shouldValidate(reported)
  }

  it should "focus a just-created preset's editing group, validly" in {
    val focused = UiPresetTransitions.withFeedback(
      paletteState(),
      None,
      UiPresetContext.CreatedPresetFocused("Custom", "Preset saved. Configure Custom.")
    )

    runner(focused).activeSettingsSurface.map(_.current) shouldBe Some(SettingsPage.Group("settings-preset-edit"))
    focused.persisted.focus shouldBe Focus.Surface(paletteId)
    shouldValidate(focused)
  }

  it should "leave the state alone when no command runner is open" in {
    UiPresetTransitions.withFeedback(AppState.initial, None, UiPresetContext.Status(None, "Preset deleted.")) shouldBe
      AppState.initial
  }

  "A preset diff review" should "open on the command runner, creating one when none is open, validly" in {
    val reviewing = UiPresetTransitions.openDiffReview(AppState.initial, preset)

    reviewing.commandRunnerSurface.map(_.content) match
      case Some(SurfaceContent.CommandPalette(runner)) =>
        runner.surface shouldBe a[CommandRunnerSurface.PresetDiffReview]
      case other => fail(s"Expected a CommandPalette surface, got $other")
    shouldValidate(reviewing)
  }

  "A preset apply request" should "number itself after the one still pending" in {
    val (first, firstRequest)   = UiPresetTransitions.requestApply(AppState.initial)
    val (second, secondRequest) = UiPresetTransitions.requestApply(first)

    (firstRequest, secondRequest) shouldBe (1L, 2L)
    second.runtime.pendingUiPresetApply shouldBe Some(2L)
  }

  "A resolved preset apply" should "apply only while it is still the pending request" in {
    val (requested, request) = UiPresetTransitions.requestApply(AppState.initial)
    val (superseded, _)      = UiPresetTransitions.requestApply(requested)
    val markTheme: AppState => AppState =
      state => state.copy(persisted = state.persisted.copy(theme = Theme.light))

    val applied = EffectResult.applyIfCurrent(
      requested,
      EffectResult.UiPresetApplyResolved(request, UiPresetApplyResolution.Apply(preset, markTheme))
    )
    val dropped = EffectResult.applyIfCurrent(
      superseded,
      EffectResult.UiPresetApplyResolved(request, UiPresetApplyResolution.Apply(preset, markTheme))
    )

    applied.persisted.theme.name shouldBe Theme.light.name
    applied.runtime.pendingUiPresetApply shouldBe None
    dropped shouldBe theSameInstanceAs(superseded)
  }

  it should "clear its own pending request when abandoned, restoring the state it started from" in {
    val (requested, request) = UiPresetTransitions.requestApply(AppState.initial)

    EffectResult.applyIfCurrent(
      requested,
      EffectResult.UiPresetApplyResolved(request, UiPresetApplyResolution.Abandon)
    ) shouldBe AppState.initial
  }

  it should "report a rejection on the command runner, validly" in {
    val (requested, request) = UiPresetTransitions.requestApply(paletteState())

    val rejected = EffectResult.applyIfCurrent(
      requested,
      EffectResult.UiPresetApplyResolved(request, UiPresetApplyResolution.Reject("Custom", "Cannot preview Custom: no"))
    )

    runner(rejected).statusMessage shouldBe Some("Cannot preview Custom: no")
    rejected.runtime.pendingUiPresetApply shouldBe None
    shouldValidate(rejected)
  }
