package com.serenity.state.manager

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import com.serenity.command.*
import com.serenity.config.*
import com.serenity.rope.Balance
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Exercises [[StateManagerKeybindingEffects]] on its own: global-hotkey and focused-keymap binding assignment, the
  * "already assigned" conflict prompt it surfaces on the one live `CommandPalette` surface, forced conflict resolution,
  * and resetting a binding back to its default -- each asserted through the resulting `AppConfig`/state rather than
  * through a fully composed `StateManager`.
  */
class StateManagerKeybindingEffectsSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val ConflictMessage =
    "Binding is already assigned. Enter to unbind the other action, or Escape to preserve it."

  final private class Harness(
      val stateRef: Ref[IO, AppState],
      val committedConfigs: Ref[IO, List[AppConfig]],
      val keybindings: StateManagerKeybindingEffects
  ):
    def currentConfig: AppConfig = stateRef.get.unsafeRunSync().persisted.config

  private def harness(initialState: AppState): Harness =
    val stateRef         = Ref.of[IO, AppState](initialState).unsafeRunSync()
    val committedConfigs = Ref.of[IO, List[AppConfig]](Nil).unsafeRunSync()

    new Harness(
      stateRef,
      committedConfigs,
      new StateManagerKeybindingEffects(
        stateRef,
        update =>
          stateRef
            .modify { state =>
              val config = update(state.persisted.config)
              (state.copy(persisted = state.persisted.copy(config = config)), config)
            }
            .flatTap(config => committedConfigs.update(_ :+ config))
      )
    )

  private def stateWithConfig(config: AppConfig, uiSurfaces: List[UiSurface] = Nil): AppState =
    AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(config = config),
      runtime = AppState.initial.runtime.copy(uiSurfaces = uiSurfaces)
    )

  private def configWithHotkeys(bindings: Map[HotkeyAction, List[HotkeyTrigger]]): AppConfig =
    AppConfig(inputConfig = InputConfig(hotkeyConfig = HotkeyConfig(bindings)))

  private def configWithEditorBindings(bindings: Map[EditorKeyAction, List[HotkeyTrigger]]): AppConfig =
    AppConfig(inputConfig =
      InputConfig(focusedKeymapConfig = FocusedKeymapConfig(editor = KeymapGroupConfig(bindings)))
    )

  private def configWithPanelBindings(bindings: Map[PanelKeyAction, List[HotkeyTrigger]]): AppConfig =
    AppConfig(inputConfig = InputConfig(focusedKeymapConfig = FocusedKeymapConfig(panel = KeymapGroupConfig(bindings))))

  private def trigger(binding: String): HotkeyTrigger =
    HotkeyTrigger.parse(binding).getOrElse(fail(s"'$binding' did not parse as a HotkeyTrigger"))

  /** A live `CommandPalette` surface drilled into a settings item's edit page, the one place a binding conflict gets
    * its "already assigned" message.
    */
  private def commandPaletteSurface(itemId: String, draftText: String = ""): UiSurface =
    val runner = CommandRunner(
      isActive = true,
      surface = CommandRunnerSurface.Settings(
        root = CommandPaletteState(),
        drilled = Some(SettingsSurfaceState(SettingsPage.Editing(groupId = "keybindings", itemId, draftText)))
      )
    )
    UiSurface(
      SurfaceId("command-runner"),
      SurfaceContent.CommandPalette(runner),
      SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
    )

  private def drilledPage(state: AppState): SettingsPage.Editing =
    state.commandRunnerSurface.map(_.content) match
      case Some(SurfaceContent.CommandPalette(runner)) =>
        runner.activeSettingsSurface.map(_.current) match
          case Some(editing: SettingsPage.Editing) => editing
          case other                               => fail(s"Expected a drilled Editing page, got $other")
      case other => fail(s"Expected a CommandPalette surface, got $other")

  "StateManagerKeybindingEffects" should "assign a fresh binding to a global hotkey action with no conflict" in {
    val config  = configWithHotkeys(Map(HotkeyAction.Save -> List(trigger("ctrl+s"))))
    val fixture = harness(stateWithConfig(config))

    fixture.keybindings.interpret(KeybindingsIntent.SetGlobalHotkey(HotkeyAction.Quit, "ctrl+q")).unsafeRunSync()

    val hotkeys = fixture.currentConfig.inputConfig.hotkeyConfig
    hotkeys.bindingsFor(HotkeyAction.Quit) shouldBe List(trigger("ctrl+q"))
    hotkeys.bindingsFor(HotkeyAction.Save) shouldBe List(trigger("ctrl+s"))
  }

  it should "leave the config untouched when a global hotkey binding conflict occurs with no command palette open" in {
    val config  = configWithHotkeys(Map(HotkeyAction.Save -> List(trigger("ctrl+s"))))
    val fixture = harness(stateWithConfig(config))

    fixture.keybindings.interpret(KeybindingsIntent.SetGlobalHotkey(HotkeyAction.Quit, "ctrl+s")).unsafeRunSync()

    fixture.currentConfig.inputConfig.hotkeyConfig shouldBe config.inputConfig.hotkeyConfig
    fixture.committedConfigs.get.unsafeRunSync() shouldBe Nil
  }

  it should "surface the already-assigned conflict message on the live command palette when a global hotkey collides" in {
    val config  = configWithHotkeys(Map(HotkeyAction.Save -> List(trigger("ctrl+s"))))
    val state   = stateWithConfig(config, uiSurfaces = List(commandPaletteSurface("irrelevant-item")))
    val fixture = harness(state)

    fixture.keybindings.interpret(KeybindingsIntent.SetGlobalHotkey(HotkeyAction.Quit, "ctrl+s")).unsafeRunSync()

    val after = fixture.stateRef.get.unsafeRunSync()
    after.persisted.config.inputConfig.hotkeyConfig shouldBe config.inputConfig.hotkeyConfig
    val page = drilledPage(after)
    page.itemId shouldBe s"keymap-global-${HotkeyAction.Quit.configKey}"
    page.draftText shouldBe "ctrl+s"
    page.recording.flatMap(_.pendingGlobalHotkeyConflict) shouldBe Some(HotkeyAction.Quit -> "ctrl+s")

    after.commandRunnerSurface.map(_.content) match
      case Some(SurfaceContent.CommandPalette(runner)) =>
        runner.statusMessage shouldBe Some(ConflictMessage)
      case other => fail(s"Expected a CommandPalette surface, got $other")
  }

  it should "force-reassign a global hotkey on conflict resolution, unbinding whichever action held it" in {
    val config  = configWithHotkeys(Map(HotkeyAction.Save -> List(trigger("ctrl+s"))))
    val fixture = harness(stateWithConfig(config))

    fixture.keybindings
      .interpret(KeybindingsIntent.ResolveGlobalHotkeyConflict(HotkeyAction.Quit, "ctrl+s"))
      .unsafeRunSync()

    val hotkeys = fixture.currentConfig.inputConfig.hotkeyConfig
    hotkeys.bindingsFor(HotkeyAction.Quit) shouldBe List(trigger("ctrl+s"))
    hotkeys.bindingsFor(HotkeyAction.Save) shouldBe Nil
  }

  it should "assign a fresh binding to an editor keymap action with no conflict" in {
    val config  = configWithEditorBindings(Map(EditorKeyAction.MoveLeft -> List(trigger("ctrl+j"))))
    val fixture = harness(stateWithConfig(config))

    fixture.keybindings
      .interpret(KeybindingsIntent.SetEditorKeyBinding(EditorKeyAction.MoveRight, "ctrl+k"))
      .unsafeRunSync()

    val editor = fixture.currentConfig.inputConfig.focusedKeymapConfig.editor
    editor.bindingsFor(EditorKeyAction.MoveRight) shouldBe List(trigger("ctrl+k"))
    editor.bindingsFor(EditorKeyAction.MoveLeft) shouldBe List(trigger("ctrl+j"))
  }

  it should "do nothing, and raise no conflict, when re-assigning an editor action's own current binding" in {
    val config  = configWithEditorBindings(Map(EditorKeyAction.MoveLeft -> List(trigger("ctrl+j"))))
    val state   = stateWithConfig(config, uiSurfaces = List(commandPaletteSurface("keymap-editor-move_left")))
    val fixture = harness(state)

    fixture.keybindings
      .interpret(KeybindingsIntent.SetEditorKeyBinding(EditorKeyAction.MoveLeft, "ctrl+j"))
      .unsafeRunSync()

    fixture.currentConfig.inputConfig.focusedKeymapConfig.editor shouldBe config.inputConfig.focusedKeymapConfig.editor
    fixture.committedConfigs.get.unsafeRunSync() shouldBe Nil
    drilledPage(fixture.stateRef.get.unsafeRunSync()).recording shouldBe None
  }

  it should "surface the focused-keymap conflict message when an editor binding belongs to a different action" in {
    val config  = configWithEditorBindings(Map(EditorKeyAction.MoveLeft -> List(trigger("ctrl+j"))))
    val state   = stateWithConfig(config, uiSurfaces = List(commandPaletteSurface("irrelevant-item")))
    val fixture = harness(state)

    fixture.keybindings
      .interpret(KeybindingsIntent.SetEditorKeyBinding(EditorKeyAction.MoveRight, "ctrl+j"))
      .unsafeRunSync()

    val after = fixture.stateRef.get.unsafeRunSync()
    after.persisted.config.inputConfig.focusedKeymapConfig.editor shouldBe config.inputConfig.focusedKeymapConfig.editor
    val page = drilledPage(after)
    page.itemId shouldBe "keymap-editor-move_right"
    page.draftText shouldBe "ctrl+j"
    page.recording.flatMap(_.pendingFocusedKeymapConflict) shouldBe Some("keymap-editor-move_right" -> "ctrl+j")

    after.commandRunnerSurface.map(_.content) match
      case Some(SurfaceContent.CommandPalette(runner)) =>
        runner.statusMessage shouldBe Some(ConflictMessage)
      case other => fail(s"Expected a CommandPalette surface, got $other")
  }

  it should "force-reassign a focused-keymap binding on conflict resolution, unbinding whichever action held it" in {
    val config  = configWithEditorBindings(Map(EditorKeyAction.MoveLeft -> List(trigger("ctrl+j"))))
    val fixture = harness(stateWithConfig(config))

    fixture.keybindings
      .interpret(KeybindingsIntent.ResolveFocusedKeymapConflict("keymap-editor-move_right", "ctrl+j"))
      .unsafeRunSync()

    val editor = fixture.currentConfig.inputConfig.focusedKeymapConfig.editor
    editor.bindingsFor(EditorKeyAction.MoveRight) shouldBe List(trigger("ctrl+j"))
    editor.bindingsFor(EditorKeyAction.MoveLeft) shouldBe Nil
  }

  it should "restore an overridden global hotkey to its platform default" in {
    val config  = configWithHotkeys(Map(HotkeyAction.Quit -> List(trigger("ctrl+shift+z"))))
    val fixture = harness(stateWithConfig(config))

    fixture.keybindings.interpret(KeybindingsIntent.ResetGlobalHotkey(HotkeyAction.Quit)).unsafeRunSync()

    fixture.currentConfig.inputConfig.hotkeyConfig.bindingsFor(HotkeyAction.Quit) shouldBe
      HotkeyConfig.defaultBindings.getOrElse(HotkeyAction.Quit, Nil)
  }

  it should "restore an overridden panel keymap binding to its default" in {
    val config  = configWithPanelBindings(Map(PanelKeyAction.GrowPanel -> List(trigger("ctrl+g"))))
    val fixture = harness(stateWithConfig(config))

    fixture.keybindings.interpret(KeybindingsIntent.ResetPanelKeyBinding(PanelKeyAction.GrowPanel)).unsafeRunSync()

    fixture.currentConfig.inputConfig.focusedKeymapConfig.panel.bindingsFor(PanelKeyAction.GrowPanel) shouldBe
      PanelKeyAction.defaultBindings.getOrElse(PanelKeyAction.GrowPanel, Nil)
  }

  it should "dispatch a panel keymap binding through the panel group rather than the editor group" in {
    val fixture = harness(stateWithConfig(AppConfig()))

    fixture.keybindings
      .interpret(KeybindingsIntent.SetPanelKeyBinding(PanelKeyAction.ShrinkPanel, "ctrl+j"))
      .unsafeRunSync()

    val focused = fixture.currentConfig.inputConfig.focusedKeymapConfig
    focused.panel.bindingsFor(PanelKeyAction.ShrinkPanel) shouldBe List(trigger("ctrl+j"))
    focused.editor shouldBe FocusedKeymapConfig().editor
  }
