package com.serenity

import scala.concurrent.duration.*

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.config.{CommandRunnerKeyAction, HotkeyAction, HotkeyConfig, KeymapGroup}
import com.serenity.keystroke.events.*
import com.serenity.keystroke.{InputKey, KeyStrokeInfo}
import com.serenity.state.models.SurfaceContent
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class KeymapEditorStateManagerSpec extends AnyFlatSpec with Matchers with StateManagerTestSupport:

  private def createLinuxStateManager(name: String) =
    val stateManager = createStateManager(name)
    stateManager
      .updateState(state =>
        state.copy(persisted =
          state.persisted.copy(config = state.persisted.config.withHotkeyConfig(HotkeyConfig.forOs("Linux")))
        )
      )
      .unsafeRunSync()
    stateManager

  "Keymap editor settings" should "update global hotkey bindings through the command runner" in {
    val stateManager = createLinuxStateManager("KeymapEditorStateManagerSpec")

    stateManager.applyEvent(ToggleCommandRunner).unsafeRunSync()
    "keymap".foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())
    stateManager.applyEvent(Enter).unsafeRunSync()
    "ctrl+k".foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())
    stateManager.applyEvent(Enter).unsafeRunSync()

    val config = stateManager.getCurrentState.unsafeRunSync().persisted.config
    config.inputConfig.hotkeyConfig.bindingsFor(HotkeyAction.ToggleCommandRunner).head.render shouldBe "ctrl+k"
  }

  it should "reset global hotkey bindings to defaults through the command runner" in {
    val stateManager = createLinuxStateManager("KeymapEditorResetSpec")

    stateManager
      .updateState(state =>
        state.copy(persisted =
          state.persisted
            .copy(config = state.persisted.config.withHotkeyOverride(HotkeyAction.ToggleCommandRunner, "ctrl+k"))
        )
      )
      .unsafeRunSync()
    stateManager.applyEvent(ToggleCommandRunner).unsafeRunSync()
    "keymap".foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())
    stateManager.applyEvent(Enter).unsafeRunSync()
    "default".foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())
    stateManager.applyEvent(Enter).unsafeRunSync()

    val config         = stateManager.getCurrentState.unsafeRunSync().persisted.config
    val defaultBinding = HotkeyConfig.defaultBindings(HotkeyAction.ToggleCommandRunner).head.render
    config.inputConfig.hotkeyConfig.bindingsFor(HotkeyAction.ToggleCommandRunner).head.render shouldBe defaultBinding
  }

  it should "finalize a single-key recording without follow-up input" in {
    val stateManager = createLinuxStateManager("KeymapEditorSingleKeyExpirySpec")

    stateManager.applyEvent(ToggleCommandRunner).unsafeRunSync()
    "keymap".foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())
    stateManager.applyEvent(Enter).unsafeRunSync()
    stateManager.applyEvent(Enter).unsafeRunSync()
    stateManager
      .applyEvent(RunnerRecordBinding(KeyStrokeInfo(InputKey.Character, Some('k'), Set.empty), 1_000L))
      .unsafeRunSync()

    stateManager.getCurrentState
      .unsafeRunSync()
      .commandRunnerSurface
      .flatMap(_.content match
        case SurfaceContent.CommandPalette(runner) => runner.activeSubmenu.flatMap(_.pendingRecordedBinding)
        case SurfaceContent.CommandPaletteSubmenu(runner, _, _) =>
          runner.activeSubmenu.flatMap(_.pendingRecordedBinding)
        case _ => None) should not be empty

    IO.sleep(500.millis).unsafeRunSync()

    stateManager.getCurrentState
      .unsafeRunSync()
      .persisted
      .config
      .inputConfig
      .hotkeyConfig
      .bindingsFor(HotkeyAction.ToggleCommandRunner)
      .map(_.render) shouldBe List("k")
  }

  it should "keep the keymap editor open when a binding conflicts" in {
    val stateManager = createLinuxStateManager("KeymapEditorConflictSpec")

    stateManager
      .updateState(state =>
        state.copy(persisted =
          state.persisted
            .copy(config = state.persisted.config.withHotkeyOverride(HotkeyAction.ToggleCommandRunner, "ctrl+k"))
        )
      )
      .unsafeRunSync()
    stateManager.applyEvent(ToggleCommandRunner).unsafeRunSync()
    "keymap".foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())
    stateManager.applyEvent(Enter).unsafeRunSync()
    "ctrl+o".foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())
    stateManager.applyEvent(Enter).unsafeRunSync()

    val state = stateManager.getCurrentState.unsafeRunSync()
    state.persisted.config.inputConfig.hotkeyConfig
      .bindingsFor(HotkeyAction.ToggleCommandRunner)
      .head
      .render shouldBe "ctrl+k"
    state.commandRunnerSurface.flatMap(_.content match
      case SurfaceContent.CommandPalette(runner) => runner.statusMessage
      case _                                     => None) shouldBe Some(
      "Binding is already assigned. Enter to unbind the other action, or Escape to preserve it."
    )
  }

  it should "offer conflict resolution by unbinding the previous owner on enter" in {
    val stateManager = createLinuxStateManager("KeymapEditorConflictResolveSpec")

    stateManager.applyEvent(ToggleCommandRunner).unsafeRunSync()
    "keymap".foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())
    stateManager.applyEvent(Enter).unsafeRunSync()
    "ctrl+o".foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())
    stateManager.applyEvent(Enter).unsafeRunSync()
    stateManager.applyEvent(Enter).unsafeRunSync()

    val config = stateManager.getCurrentState.unsafeRunSync().persisted.config
    config.inputConfig.hotkeyConfig.bindingsFor(HotkeyAction.ToggleCommandRunner).head.render shouldBe "ctrl+o"
    config.inputConfig.hotkeyConfig.bindingsFor(HotkeyAction.OpenFile) shouldBe Nil
  }

  it should "leave an idempotent focused keymap assignment unchanged" in {
    val stateManager = createLinuxStateManager("FocusedKeymapEditorIdempotentSpec")

    stateManager
      .updateState(state =>
        state.copy(
          persisted = state.persisted.copy(
            config = state.persisted.config.withKeymapBinding(KeymapGroup.CommandRunner)(
              CommandRunnerKeyAction.Submit,
              "ctrl+k"
            )
          )
        )
      )
      .unsafeRunSync()
    stateManager.applyEvent(ToggleCommandRunner).unsafeRunSync()
    "keymap".foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())
    stateManager.applyEvent(Enter).unsafeRunSync()
    List.fill(3)(MoveDown).foreach(event => stateManager.applyEvent(event).unsafeRunSync())
    "ctrl+k".foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())
    stateManager.applyEvent(Enter).unsafeRunSync()

    val state = stateManager.getCurrentState.unsafeRunSync()
    state.persisted.config.inputConfig.focusedKeymapConfig.commandRunner
      .bindingsFor(CommandRunnerKeyAction.Submit)
      .map(_.render) shouldBe List("ctrl+k")
    state.commandRunnerSurface.flatMap(_.content match
      case SurfaceContent.CommandPalette(runner) => runner.statusMessage
      case _                                     => None) shouldBe None
  }

  it should "offer focused keymap conflict resolution by unbinding the previous owner on enter" in {
    val stateManager = createLinuxStateManager("FocusedKeymapEditorConflictResolveSpec")

    stateManager
      .updateState(state =>
        state.copy(
          persisted = state.persisted.copy(
            config = state.persisted.config
              .withKeymapBinding(KeymapGroup.CommandRunner)(CommandRunnerKeyAction.NavigateDown, "ctrl+k")
          )
        )
      )
      .unsafeRunSync()
    stateManager.applyEvent(ToggleCommandRunner).unsafeRunSync()
    "keymap".foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())
    stateManager.applyEvent(Enter).unsafeRunSync()
    List.fill(3)(MoveDown).foreach(event => stateManager.applyEvent(event).unsafeRunSync())
    stateManager.getCurrentState
      .unsafeRunSync()
      .commandRunnerSurface
      .flatMap(_.content match
        case SurfaceContent.CommandPalette(runner) =>
          runner.activeSubmenu
            .flatMap(submenu => runner.submenuItems(submenu.groupId).lift(submenu.selectedIndex))
            .map(_.id)
        case _ => None) shouldBe Some("keymap-command-runner-submit")
    "ctrl+k".foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())
    stateManager.applyEvent(Enter).unsafeRunSync()

    val conflicted = stateManager.getCurrentState.unsafeRunSync()
    conflicted.persisted.config.inputConfig.focusedKeymapConfig.commandRunner
      .bindingsFor(CommandRunnerKeyAction.NavigateDown)
      .map(_.render) shouldBe List("ctrl+k")
    conflicted.persisted.config.inputConfig.focusedKeymapConfig.commandRunner
      .bindingsFor(CommandRunnerKeyAction.Submit)
      .map(_.render) shouldBe List("enter")
    conflicted.commandRunnerSurface.flatMap(_.content match
      case SurfaceContent.CommandPalette(runner) => runner.statusMessage
      case _                                     => None) shouldBe Some(
      "Binding is already assigned. Enter to unbind the other action, or Escape to preserve it."
    )

    stateManager.applyEvent(Enter).unsafeRunSync()

    val resolved =
      stateManager.getCurrentState.unsafeRunSync().persisted.config.inputConfig.focusedKeymapConfig.commandRunner
    resolved.bindingsFor(CommandRunnerKeyAction.Submit).map(_.render) shouldBe List("ctrl+k")
    resolved.bindingsFor(CommandRunnerKeyAction.NavigateDown) shouldBe Nil
  }

end KeymapEditorStateManagerSpec
