package com.serenity

import cats.effect.unsafe.implicits.global
import com.serenity.config.{HotkeyAction, HotkeyConfig}
import com.serenity.keystroke.events.*
import com.serenity.state.models.SurfaceContent
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class KeymapEditorStateManagerSpec extends AnyFlatSpec with Matchers with StateManagerTestSupport:

  private def createLinuxStateManager(name: String) =
    val stateManager = createStateManager(name)
    stateManager
      .updateState(state => state.copy(config = state.config.withHotkeyConfig(HotkeyConfig.forOs("Linux"))))
      .unsafeRunSync()
    stateManager

  "Keymap editor settings" should "update global hotkey bindings through the command runner" in {
    val stateManager = createLinuxStateManager("KeymapEditorStateManagerSpec")

    stateManager.applyEvent(ToggleCommandRunner).unsafeRunSync()
    "keymap".foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())
    stateManager.applyEvent(Enter).unsafeRunSync()
    "ctrl+k".foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())
    stateManager.applyEvent(Enter).unsafeRunSync()

    val config = stateManager.getCurrentState.unsafeRunSync().config
    config.hotkeyConfig.bindingsFor(HotkeyAction.ToggleCommandRunner).head.render shouldBe "ctrl+k"
  }

  it should "reset global hotkey bindings to defaults through the command runner" in {
    val stateManager = createLinuxStateManager("KeymapEditorResetSpec")

    stateManager
      .updateState(state =>
        state.copy(config = state.config.withHotkeyOverride(HotkeyAction.ToggleCommandRunner, "ctrl+k"))
      )
      .unsafeRunSync()
    stateManager.applyEvent(ToggleCommandRunner).unsafeRunSync()
    "keymap".foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())
    stateManager.applyEvent(Enter).unsafeRunSync()
    "default".foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())
    stateManager.applyEvent(Enter).unsafeRunSync()

    val config         = stateManager.getCurrentState.unsafeRunSync().config
    val defaultBinding = HotkeyConfig.defaultBindings(HotkeyAction.ToggleCommandRunner).head.render
    config.hotkeyConfig.bindingsFor(HotkeyAction.ToggleCommandRunner).head.render shouldBe defaultBinding
  }

  it should "keep the keymap editor open when a binding conflicts" in {
    val stateManager = createLinuxStateManager("KeymapEditorConflictSpec")

    stateManager
      .updateState(state =>
        state.copy(config = state.config.withHotkeyOverride(HotkeyAction.ToggleCommandRunner, "ctrl+k"))
      )
      .unsafeRunSync()
    stateManager.applyEvent(ToggleCommandRunner).unsafeRunSync()
    "keymap".foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())
    stateManager.applyEvent(Enter).unsafeRunSync()
    "ctrl+o".foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())
    stateManager.applyEvent(Enter).unsafeRunSync()

    val state = stateManager.getCurrentState.unsafeRunSync()
    state.config.hotkeyConfig.bindingsFor(HotkeyAction.ToggleCommandRunner).head.render shouldBe "ctrl+k"
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

    val config = stateManager.getCurrentState.unsafeRunSync().config
    config.hotkeyConfig.bindingsFor(HotkeyAction.ToggleCommandRunner).head.render shouldBe "ctrl+o"
    config.hotkeyConfig.bindingsFor(HotkeyAction.OpenFile) shouldBe Nil
  }

end KeymapEditorStateManagerSpec
