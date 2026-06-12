package com.serenity

import cats.effect.unsafe.implicits.global
import com.serenity.config.{HotkeyAction, HotkeyConfig}
import com.serenity.keystroke.events.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class KeymapEditorStateManagerSpec extends AnyFlatSpec with Matchers with StateManagerTestSupport:

  "Keymap editor settings" should "update global hotkey bindings through the command runner" in {
    val stateManager = createStateManager("KeymapEditorStateManagerSpec")

    stateManager.applyEvent(ToggleCommandRunner).unsafeRunSync()
    "keymap".foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())
    stateManager.applyEvent(Enter).unsafeRunSync()
    "ctrl+k".foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())
    stateManager.applyEvent(Enter).unsafeRunSync()

    val config = stateManager.getCurrentState.unsafeRunSync().config
    config.hotkeyConfig.bindingsFor(HotkeyAction.ToggleCommandRunner).head.render shouldBe "ctrl+k"
  }

  it should "reset global hotkey bindings to defaults through the command runner" in {
    val stateManager = createStateManager("KeymapEditorResetSpec")

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

end KeymapEditorStateManagerSpec
