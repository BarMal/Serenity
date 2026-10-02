package com.serenity.command

import com.serenity.config.*
import com.serenity.keystroke.events.GoToFile
import com.serenity.keystroke.translators.TextHotkeyConverters
import com.serenity.keystroke.{InputKey, KeyStrokeInfo, Modifier}
import com.serenity.state.models.AppState
import com.serenity.state.reducers.{AppEffect, AppEventReducer}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GoToFileCommandSpec extends AnyFlatSpec with Matchers:

  given com.serenity.rope.Balance = com.serenity.rope.Balance.default

  private val ctrlE = HotkeyTrigger(InputKey.Character, Some('e'), Set(Modifier.Ctrl))

  "Go to File" should "be a core File command in the palette" in {
    val command = CommandRegistry.default.findCommand("go-to-file").getOrElse(fail("go-to-file is not registered"))

    command shouldBe FileFinderCommands.goToFile
    command.label shouldBe "Go to File"
    command.category shouldBe CommandCategory.File
    command.scope shouldBe CommandScope.core
  }

  it should "be bound to the primary modifier with E on every platform, clashing with no other default" in {
    HotkeyConfig.defaultBindingsFor("Linux")(HotkeyAction.GoToFile).map(_.render) shouldBe List("ctrl+e")
    HotkeyConfig.defaultBindingsFor("Mac OS X")(HotkeyAction.GoToFile).map(_.render) shouldBe List("meta+e")
    HotkeyConfig.validate(HotkeyConfig.defaultBindingsFor("Linux")) shouldBe Right(())
    HotkeyConfig.validate(HotkeyConfig.defaultBindingsFor("Mac OS X")) shouldBe Right(())
    HotkeyConfig.forOs("Mac OS X").forTerminalUse.bindingsFor(HotkeyAction.GoToFile) shouldBe List(ctrlE)
  }

  it should "not shadow any focused keymap's default" in {
    val keymaps = FocusedKeymapConfig()
    val focusedTriggers =
      keymaps.editor.bindings.values ++ keymaps.commandRunner.bindings.values ++ keymaps.modal.bindings.values ++
        keymaps.panel.bindings.values ++ keymaps.peek.bindings.values

    focusedTriggers.flatten.toList should not contain ctrlE
  }

  it should "turn its hotkey into the Go to File event, and show the binding in the palette" in {
    val config = AppConfig.default.copy(inputConfig =
      AppConfig.default.inputConfig.copy(hotkeyConfig = HotkeyConfig.forOs("Linux"))
    )

    TextHotkeyConverters
      .hotkeyConverter(config)
      .lift(KeyStrokeInfo(InputKey.Character, Some('e'), Set(Modifier.Ctrl))) shouldBe
      Some(GoToFile)
    CommandRunner.commandBindings(config).get("go-to-file") shouldBe Some("ctrl+e")
  }

  it should "run the command when its event arrives" in {
    AppEventReducer.reduce(GoToFile, AppState.initial, CommandRegistry.default).effects shouldBe
      List(AppEffect.ExecuteCommand(FileFinderCommands.goToFile))
  }
