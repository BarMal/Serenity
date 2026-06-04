package com.serenity

import java.nio.file.Files

import com.serenity.config.{ConfigManager, HotkeyAction}
import com.serenity.keystroke.{InputKey, Modifier}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ConfigManagerSpec extends AnyFlatSpec with Matchers:

  "ConfigManager" should "load configured hotkey overrides from a config file" in {
    val configFile = Files.createTempFile("serenity-config", ".conf")
    Files.writeString(
      configFile,
      """hotkey.command_palette = ctrl+k
        |hotkey.file_search = ctrl+alt+f
        |hotkey.previous_tab = ctrl+shift+tab
        |""".stripMargin
    )

    val config = ConfigManager.loadConfig(Some(configFile.toString))

    config.hotkeyConfig
      .bindingsFor(HotkeyAction.ToggleCommandRunner)
      .head shouldBe com.serenity.config.HotkeyTrigger(
      keyType = InputKey.Character,
      character = Some('k'),
      modifiers = Set(Modifier.Ctrl)
    )
    config.hotkeyConfig
      .bindingsFor(HotkeyAction.FileSearch)
      .head shouldBe com.serenity.config.HotkeyTrigger(
      keyType = InputKey.Character,
      character = Some('f'),
      modifiers = Set(Modifier.Ctrl, Modifier.Alt)
    )
    config.hotkeyConfig
      .bindingsFor(HotkeyAction.PreviousTab)
      .head shouldBe com.serenity.config.HotkeyTrigger(
      keyType = InputKey.Tab,
      character = None,
      modifiers = Set(Modifier.Ctrl, Modifier.Shift)
    )
  }
