package com.serenity

import java.nio.file.Files

import com.serenity.config.*
import com.serenity.keystroke.{InputKey, KeyStrokeInfo}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class HotkeyConfigSpec extends AnyFlatSpec with Matchers:

  private val editingActions = List(
    HotkeyAction.Find,
    HotkeyAction.Replace,
    HotkeyAction.GoToLine,
    HotkeyAction.SaveAs
  )

  "HotkeyConfig" should "supply conventional Ctrl defaults for core editing workflows on Windows and Linux" in {
    val bindings = HotkeyConfig.defaultBindingsFor("Linux")

    editingActions.map(action => bindings(action).head.render) shouldBe List(
      "ctrl+f",
      "ctrl+h",
      "ctrl+g",
      "ctrl+shift+s"
    )
  }

  it should "supply Command defaults for core editing workflows on macOS" in {
    val bindings = HotkeyConfig.defaultBindingsFor("Mac OS X")

    editingActions.map(action => bindings(action).head.render) shouldBe List(
      "meta+f",
      "alt+meta+f",
      "meta+g",
      "meta+shift+s"
    )
  }

  it should "preserve a user override over its platform default" in {
    val config = HotkeyConfig.forOs("Mac OS X").withBinding(HotkeyAction.Find, "ctrl+alt+f")

    config.bindingsFor(HotkeyAction.Find).map(_.render) shouldBe List("ctrl+alt+f")
  }

  it should "report conflicting bindings during validation" in {
    val bindings = Map(
      HotkeyAction.Find    -> HotkeyConfig.defaultBindingsFor("Linux")(HotkeyAction.Find),
      HotkeyAction.Replace -> HotkeyConfig.defaultBindingsFor("Linux")(HotkeyAction.Find)
    )

    HotkeyConfig.validate(bindings).isLeft shouldBe true
  }

  it should "unbind a conflicting trigger before assigning it to a new action" in {
    val config     = HotkeyConfig.forOs("Linux")
    val reassigned = config.withBindingUnbindingConflicts(HotkeyAction.Find, "ctrl+o")

    reassigned.bindingsFor(HotkeyAction.OpenFile).map(_.render) shouldBe Nil
    reassigned.bindingsFor(HotkeyAction.Find).map(_.render) shouldBe List("ctrl+o")
    HotkeyConfig.validate(reassigned.bindings) shouldBe Right(())
  }

  it should "unbind a conflicting focused keymap trigger before assigning it" in {
    val config =
      FocusedKeymapConfig().withBinding(KeymapGroup.CommandRunner)(CommandRunnerKeyAction.NavigateDown, "ctrl+enter")

    val reassigned = config.withBindingUnbindingConflicts(KeymapGroup.CommandRunner)(
      CommandRunnerKeyAction.Submit,
      "ctrl+enter"
    )

    reassigned.commandRunner.bindingsFor(CommandRunnerKeyAction.NavigateDown) shouldBe Nil
    reassigned.commandRunner.bindingsFor(CommandRunnerKeyAction.Submit).map(_.render) shouldBe List("ctrl+enter")
  }

  it should "preserve a valid keymap when resetting an override would conflict" in {
    val findDefault = HotkeyConfig.defaultBindings(HotkeyAction.Find).head
    val overridden = HotkeyConfig()
      .withBinding(HotkeyAction.Find, "ctrl+alt+f")
      .withBinding(HotkeyAction.Replace, findDefault)

    val reset = overridden.resetBinding(HotkeyAction.Find)

    reset shouldBe overridden
    HotkeyConfig.validate(reset.bindings) shouldBe Right(())
  }

  it should "parse and match a double modifier tap" in {
    val trigger = HotkeyTrigger.parse("ctrl+ctrl").getOrElse(fail("double modifier trigger"))

    trigger.render shouldBe "ctrl+ctrl"
    trigger.matches(KeyStrokeInfo(InputKey.Ctrl, None, Set.empty)) shouldBe true
    trigger.matches(KeyStrokeInfo(InputKey.Character, Some('c'), Set.empty)) shouldBe false
  }

  it should "include a platform primary modifier double tap for the command runner" in {
    HotkeyConfig.defaultBindingsFor("Linux")(HotkeyAction.ToggleCommandRunner).map(_.render) should contain("ctrl+ctrl")
    HotkeyConfig.defaultBindingsFor("Mac OS X")(HotkeyAction.ToggleCommandRunner).map(_.render) should contain(
      "meta+meta"
    )
  }

  it should "preserve core editing overrides when configuration is saved and reloaded" in {
    val overrides = List(
      HotkeyAction.Find     -> "ctrl+alt+f",
      HotkeyAction.Replace  -> "ctrl+alt+h",
      HotkeyAction.GoToLine -> "ctrl+alt+g",
      HotkeyAction.SaveAs   -> "ctrl+alt+shift+s"
    )
    val config = overrides.foldLeft(AppConfig.default) {
      case (updated, (action, binding)) =>
        updated.withHotkeyOverride(action, binding)
    }
    val configFile = Files.createTempFile("serenity-hotkeys", ".conf")

    ConfigManager.saveConfig(config, configFile) shouldBe true

    val reloaded = ConfigManager.loadConfig(Some(configFile.toString))

    overrides.foreach {
      case (action, binding) =>
        reloaded.hotkeyConfig.bindingsFor(action).headOption.map(_.render) shouldBe Some(binding)
    }
  }

  it should "load a double modifier tap from the text configuration" in {
    val configFile = Files.createTempFile("serenity-double-tap-hotkey", ".conf")
    Files.writeString(configFile, "hotkey.command_palette = ctrl+ctrl\n")

    val config = ConfigManager.loadConfig(Some(configFile.toString))

    config.hotkeyConfig.bindingsFor(HotkeyAction.ToggleCommandRunner).head.render shouldBe "ctrl+ctrl"
  }

  it should "round-trip all command palette bindings through config persistence" in {
    val config     = AppConfig.default.withHotkeyConfig(HotkeyConfig.forOs("Linux"))
    val configFile = Files.createTempFile("serenity-multi-hotkey", ".conf")

    ConfigManager.saveConfig(config, configFile) shouldBe true

    val reloaded = ConfigManager.loadConfig(Some(configFile.toString))

    reloaded.hotkeyConfig.bindingsFor(HotkeyAction.ToggleCommandRunner).map(_.render) shouldBe List(
      "ctrl+p",
      "ctrl+ctrl"
    )
  }

  it should "round-trip multi-bindings for every serialized hotkey action" in {
    val configFile = Files.createTempFile("serenity-multi-hotkey-actions", ".conf")
    Files.writeString(
      configFile,
      """hotkey.file_search = ctrl+shift+f,alt+shift+f
        |hotkey.find = ctrl+alt+f,meta+alt+f
        |hotkey.replace = ctrl+alt+h,meta+alt+h
        |hotkey.go_to_line = ctrl+alt+g,meta+alt+g
        |hotkey.save_as = ctrl+alt+s,meta+alt+s
        |""".stripMargin
    )

    val loaded = ConfigManager.loadConfig(Some(configFile.toString))
    ConfigManager.saveConfig(loaded, configFile) shouldBe true

    val reloaded = ConfigManager.loadConfig(Some(configFile.toString))

    Map(
      HotkeyAction.FileSearch -> List("ctrl+shift+f", "alt+shift+f"),
      HotkeyAction.Find       -> List("ctrl+alt+f", "alt+meta+f"),
      HotkeyAction.Replace    -> List("ctrl+alt+h", "alt+meta+h"),
      HotkeyAction.GoToLine   -> List("ctrl+alt+g", "alt+meta+g"),
      HotkeyAction.SaveAs     -> List("ctrl+alt+s", "alt+meta+s")
    ).foreach {
      case (action, expected) =>
        reloaded.hotkeyConfig.bindingsFor(action).map(_.render) shouldBe expected
    }
  }
