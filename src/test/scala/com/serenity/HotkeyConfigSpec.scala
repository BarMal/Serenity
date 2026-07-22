package com.serenity

import java.nio.file.Files

import com.serenity.config.{AppConfig, ConfigManager, HotkeyAction, HotkeyConfig}
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
      "meta+h",
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

  it should "preserve a valid keymap when resetting an override would conflict" in {
    val overridden = HotkeyConfig
      .forOs("Linux")
      .withBinding(HotkeyAction.Find, "ctrl+alt+f")
      .withBinding(HotkeyAction.Replace, "ctrl+f")

    val reset = overridden.resetBinding(HotkeyAction.Find)

    reset shouldBe overridden
    HotkeyConfig.validate(reset.bindings) shouldBe Right(())
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
