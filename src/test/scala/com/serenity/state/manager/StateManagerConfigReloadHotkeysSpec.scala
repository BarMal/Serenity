package com.serenity.state.manager

import java.nio.file.Path

import cats.effect.IO
import com.serenity.config.AppConfigOps.*
import com.serenity.config.{
  AppConfig,
  ConfigError,
  ConfigFileFormat,
  ConfigLoadResult,
  ConfigManagerTestSupport,
  ConfigMigrations,
  HotkeyAction,
  HotkeyConfig,
  HotkeyTrigger
}
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A terminal session keeps its hotkey adjustment (#1213) when the config file is reloaded after an outside edit
  * (#2156).
  */
class StateManagerConfigReloadHotkeysSpec extends AnyFlatSpec with Matchers with ConfigEffectsHarness:

  private val macHotkeys   = HotkeyConfig.forOs("Mac OS X")
  private val linuxHotkeys = HotkeyConfig.forOs("linux")

  private def loadingAs(plan: ConfigMigrations.Plan): Path => IO[Either[ConfigError, ConfigLoadResult]] =
    path => IO.blocking(Right(ConfigManagerTestSupport.loadConfigResult(Some(path.toString), plan)))

  private def editAs(fixture: Harness, osName: String, config: AppConfig): Unit =
    editExternally(fixture, ConfigFileFormat.render(config, osName))

  private def terminalFixture(hotkeys: HotkeyConfig, plan: ConfigMigrations.Plan): Harness =
    val terminalConfig = AppConfig.default.withHotkeyConfig(hotkeys.forTerminalUse)
    harness(
      initialState = AppState.initial(terminalConfig),
      configOnDisk = Some(terminalConfig),
      loadConfig = loadingAs(plan)
    )

  private def reloadedHotkeys(fixture: Harness): HotkeyConfig =
    reload(fixture)
    fixture.currentConfig.inputConfig.hotkeyConfig

  "Reloading the config file in a terminal" should "keep a macOS terminal's hotkey adjustment when an unrelated setting is edited outside" in {
    val fixture  = terminalFixture(macHotkeys, ConfigManagerTestSupport.macPlan)
    val adjusted = macHotkeys.forTerminalUse
    editAs(fixture, "Mac OS X", AppConfig.default.withHotkeyConfig(macHotkeys).withWheelScrollLines(7))

    val hotkeys = reloadedHotkeys(fixture)

    fixture.currentConfig.inputConfig.wheelScrollLines shouldBe 7
    hotkeys.terminalAdjusted shouldBe true
    hotkeys.bindingsFor(HotkeyAction.Save).map(_.render) shouldBe List("ctrl+s")
    hotkeys.bindings shouldBe adjusted.bindings
    hotkeys.commandBindings shouldBe adjusted.commandBindings
  }

  it should "keep a hotkey the file overrides when a macOS terminal reloads it" in {
    val fixture = terminalFixture(macHotkeys, ConfigManagerTestSupport.macPlan)
    val rebound = HotkeyTrigger.parse("ctrl+alt+r").toList
    val edited  = macHotkeys.copy(bindings = macHotkeys.bindings.updated(HotkeyAction.Redo, rebound))
    editAs(fixture, "Mac OS X", AppConfig.default.withHotkeyConfig(edited).withWheelScrollLines(7))

    val hotkeys = reloadedHotkeys(fixture)

    fixture.currentConfig.inputConfig.wheelScrollLines shouldBe 7
    hotkeys.bindingsFor(HotkeyAction.Redo) shouldBe rebound
    hotkeys.bindingsFor(HotkeyAction.Save).map(_.render) shouldBe List("ctrl+s")
    hotkeys.terminalAdjusted shouldBe true
  }

  it should "keep the Cmd hotkeys of a macOS GUI session when the file is reloaded" in {
    val guiConfig = AppConfig.default.withHotkeyConfig(macHotkeys)
    val fixture = harness(
      initialState = AppState.initial(guiConfig),
      configOnDisk = Some(guiConfig),
      loadConfig = loadingAs(ConfigManagerTestSupport.macPlan)
    )
    editAs(fixture, "Mac OS X", guiConfig.withWheelScrollLines(7))

    val hotkeys = reloadedHotkeys(fixture)

    fixture.currentConfig.inputConfig.wheelScrollLines shouldBe 7
    hotkeys.terminalAdjusted shouldBe false
    hotkeys.bindings shouldBe macHotkeys.bindings
  }

  it should "leave a non-macOS terminal's hotkeys at the platform defaults when the file is reloaded" in {
    val fixture = terminalFixture(linuxHotkeys, ConfigManagerTestSupport.linuxPlan)
    editAs(fixture, "linux", AppConfig.default.withHotkeyConfig(linuxHotkeys).withWheelScrollLines(7))

    val hotkeys = reloadedHotkeys(fixture)

    fixture.currentConfig.inputConfig.wheelScrollLines shouldBe 7
    hotkeys.terminalAdjusted shouldBe true
    hotkeys.bindings shouldBe linuxHotkeys.bindings
  }
