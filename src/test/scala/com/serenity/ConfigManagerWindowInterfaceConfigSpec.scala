package com.serenity

import java.nio.file.Files

import com.serenity.config.*
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Window size/chrome, interface density, and general UI metrics (element gap, corner radius, outline thickness). */
class ConfigManagerWindowInterfaceConfigSpec extends AnyFlatSpec with Matchers with OptionValues:

  "ConfigManager" should "load and write preferred window size" in {
    val configFile = Files.createTempFile("serenity-window-size-config", ".conf")
    Files.writeString(
      configFile,
      """window.preferred.width = 1400
        |window.preferred.height = 900
        |""".stripMargin
    )

    val config = ConfigManagerTestSupport.loadConfig(Some(configFile.toString))

    config.preferredWindowSize shouldBe Some(PreferredWindowSize(1400, 900))
    ConfigManager.configToString(config) should include("window.preferred.width = 1400")
    ConfigManager.configToString(config) should include("window.preferred.height = 900")
  }

  it should "load and write window chrome mode" in {
    val configFile = Files.createTempFile("serenity-window-chrome-config", ".conf")
    Files.writeString(
      configFile,
      """window.chrome = auto
        |""".stripMargin
    )

    val config = ConfigManagerTestSupport.loadConfig(Some(configFile.toString))

    config.windowChromeMode shouldBe WindowChromeMode.Auto
    ConfigManager.configToString(config) should include(
      "# Window chrome: auto uses themed chrome on Linux; native preserves OS snap/window animations; native-themed uses Windows system chrome colours; custom is themed and applies after restart"
    )
    ConfigManager.configToString(config) should include("window.chrome = auto")
  }

  it should "report invalid window config values through the window schema" in {
    val configFile = Files.createTempFile("serenity-window-invalid-config", ".conf")
    Files.writeString(
      configFile,
      """window.chrome = themed-ish
        |window.preferred.width = very-wide
        |""".stripMargin
    )

    val result = ConfigManagerTestSupport.loadConfigResult(Some(configFile.toString))

    result.report.invalidEntries.map(_.key) should contain("window.chrome")
    result.report.invalidEntries.map(_.key) should contain("window.preferred.width")
  }

  it should "load and write interface density mode" in {
    val configFile = Files.createTempFile("serenity-density-config", ".conf")
    Files.writeString(
      configFile,
      """interface.density = spacious
        |""".stripMargin
    )

    val config = ConfigManagerTestSupport.loadConfig(Some(configFile.toString))

    config.interfaceDensity shouldBe InterfaceDensity.Spacious
    ConfigManager.configToString(config) should include("interface.density = spacious")
  }

  it should "report invalid interface config values through the interface schema" in {
    val configFile = Files.createTempFile("serenity-interface-invalid-config", ".conf")
    Files.writeString(
      configFile,
      """interface.density = roomy
        |ui.element_gap = wide
        |ui.outline_thickness =
        |""".stripMargin
    )

    val result = ConfigManagerTestSupport.loadConfigResult(Some(configFile.toString))

    result.report.invalidEntries.map(_.key) should contain("interface.density")
    result.report.invalidEntries.map(_.key) should contain("ui.element_gap")
    result.report.invalidEntries.map(_.key) should contain("ui.outline_thickness")
  }

  it should "load and write UI element gaps" in {
    val configFile = Files.createTempFile("serenity-ui-gap-config", ".conf")
    Files.writeString(
      configFile,
      """ui.element_gap = 3
        |""".stripMargin
    )

    val config = ConfigManagerTestSupport.loadConfig(Some(configFile.toString))

    config.uiElementGap shouldBe 3
    ConfigManager.configToString(config) should include("ui.element_gap = 3")
  }

  it should "load and write UI corner radius" in {
    val configFile = Files.createTempFile("serenity-ui-corner-radius-config", ".conf")
    Files.writeString(
      configFile,
      """ui.corner_radius = 14
        |""".stripMargin
    )

    val config = ConfigManagerTestSupport.loadConfig(Some(configFile.toString))

    config.uiCornerRadiusPx shouldBe 14
    ConfigManager.configToString(config) should include("ui.corner_radius = 14")
  }

  it should "load and write UI outline thickness" in {
    val configFile = Files.createTempFile("serenity-ui-outline-thickness-config", ".conf")
    Files.writeString(
      configFile,
      """ui.outline_thickness = 4
        |""".stripMargin
    )

    val config = ConfigManagerTestSupport.loadConfig(Some(configFile.toString))

    config.uiOutlineThicknessPx shouldBe 4
    ConfigManager.configToString(config) should include("ui.outline_thickness = 4")
  }

  it should "store interface density and chrome metrics inside the interface sub-config" in {
    val config = AppConfig.default
      .withInterfaceDensity(InterfaceDensity.Spacious)
      .withUiElementGap(3)
      .withUiCornerRadiusPx(14)
      .withUiOutlineThicknessPx(4)

    config.interfaceConfig shouldBe InterfaceConfig(
      density = InterfaceDensity.Spacious,
      elementGap = 3,
      cornerRadiusPx = 14,
      outlineThicknessPx = 4
    )
  }
