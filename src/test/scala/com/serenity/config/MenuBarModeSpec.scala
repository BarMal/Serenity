package com.serenity.config

import java.nio.file.Files

import com.serenity.TestTemp
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MenuBarModeSpec extends AnyFlatSpec with Matchers:

  private def loadResult(text: String) =
    val file = TestTemp.file("serenity-menu-bar-config", ".conf")
    Files.writeString(file, text)
    ConfigManagerTestSupport.loadConfigResult(Some(file.toString))

  "The menu bar setting" should "be hidden unless the user asks otherwise" in {
    AppConfig.default.menuBarMode shouldBe MenuBarMode.Hidden
    ConfigRegistry.defaultFor("ui.menu_bar").map(_.rendered) shouldBe Some("hidden")
  }

  it should "parse its three values and nothing else" in {
    MenuBarMode.fromConfigKey("hidden") shouldBe Some(MenuBarMode.Hidden)
    MenuBarMode.fromConfigKey(" Alt ") shouldBe Some(MenuBarMode.Alt)
    MenuBarMode.fromConfigKey("visible") shouldBe Some(MenuBarMode.Visible)
    MenuBarMode.fromConfigKey("always") shouldBe None
    MenuBarMode.fromConfigKey("") shouldBe None
  }

  it should "be a known key that rejects an unlisted value" in {
    ConfigKeySchema.isKnownKey("ui.menu_bar") shouldBe true
    ConfigRegistry.rejects("ui.menu_bar", "alt") shouldBe false
    ConfigRegistry.rejects("ui.menu_bar", "sometimes") shouldBe true
  }

  it should "load from the config file and be written back" in {
    val result = loadResult("ui.menu_bar = alt\n")

    result.config.menuBarMode shouldBe MenuBarMode.Alt
    result.report.invalidEntries shouldBe Nil
    ConfigManager.configToString(result.config) should include("ui.menu_bar = alt")
  }

  it should "report an invalid value through the invalid-entry report and keep the default" in {
    val result = loadResult("ui.menu_bar = sometimes\n")

    result.report.invalidEntries.map(_.key) should contain("ui.menu_bar")
    result.config.menuBarMode shouldBe MenuBarMode.Hidden
  }

  it should "need a restart to change, because the bar is installed with the window" in {
    ConfigRestart.changed(AppConfig.default, AppConfig.default.withMenuBarMode(MenuBarMode.Visible)) shouldBe
      List("ui.menu_bar")
  }
