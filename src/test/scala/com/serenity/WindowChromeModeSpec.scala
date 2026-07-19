package com.serenity

import com.serenity.config.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class WindowChromeModeSpec extends AnyFlatSpec with Matchers:

  "WindowConfig" should "own window chrome and preferred-size schema metadata" in {
    WindowConfig.Schema.currentKeys.should(
      contain allOf (
        "window.chrome",
        "window.chrome.mode",
        "window.preferred.width",
        "window.preferred.height"
      )
    )

    WindowConfig.Schema.deprecatedKeys.should(
      contain allOf (
        "window_chrome"           -> "window.chrome",
        "window_chrome_mode"      -> "window.chrome",
        "window_preferred_width"  -> "window.preferred.width",
        "window_preferred_height" -> "window.preferred.height"
      )
    )
  }

  it should "parse window chrome config values centrally" in {
    WindowChromeMode.fromConfigKey("auto").shouldBe(Some(WindowChromeMode.Auto))
    WindowChromeMode.fromConfigKey("serenity").shouldBe(Some(WindowChromeMode.Custom))
    WindowChromeMode.fromConfigKey("native-themed").shouldBe(Some(WindowChromeMode.NativeThemed))
    WindowChromeMode.fromConfigKey("os").shouldBe(Some(WindowChromeMode.Native))
    WindowChromeMode.fromConfigKey("unknown").shouldBe(None)
  }

  it should "parse window config entries centrally" in {
    val chromeConfig =
      WindowConfig.Schema.parse(AppConfig.default, "window_chrome_mode", "serenity").getOrElse(fail("chrome parse"))
    val widthConfig =
      WindowConfig.Schema
        .parse(AppConfig.default, "window.preferred.width", "320")
        .getOrElse(fail("width parse"))
    val heightConfig =
      WindowConfig.Schema
        .parse(AppConfig.default, "window_preferred_height", "200")
        .getOrElse(fail("height parse"))

    chromeConfig.windowConfig.shouldBe(WindowConfig(chromeMode = WindowChromeMode.Custom))
    widthConfig.preferredWindowSize.shouldBe(Some(PreferredWindowSize(400, 768)))
    heightConfig.preferredWindowSize.shouldBe(Some(PreferredWindowSize(1024, 300)))
    WindowConfig.Schema.parse(AppConfig.default, "window.chrome", "unknown").shouldBe(None)
  }

  it should "validate window config entries centrally" in {
    WindowConfig.Schema.invalidValue("window.chrome", "native").shouldBe(false)
    WindowConfig.Schema.invalidValue("window.chrome", "unknown").shouldBe(true)
    WindowConfig.Schema.invalidValue("window.preferred.width", "wide").shouldBe(true)
    WindowConfig.Schema.invalidValue("window.preferred.height", "").shouldBe(false)
  }

  "AppConfig" should "default window chrome mode to Auto" in {
    AppConfig.default.windowChromeMode shouldBe WindowChromeMode.Auto
  }

  it should "change window chrome mode without disturbing other config fields" in {
    val config = AppConfig.default
      .withBlurRadius(0.55f)
      .withWindowChromeMode(WindowChromeMode.Custom)

    config.windowChromeMode shouldBe WindowChromeMode.Custom
    config.windowConfig shouldBe WindowConfig(chromeMode = WindowChromeMode.Custom)
    config.blurRadius shouldBe 0.55f
    config.backgroundStyle shouldBe AppConfig.default.backgroundStyle
    config.cursorMode shouldBe AppConfig.default.cursorMode
  }

  it should "retain native-themed chrome in window configuration" in {
    AppConfig.default.withWindowChromeMode(WindowChromeMode.NativeThemed).windowChromeMode shouldBe
      WindowChromeMode.NativeThemed
  }

  it should "store preferred window size inside the window sub-config" in {
    val config = AppConfig.default.withPreferredWindowSize(PreferredWindowSize(320, 240))

    config.preferredWindowSize shouldBe Some(PreferredWindowSize(400, 300))
    config.windowConfig shouldBe WindowConfig(preferredSize = Some(PreferredWindowSize(400, 300)))
  }
