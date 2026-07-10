package com.serenity

import com.serenity.config.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class WindowChromeModeSpec extends AnyFlatSpec with Matchers:

  "AppConfig" should "default window chrome mode to Native" in {
    AppConfig.default.windowChromeMode shouldBe WindowChromeMode.Native
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

  it should "store preferred window size inside the window sub-config" in {
    val config = AppConfig.default.withPreferredWindowSize(PreferredWindowSize(320, 240))

    config.preferredWindowSize shouldBe Some(PreferredWindowSize(400, 300))
    config.windowConfig shouldBe WindowConfig(preferredSize = Some(PreferredWindowSize(400, 300)))
  }
