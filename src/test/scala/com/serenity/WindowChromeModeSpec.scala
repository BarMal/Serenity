package com.serenity

import com.serenity.config.{AppConfig, WindowChromeMode}
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
    config.blurRadius shouldBe 0.55f
    config.backgroundStyle shouldBe AppConfig.default.backgroundStyle
    config.cursorMode shouldBe AppConfig.default.cursorMode
  }
