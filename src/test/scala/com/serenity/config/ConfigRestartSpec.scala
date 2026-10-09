package com.serenity.config

import com.serenity.config.AppConfigOps.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ConfigRestartSpec extends AnyFlatSpec with Matchers:

  private val base = AppConfig.default

  "ConfigRestart.changed" should "name a startup-only setting that differs" in {
    val chrome = WindowChromeMode.values.find(_ != base.windowChromeMode).get

    ConfigRestart.changed(base, base.withWindowChromeMode(chrome)) shouldBe List("window.chrome")
    ConfigRestart.changed(base, base.withPreferredWindowSize(PreferredWindowSize(1280, 800))) shouldBe
      List("window.preferred.width/height")
    ConfigRestart.changed(base, base.withStartupWarmUp(!base.surfaceConfig.startupWarmUpEnabled)) shouldBe
      List("startup.warm_up")
  }

  it should "name nothing for settings that apply live" in {
    ConfigRestart.changed(base, base.withWheelScrollLines(7).withLineNumbers(false)) shouldBe Nil
  }

  "ConfigRestart.notice" should "say that a restart is needed only when something is listed" in {
    ConfigRestart.notice(Nil) shouldBe None
    ConfigRestart
      .notice(List("window.chrome", "startup.warm_up"))
      .exists(_.contains("window.chrome, startup.warm_up")) shouldBe
      true
  }
