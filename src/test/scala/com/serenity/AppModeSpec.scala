package com.serenity

import com.serenity.config.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** `AppMode` distinguishes a code workspace from a prose one. It gates code-only tooling (LSP connections, project
  * build/run/test/debug -- issues #1294 and #1296) and filters which settings are shown by default (issue #1297).
  */
class AppModeSpec extends AnyFlatSpec with Matchers:

  "AppMode.fromConfigKey" should "parse the known spellings" in {
    AppMode.fromConfigKey("code") shouldBe Some(AppMode.Code)
    AppMode.fromConfigKey("prose") shouldBe Some(AppMode.Prose)
    AppMode.fromConfigKey("PROSE") shouldBe Some(AppMode.Prose)
    AppMode.fromConfigKey("nonsense") shouldBe None
  }

  "AppConfig.default" should "start in code mode with settings filtering on" in {
    AppConfig.default.appMode shouldBe AppMode.Code
    AppConfig.default.showAllSettingsRegardlessOfMode shouldBe false
  }

  "AppConfig.withAppMode" should "switch the mode without disturbing the settings-visibility flag" in {
    val prose = AppConfig.default.withShowAllSettingsRegardlessOfMode(true).withAppMode(AppMode.Prose)

    prose.appMode shouldBe AppMode.Prose
    prose.showAllSettingsRegardlessOfMode shouldBe true
  }

  "ConfigRegistry" should "read and validate the app mode settings" in {
    ConfigRegistry
      .read(AppConfig.default, "app.mode", "prose")
      .getOrElse(fail("app.mode parse"))
      .appMode shouldBe AppMode.Prose

    ConfigRegistry
      .read(AppConfig.default, "app.show_all_settings", "true")
      .getOrElse(fail("app.show_all_settings parse"))
      .showAllSettingsRegardlessOfMode shouldBe true

    ConfigRegistry.rejects("app.mode", "prose") shouldBe false
    ConfigRegistry.rejects("app.mode", "unknown") shouldBe true
  }

  "ConfigKeySchema" should "know the app mode keys" in {
    ConfigKeySchema.isKnownKey("app.mode") shouldBe true
    ConfigKeySchema.isKnownKey("app.show_all_settings") shouldBe true
  }
