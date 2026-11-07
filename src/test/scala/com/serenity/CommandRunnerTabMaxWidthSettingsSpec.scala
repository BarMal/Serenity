package com.serenity

import com.serenity.command.{CommandIntent, CommandRunnerSettingsInputItems, InterfaceChromeIntent, SettingsIntent}
import com.serenity.config.AppConfig
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** `editor.tab_max_width` in the settings palette. */
class CommandRunnerTabMaxWidthSettingsSpec extends AnyFlatSpec with Matchers:

  private def chrome(intent: InterfaceChromeIntent): CommandIntent =
    CommandIntent.Settings(SettingsIntent.InterfaceChrome(intent))

  "The tab max width input" should "show the configured width and parse only whole cells from 8 to 60" in {
    val width =
      CommandRunnerSettingsInputItems
        .build(AppConfig.default.withTabMaxWidth(40))
        .find(_.id == "tab-max-width")
        .getOrElse(fail("missing tab-max-width"))

    width.label shouldBe "Tab Max Width"
    width.currentValue shouldBe "40"
    width.parse("8") shouldBe Some(chrome(InterfaceChromeIntent.SetTabMaxWidth(8)))
    width.parse("60") shouldBe Some(chrome(InterfaceChromeIntent.SetTabMaxWidth(60)))
    width.parse("7") shouldBe None
    width.parse("61") shouldBe None
    width.parse("wide") shouldBe None
    width.defaultValue shouldBe Some("28")
  }
