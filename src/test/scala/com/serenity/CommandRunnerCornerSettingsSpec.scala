package com.serenity.command

import com.serenity.config.{AppConfig, DockedCornerStyle, PaneHeaderCorners}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Issue #2389: the corner radius, the docked corner style and the pane header's corners in the settings palette. */
class CommandRunnerCornerSettingsSpec extends AnyFlatSpec with Matchers:

  private def chrome(intent: InterfaceChromeIntent): CommandIntent =
    CommandIntent.Settings(SettingsIntent.InterfaceChrome(intent))

  "The corner radius input" should "show the configured radius and parse only whole pixels from 0 to 32" in {
    val radius =
      CommandRunnerSettingsInputItems
        .build(AppConfig.default.withUiCornerRadiusPx(20))
        .find(_.id == "ui-corner-radius")
        .getOrElse(fail("missing ui-corner-radius"))

    radius.label shouldBe "UI Corner Radius"
    radius.currentValue shouldBe "20"
    radius.parse("0") shouldBe Some(chrome(InterfaceChromeIntent.SetUiCornerRadiusPx(0)))
    radius.parse("32") shouldBe Some(chrome(InterfaceChromeIntent.SetUiCornerRadiusPx(32)))
    radius.parse("33") shouldBe None
    radius.parse("-1") shouldBe None
    radius.parse("round") shouldBe None
    radius.defaultValue shouldBe Some("12")
  }

  "The docked corner style option" should "offer floating or flush, selecting the configured one" in {
    val item = CommandRunnerSettingsAppearanceItems.dockedCornerStyleOptionItem(Map("docked-corner-style" -> 1))

    item.label shouldBe "Docked Corner Style"
    item.options.map(_.label) shouldBe List("Floating", "Flush")
    item.selectedOption shouldBe "Flush"
    item.selectedIntent shouldBe Some(chrome(InterfaceChromeIntent.SetDockedCornerStyle(DockedCornerStyle.Flush)))
  }

  "The pane header corners option" should "offer rounded or square, selecting the configured one" in {
    val item = CommandRunnerSettingsAppearanceItems.paneHeaderCornersOptionItem(Map("pane-header-corners" -> 1))

    item.label shouldBe "Pane Header Corners"
    item.options.map(_.label) shouldBe List("Rounded", "Square")
    item.selectedOption shouldBe "Square"
    item.selectedIntent shouldBe Some(chrome(InterfaceChromeIntent.SetPaneHeaderCorners(PaneHeaderCorners.Square)))
  }

  "The option selections" should "follow the configured corner style and header corners" in {
    val config = AppConfig.default
      .withDockedCornerStyle(DockedCornerStyle.Flush)
      .withPaneHeaderCorners(PaneHeaderCorners.Square)
    val selections = CommandRunnerOptionSelections.default(config)

    selections("docked-corner-style") shouldBe 1
    selections("pane-header-corners") shouldBe 1
    CommandRunnerOptionSelections.default(AppConfig.conventional)("docked-corner-style") shouldBe 0
  }
