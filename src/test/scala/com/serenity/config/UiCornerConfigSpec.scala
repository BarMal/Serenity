package com.serenity.config

import java.nio.file.Files

import com.serenity.TestTemp
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Issue #2389 (owner decisions, 2026-10-31): the corner radius of the GUI's panels and floating surfaces, the style of
  * docked panels, and the pane header's top corners are settings. A radius outside its range is clamped to the nearest
  * bound and reported, never silently.
  */
class UiCornerConfigSpec extends AnyFlatSpec with Matchers:

  private def load(lines: String*): ConfigLoadResult =
    val file = TestTemp.file("serenity-corner-config", ".conf")
    Files.writeString(file, lines.mkString("", "\n", "\n"))
    ConfigManagerTestSupport.loadConfigResult(Some(file.toString))

  "The corner settings" should "default to a 12 px radius and a rounded pane header, whatever the layout" in
    List(AppConfig.default, AppConfig.conventional).foreach { config =>
      config.interfaceConfig.cornerRadiusPx shouldBe 12
      config.interfaceConfig.paneHeaderCorners shouldBe PaneHeaderCorners.Rounded
    }

  it should "start docked chrome flush in the minimal layout and floating in the conventional one" in {
    AppConfig.default.dockedCornerStyle shouldBe DockedCornerStyle.Flush
    AppConfig.conventional.dockedCornerStyle shouldBe DockedCornerStyle.Floating
  }

  it should "leave a docked corner style the file sets alone when the layout is conventional" in {
    val result = load("ui.layout = conventional", "ui.docked_corner_style = flush")
    result.config.dockedCornerStyle shouldBe DockedCornerStyle.Flush
  }

  it should "read all three from a config file, reporting nothing" in {
    val result = load("ui.corner_radius = 8", "ui.docked_corner_style = flush", "ui.pane_header_corners = square")
    val config = result.config.interfaceConfig
    config.cornerRadiusPx shouldBe 8
    config.dockedCornerStyle shouldBe DockedCornerStyle.Flush
    config.paneHeaderCorners shouldBe PaneHeaderCorners.Square
    result.report.invalidEntries shouldBe Nil
    result.report.removedKeys shouldBe Nil
    result.report.unknownKeys shouldBe Nil
  }

  it should "no longer report the corner radius, under any of its spellings, as removed" in
    List("ui.corner_radius", "ui.corner.radius", "ui_corner_radius").foreach { key =>
      withClue(key) {
        val result = load(s"$key = 5")
        result.config.interfaceConfig.cornerRadiusPx shouldBe 5
        result.report.removedKeys shouldBe Nil
        result.report.unknownKeys shouldBe Nil
      }
    }

  it should "accept a radius of 0 and of 32 without a notice" in
    List(0, 32).foreach { radius =>
      val result = load(s"ui.corner_radius = $radius")
      result.config.interfaceConfig.cornerRadiusPx shouldBe radius
      result.report.invalidEntries shouldBe Nil
    }

  it should "clamp a radius above the range to 32 and say so" in {
    val result = load("ui.corner_radius = 40")
    result.config.interfaceConfig.cornerRadiusPx shouldBe 32
    val notice = result.report.invalidEntries.find(_.key == "ui.corner_radius").getOrElse(fail("no notice"))
    notice.value shouldBe "40"
    notice.reason should include("32")
    notice.reason should include("maximum")
  }

  it should "clamp a radius below the range to 0 and say so" in {
    val result = load("ui.corner_radius = -5")
    result.config.interfaceConfig.cornerRadiusPx shouldBe 0
    val notice = result.report.invalidEntries.find(_.key == "ui.corner_radius").getOrElse(fail("no notice"))
    notice.reason should include("minimum")
  }

  it should "keep the default radius and say so when the value is not a whole number" in {
    val result = load("ui.corner_radius = round")
    result.config.interfaceConfig.cornerRadiusPx shouldBe 12
    result.report.invalidEntries.map(_.key) should contain("ui.corner_radius")
  }

  it should "keep the default and say so for an unknown docked corner style or header corner value" in {
    val result = load("ui.docked_corner_style = hovering", "ui.pane_header_corners = pointy")
    result.config.interfaceConfig.dockedCornerStyle shouldBe DockedCornerStyle.Flush
    result.config.interfaceConfig.paneHeaderCorners shouldBe PaneHeaderCorners.Rounded
    result.report.invalidEntries.map(_.key) should contain allOf ("ui.docked_corner_style", "ui.pane_header_corners")
  }

  it should "clamp the radius wherever an InterfaceConfig is built" in {
    InterfaceConfig(cornerRadiusPx = 99).normalized.cornerRadiusPx shouldBe 32
    InterfaceConfig(cornerRadiusPx = -1).normalized.cornerRadiusPx shouldBe 0
  }

  it should "scale the radius by the UI chrome scale, like the outline thickness" in {
    val base = AppConfig.default
    base.scaledUiCornerRadiusPx shouldBe 12
    val larger = base.withFontConfig(base.editorConfig.fontConfig.copy(uiFontSize = 24.0f))
    larger.uiChromeScale shouldBe 2.0
    larger.scaledUiCornerRadiusPx shouldBe 24
  }

  it should "parse the three enum spellings centrally" in {
    DockedCornerStyle.fromConfigKey("floating") shouldBe Some(DockedCornerStyle.Floating)
    DockedCornerStyle.fromConfigKey("flush") shouldBe Some(DockedCornerStyle.Flush)
    DockedCornerStyle.fromConfigKey("x") shouldBe None
    PaneHeaderCorners.fromConfigKey("rounded") shouldBe Some(PaneHeaderCorners.Rounded)
    PaneHeaderCorners.fromConfigKey("square") shouldBe Some(PaneHeaderCorners.Square)
    PaneHeaderCorners.fromConfigKey("x") shouldBe None
  }

  it should "round-trip through the config registry and its schema" in {
    ConfigKeySchema.isKnownKey("ui.corner_radius") shouldBe true
    ConfigKeySchema.isKnownKey("ui.corner.radius") shouldBe true
    ConfigKeySchema.isKnownKey("ui.docked_corner_style") shouldBe true
    ConfigKeySchema.isKnownKey("ui.pane_header_corners") shouldBe true
    val read = ConfigRegistry
      .read(AppConfig.default, "ui.docked_corner_style", "flush")
      .getOrElse(fail("docked corner style parse"))
    read.interfaceConfig.dockedCornerStyle shouldBe DockedCornerStyle.Flush
  }
