package com.serenity.config

import java.nio.file.Files

import com.serenity.TestTemp
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** `editor.tab_max_width`: the widest a tab in the tab strip may be, in cells. A width outside its range is clamped to
  * the nearest bound and reported, never silently.
  */
class UiTabMaxWidthConfigSpec extends AnyFlatSpec with Matchers:

  private def load(lines: String*): ConfigLoadResult =
    val file = TestTemp.file("serenity-tab-max-width-config", ".conf")
    Files.writeString(file, lines.mkString("", "\n", "\n"))
    ConfigManagerTestSupport.loadConfigResult(Some(file.toString))

  "The tab max width setting" should "default to 28 cells, within 8 to 60" in {
    AppConfig.default.surfaceConfig.tabMaxWidth shouldBe 28
    TabMaxWidth.Default shouldBe 28
    TabMaxWidth.Min shouldBe 8
    TabMaxWidth.Max shouldBe 60
  }

  it should "be read from a config file, reporting nothing" in {
    val result = load("editor.tab_max_width = 40")
    result.config.surfaceConfig.tabMaxWidth shouldBe 40
    result.report.invalidEntries shouldBe Nil
    result.report.removedKeys shouldBe Nil
    result.report.unknownKeys shouldBe Nil
  }

  it should "accept a width of 8 and of 60 without a notice" in
    List(8, 60).foreach { width =>
      val result = load(s"editor.tab_max_width = $width")
      result.config.surfaceConfig.tabMaxWidth shouldBe width
      result.report.invalidEntries shouldBe Nil
    }

  it should "clamp a width above the range to 60 and say so" in {
    val result = load("editor.tab_max_width = 100")
    result.config.surfaceConfig.tabMaxWidth shouldBe 60
    val notice = result.report.invalidEntries.find(_.key == "editor.tab_max_width").getOrElse(fail("no notice"))
    notice.value shouldBe "100"
    notice.reason should include("60")
    notice.reason should include("maximum")
  }

  it should "clamp a width below the range to 8 and say so" in {
    val result = load("editor.tab_max_width = 2")
    result.config.surfaceConfig.tabMaxWidth shouldBe 8
    val notice = result.report.invalidEntries.find(_.key == "editor.tab_max_width").getOrElse(fail("no notice"))
    notice.reason should include("8")
    notice.reason should include("minimum")
  }

  it should "keep the default width and say so when the value is not a whole number" in {
    val result = load("editor.tab_max_width = wide")
    result.config.surfaceConfig.tabMaxWidth shouldBe 28
    result.report.invalidEntries.map(_.key) should contain("editor.tab_max_width")
  }

  it should "clamp the width wherever a SurfaceConfig is built" in {
    SurfaceConfig(tabMaxWidth = 99).normalized.tabMaxWidth shouldBe 60
    SurfaceConfig(tabMaxWidth = 1).normalized.tabMaxWidth shouldBe 8
    TabMaxWidth.clamp(30) shouldBe 30
  }

  it should "be set through withTabMaxWidth without touching the tab strip mode" in {
    val config = AppConfig.default.withTabStrip(TabStripMode.Off).withTabMaxWidth(36)
    config.surfaceConfig.tabMaxWidth shouldBe 36
    config.surfaceConfig.tabStrip shouldBe TabStripMode.Off
  }

  it should "round-trip through the config registry and its schema" in {
    ConfigKeySchema.isKnownKey("editor.tab_max_width") shouldBe true
    ConfigRegistry.defaultFor("editor.tab_max_width").map(_.rendered) shouldBe Some("28")
    val read = ConfigRegistry
      .read(AppConfig.default, "editor.tab_max_width", "44")
      .getOrElse(fail("tab max width parse"))
    read.surfaceConfig.tabMaxWidth shouldBe 44
  }

  it should "round-trip through a saved config file" in {
    val file = TestTemp.file("serenity-tab-max-width-roundtrip", ".conf")
    try
      ConfigManagerTestSupport.saveConfig(AppConfig.default.withTabMaxWidth(44), file) shouldBe true
      ConfigManagerTestSupport.loadConfig(Some(file.toString)).surfaceConfig.tabMaxWidth shouldBe 44
    finally Files.deleteIfExists(file): Unit
  }
