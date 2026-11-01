package com.serenity.config

import java.nio.file.Files

import com.serenity.TestTemp
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** An invalid render setting in config.conf goes through the loader's existing report of invalid entries, which the
  * start page and the log show, and it names the values the setting accepts.
  */
class RenderSettingValidationSpec extends AnyFlatSpec with Matchers:

  private def loaded(content: String): ConfigLoadResult =
    val file = TestTemp.file("serenity-render-invalid", ".conf")
    Files.writeString(file, content)
    ConfigManagerTestSupport.loadConfigResult(Some(file.toString))

  "An invalid ui.render.backend" should "be reported with the value and the accepted choices, and use the default" in {
    val result = loaded("ui.render.backend = vulkan\n")
    val entry  = result.report.invalidEntries.find(_.key == "ui.render.backend")
    entry.map(_.value) shouldBe Some("vulkan")
    entry.map(_.reason).getOrElse("") should include("auto, skia or java2d")
    result.config.surfaceConfig.renderBackend shouldBe RenderBackendSetting.Auto
  }

  "An invalid ui.render.wayland" should "be reported with the value and the accepted choices, and use the default" in {
    val result = loaded("ui.render.wayland = sideways\n")
    val entry  = result.report.invalidEntries.find(_.key == "ui.render.wayland")
    entry.map(_.value) shouldBe Some("sideways")
    entry.map(_.reason).getOrElse("") should include("java2d, skia-raster or xwayland")
    result.config.surfaceConfig.renderWayland shouldBe WaylandRender.SkiaRaster
  }

  "The invalid-entry warning" should "reach the log text with the key, the value and the choices" in {
    val result  = loaded("ui.render.backend = vulkan\n")
    val message = ConfigMigrationWarning.message(TestTemp.file("serenity-render-warning", ".conf"), result.report)
    message.getOrElse("") should (include("ui.render.backend = vulkan") and include("auto, skia or java2d"))
  }

  "A valid render setting" should "be reported as nothing" in {
    loaded("ui.render.backend = skia\nui.render.wayland = xwayland\n").report.invalidEntries shouldBe Nil
  }

  "Other invalid settings" should "keep their existing wording" in {
    val reason = loaded("ui.render.fps = banana\n").report.invalidEntries.map(_.reason).headOption.getOrElse("")
    reason should include("not a value this setting accepts")
  }
