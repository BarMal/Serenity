package com.serenity.config

import java.nio.file.Files

import com.serenity.TestTemp
import com.typesafe.config.ConfigFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The version 2 to 3 step (#2660 item 1): Java2D is no longer a renderer, so a file that chose it keeps working. */
class ConfigRenderMigrationSpec extends AnyFlatSpec with Matchers:

  private def load(text: String): ConfigLoadResult =
    val file = TestTemp.file("serenity-render-migration", ".conf")
    Files.writeString(file, text)
    ConfigManager.parseConfigResult(file, ConfigMigrations.installed)

  private def notes(result: ConfigLoadResult): List[String] = result.report.migrationNotes.map(_.message)

  "A version 2 file that chose java2d" should "use auto and say so" in {
    val result = load("config.version = 2\nui.render.backend = java2d\n")

    result.config.surfaceConfig.renderBackend shouldBe RenderBackendSetting.Auto
    result.report.invalidEntries shouldBe Nil
    result.report.migratedFrom shouldBe Some(ConfigVersion(2))
    notes(result) should have size 1
    notes(result).head should (include("ui.render.backend") and include("java2d") and include("auto"))
  }

  it should "use skia-raster for the Wayland render and say so" in {
    val result = load("config.version = 2\nui.render.wayland = java2d\n")

    result.config.surfaceConfig.renderWayland shouldBe WaylandRender.SkiaRaster
    result.report.invalidEntries shouldBe Nil
    notes(result) should have size 1
    notes(result).head should (include("ui.render.wayland") and include("java2d") and include("skia-raster"))
  }

  it should "be migrated for every spelling of the key and of the value" in {
    val spellings = List("ui.render.backend", "renderBackend", "render.backend", "render_backend")
    for
      key   <- spellings
      value <- List("java2d", "java", "JAVA2D", " Java2D ", "\"java2d\"")
    do
      val result = load(s"config.version = 2\n$key = $value\n")
      withClue(s"$key = $value: ") {
        result.config.surfaceConfig.renderBackend shouldBe RenderBackendSetting.Auto
        result.report.invalidEntries shouldBe Nil
        notes(result) should have size 1
      }
  }

  it should "be migrated when the file is written as quoted dotted keys" in {
    val result = load("\"config.version\" = 2\n\"ui.render.backend\" = java2d\n\"ui.render.wayland\" = java2d\n")

    result.config.surfaceConfig.renderBackend shouldBe RenderBackendSetting.Auto
    result.config.surfaceConfig.renderWayland shouldBe WaylandRender.SkiaRaster
    result.report.invalidEntries shouldBe Nil
    notes(result) should have size 2
  }

  it should "be migrated for both settings at once, with a note for each" in {
    val result = load("config.version = 2\nui.render.backend = java2d\nui.render.wayland = java2d\n")

    result.config.surfaceConfig.renderBackend shouldBe RenderBackendSetting.Auto
    result.config.surfaceConfig.renderWayland shouldBe WaylandRender.SkiaRaster
    notes(result) should have size 2
  }

  "A version 1 file that chose java2d" should "go through the hotkey step and this one" in {
    val result = load("config.version = 1\nui.render.backend = java2d\n")

    result.config.surfaceConfig.renderBackend shouldBe RenderBackendSetting.Auto
    result.report.migratedFrom shouldBe Some(ConfigVersion(1))
    notes(result) should have size 1
  }

  "A version 2 file that chose a renderer Skia has" should "keep it, with no note" in {
    val result = load("config.version = 2\nui.render.backend = skia-raster\nui.render.wayland = xwayland\n")

    result.config.surfaceConfig.renderBackend shouldBe RenderBackendSetting.SkiaRaster
    result.config.surfaceConfig.renderWayland shouldBe WaylandRender.XWayland
    notes(result) shouldBe Nil
    result.report.invalidEntries shouldBe Nil
  }

  "A version 2 file that chose nothing" should "get the defaults, with no note" in {
    val result = load("config.version = 2\nui.render.fps = 60\n")

    result.config.surfaceConfig.renderBackend shouldBe RenderBackendSetting.Auto
    result.config.surfaceConfig.renderWayland shouldBe WaylandRender.SkiaRaster
    notes(result) shouldBe Nil
  }

  "A version 3 file that says java2d" should "be left alone and reported as invalid" in {
    val result = load("config.version = 3\nui.render.backend = java2d\n")

    notes(result) shouldBe Nil
    result.report.invalidEntries.map(_.key) shouldBe List("ui.render.backend")
    result.config.surfaceConfig.renderBackend shouldBe RenderBackendSetting.Auto
  }

  "The migrated file" should "be stamped with the current version, having applied the step from version 2" in {
    val outcome = ConfigMigrations.migrate(
      ConfigFactory.parseString("config.version = 2\nui.render.backend = java2d\n"),
      ConfigMigrations.installed
    )

    outcome.config.getInt("config.version") shouldBe ConfigVersion.Current.value
    outcome.config.getString("ui.render.backend") shouldBe "auto"
    outcome.applied should contain(ConfigVersion(2))
  }

end ConfigRenderMigrationSpec
