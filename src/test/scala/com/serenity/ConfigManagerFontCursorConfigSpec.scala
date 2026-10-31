package com.serenity

import java.awt.Color
import java.nio.file.Files

import com.serenity.config.*
import com.serenity.ui.fonts.{FontLoader, FontSizes}
import com.serenity.ui.fonts.FontLoader.TextScaleMode
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Font configuration (family/size/scale/ligatures) and cursor configuration (colour, mode, info bar) round-tripping.
  */
class ConfigManagerFontCursorConfigSpec extends AnyFlatSpec with Matchers with OptionValues:

  "ConfigManager" should "load and write font configuration including UI font family" in {
    val configFile = TestTemp.file("serenity-font-config", ".conf")
    Files.writeString(
      configFile,
      """typography.code.family = Monospaced
        |typography.prose.family = Serif
        |typography.ui.family = Dialog
        |typography.code.size = 15.0
        |typography.prose.size = 16.0
        |typography.ui.size = 13.0
        |typography.scale.mode = manual
        |typography.scale.factor = 1.5
        |typography.code.ligatures = false
        |typography.prose.ligatures = true
        |typography.ui.ligatures = true
        |""".stripMargin
    )

    val config = ConfigManagerTestSupport.loadConfig(Some(configFile.toString))

    config.editorConfig.fontConfig.codeFontFamily shouldBe "Monospaced"
    config.editorConfig.fontConfig.textFontFamily shouldBe "Serif"
    config.editorConfig.fontConfig.uiFontFamily shouldBe "Dialog"
    config.editorConfig.fontConfig.codeFontSize shouldBe 15.0f
    config.editorConfig.fontConfig.textFontSize shouldBe 16.0f
    config.editorConfig.fontConfig.uiFontSize shouldBe 13.0f
    config.editorConfig.fontConfig.textScaleMode shouldBe TextScaleMode.Manual
    config.editorConfig.fontConfig.textScaleMultiplier shouldBe 1.5
    config.editorConfig.fontConfig.codeLigatures shouldBe false
    config.editorConfig.fontConfig.textLigatures shouldBe true
    config.editorConfig.fontConfig.uiLigatures shouldBe true

    ConfigManager.configToString(config) should include("typography.ui.family = Dialog")
    ConfigManager.configToString(config) should include("typography.code.size = 15.0")
    ConfigManager.configToString(config) should include("typography.prose.size = 16.0")
    ConfigManager.configToString(config) should include("typography.scale.mode = manual")
    ConfigManager.configToString(config) should include("typography.scale.factor = 1.5")
    ConfigManager.configToString(config) should include("typography.ui.ligatures = true")
    ConfigManager.configToString(config) should include(s"config.version = ${ConfigVersion.Current.value}")
  }

  it should "load the previous font sizes as prose, code and UI, clamped to the range sizes accept (#2405)" in {
    def previousOf(line: String): Option[FontSizes] =
      val configFile = TestTemp.file("serenity-previous-sizes", ".conf")
      Files.writeString(configFile, line)
      ConfigManagerTestSupport.loadConfig(Some(configFile.toString)).editorConfig.fontConfig.previousSizes

    previousOf("typography.previous_sizes = \"36.0, 12.0, 11.0\"") shouldBe Some(FontSizes(36.0f, 12.0f, 11.0f))
    previousOf("typography.previous_sizes = [36.0, 12.0, 11.0]") shouldBe Some(FontSizes(36.0f, 12.0f, 11.0f))
    previousOf("typography.previous_sizes = \"60.0, 2.0, 11.0\"") shouldBe Some(FontSizes(48.0f, 8.0f, 11.0f))
    previousOf("typography.previous_sizes = off") shouldBe None
    previousOf("typography.previous_sizes = \"36.0, 12.0\"") shouldBe None
    previousOf("typography.prose.size = 20.0") shouldBe None
  }

  it should "write the previous font sizes and read them back after a reload (#2405)" in {
    val fonts  = FontLoader.FontConfig(previousSizes = Some(FontSizes(36.0f, 12.0f, 11.0f)))
    val config = AppConfig.default.withFontConfig(fonts)

    val written = ConfigManager.configToString(config)
    val file    = TestTemp.file("serenity-previous-sizes-reload", ".conf")
    Files.writeString(file, written)

    written should include("typography.previous_sizes")
    ConfigManagerTestSupport.loadConfig(Some(file.toString)).editorConfig.fontConfig.previousSizes shouldBe
      Some(FontSizes(36.0f, 12.0f, 11.0f))
  }

  it should "write no previous sizes as off, and read that back as none (#2405)" in {
    val written = ConfigManager.configToString(AppConfig.default)
    val file    = TestTemp.file("serenity-no-previous-sizes", ".conf")
    Files.writeString(file, written)

    written should include("typography.previous_sizes = off")
    ConfigManagerTestSupport.loadConfig(Some(file.toString)).editorConfig.fontConfig.previousSizes shouldBe None
  }

  it should "load the font fallback families in order and write them back (#1866)" in {
    val configFile = TestTemp.file("serenity-font-fallback-config", ".conf")
    Files.writeString(
      configFile,
      """typography.fallback.families = ["Noto Sans CJK JP", "Symbola"]
        |""".stripMargin
    )

    val config = ConfigManagerTestSupport.loadConfig(Some(configFile.toString))

    config.editorConfig.fontConfig.fallbackFamilies shouldBe List("Noto Sans CJK JP", "Symbola")
    config.editorConfig.fontConfig.fallback.slotFamilies shouldBe
      Vector("Noto Sans CJK JP", "Symbola") ++ FontLoader.emojiFontFamily :+ "Dialog"
    ConfigManager.configToString(config) should include(
      """typography.fallback.families = ["Noto Sans CJK JP", "Symbola"]"""
    )
  }

  it should "default to no configured fallback families, leaving only the system's resolver" in {
    AppConfig.default.editorConfig.fontConfig.fallbackFamilies shouldBe Nil
  }

  it should "clamp out-of-range font sizes when loading via the registry" in {
    val configFile = TestTemp.file("serenity-font-clamp-config", ".conf")
    Files.writeString(
      configFile,
      """typography.code.size = 400
        |typography.prose.size = 1
        |typography.ui.size = -5
        |""".stripMargin
    )

    val config = ConfigManagerTestSupport.loadConfig(Some(configFile.toString))

    config.editorConfig.fontConfig.codeFontSize shouldBe 48.0f
    config.editorConfig.fontConfig.textFontSize shouldBe 8.0f
    config.editorConfig.fontConfig.uiFontSize shouldBe 8.0f
  }

  it should "load and write active and inactive cursor colour overrides" in {
    val configFile = TestTemp.file("serenity-cursor-config", ".conf")
    Files.writeString(
      configFile,
      """editor.cursor.active_color = #3366CC
        |editor.cursor.inactive_color = #CC663380
        |""".stripMargin
    )

    val config = ConfigManagerTestSupport.loadConfig(Some(configFile.toString))

    config.cursorColors.active shouldBe Some(new Color(0x33, 0x66, 0xcc))
    config.cursorColors.inactive shouldBe Some(new Color(0xcc, 0x66, 0x33, 0x80))
    ConfigManager.configToString(config) should include("editor.cursor.active_color = \"#3366CC\"")
    ConfigManager.configToString(config) should include("editor.cursor.inactive_color = \"#CC663380\"")
  }

  it should "load a breathe cursor mode as blink and write it back as blink" in {
    val configFile = TestTemp.file("serenity-cursor-mode-config", ".conf")
    Files.writeString(
      configFile,
      """editor.cursor.mode = breathe
        |""".stripMargin
    )

    val config = ConfigManagerTestSupport.loadConfig(Some(configFile.toString))

    config.cursorMode shouldBe CursorMode.Blink
    ConfigManager.configToString(config) should include("editor.cursor.mode = blink")
  }

  it should "load and write cursor information bar mode" in {
    val configFile = TestTemp.file("serenity-cursor-info-config", ".conf")
    Files.writeString(
      configFile,
      """cursor.info_bar = detailed
        |cursor.info_bar.placement = pinned-bottom
        |""".stripMargin
    )

    val config = ConfigManagerTestSupport.loadConfig(Some(configFile.toString))

    // The old "detailed" preset plus the mode the corner glyph always showed alongside it, on today's status line.
    config.statusLine.segments shouldBe List(StatusSegment.Position, StatusSegment.Title, StatusSegment.Mode)
    config.statusLine.placement shouldBe StatusLinePlacement.Pinned
    // Written back under the current keys, with the segment list quoted: a bare comma made the file invalid HOCON.
    ConfigManager.configToString(config) should include("status.segments = \"position, title, mode\"")
    ConfigManager.configToString(config) should include("status.placement = pinned")
  }

  it should "ignore invalid cursor colour overrides" in {
    val configFile = TestTemp.file("serenity-cursor-config", ".conf")
    Files.writeString(
      configFile,
      """editor.cursor.active_color = not-a-colour
        |editor.cursor.inactive_color = #xyz
        |""".stripMargin
    )

    val config = ConfigManagerTestSupport.loadConfig(Some(configFile.toString))

    config.cursorColors.active shouldBe None
    config.cursorColors.inactive shouldBe None
  }

  it should "report invalid cursor config values through the cursor schema" in {
    val configFile = TestTemp.file("serenity-cursor-invalid-config", ".conf")
    Files.writeString(
      configFile,
      """editor.cursor.mode = unknown
        |editor.cursor.active_color = not-a-colour
        |cursor.info_bar = sideways
        |cursor.info_bar.placement = upside-down
        |""".stripMargin
    )

    val result = ConfigManagerTestSupport.loadConfigResult(Some(configFile.toString))

    result.report.invalidEntries.map(_.key) should contain("editor.cursor.mode")
    result.report.invalidEntries.map(_.key) should contain("editor.cursor.active_color")
    result.report.invalidEntries.map(_.key) should contain("cursor.info_bar")
    result.report.invalidEntries.map(_.key) should contain("cursor.info_bar.placement")
  }
