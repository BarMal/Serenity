package com.serenity

import java.awt.Color
import java.nio.file.Files

import com.serenity.config.*
import com.serenity.ui.fonts.FontLoader.TextScaleMode
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Font configuration (family/size/scale/ligatures) and cursor configuration (colour, mode, info bar) round-tripping.
  */
class ConfigManagerFontCursorConfigSpec extends AnyFlatSpec with Matchers with OptionValues:

  "ConfigManager" should "load and write font configuration including UI font family" in {
    val configFile = Files.createTempFile("serenity-font-config", ".conf")
    Files.writeString(
      configFile,
      """font.code.family = Monospaced
        |font.text.family = Serif
        |font.ui.family = Dialog
        |font.code.size = 15.0
        |font.text.size = 16.0
        |font.ui.size = 13.0
        |font.scale.mode = manual
        |font.text_scale = 1.5
        |font.code.ligatures = false
        |font.text.ligatures = true
        |font.ui.ligatures = true
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

    ConfigManager.configToString(config) should include("font.ui.family = Dialog")
    ConfigManager.configToString(config) should include("font.code.size = 15.0")
    ConfigManager.configToString(config) should include("font.text.size = 16.0")
    ConfigManager.configToString(config) should include("font.scale.mode = manual")
    ConfigManager.configToString(config) should include("font.text_scale = 1.5")
    ConfigManager.configToString(config) should include("font.ui.ligatures = true")
    ConfigManager.configToString(config) should include("config.version = 1")
  }

  it should "clamp out-of-range font sizes when loading via the registry" in {
    val configFile = Files.createTempFile("serenity-font-clamp-config", ".conf")
    Files.writeString(
      configFile,
      """font.code.size = 400
        |font.text.size = 1
        |font.ui.size = -5
        |""".stripMargin
    )

    val config = ConfigManagerTestSupport.loadConfig(Some(configFile.toString))

    config.editorConfig.fontConfig.codeFontSize shouldBe 48.0f
    config.editorConfig.fontConfig.textFontSize shouldBe 8.0f
    config.editorConfig.fontConfig.uiFontSize shouldBe 8.0f
  }

  it should "load and write active and inactive cursor colour overrides" in {
    val configFile = Files.createTempFile("serenity-cursor-config", ".conf")
    Files.writeString(
      configFile,
      """cursor.active.color = #3366CC
        |cursor.inactive.color = #CC663380
        |""".stripMargin
    )

    val config = ConfigManagerTestSupport.loadConfig(Some(configFile.toString))

    config.cursorColors.active shouldBe Some(new Color(0x33, 0x66, 0xcc))
    config.cursorColors.inactive shouldBe Some(new Color(0xcc, 0x66, 0x33, 0x80))
    ConfigManager.configToString(config) should include("cursor.active.color = \"#3366CC\"")
    ConfigManager.configToString(config) should include("cursor.inactive.color = \"#CC663380\"")
  }

  it should "load and write cursor mode" in {
    val configFile = Files.createTempFile("serenity-cursor-mode-config", ".conf")
    Files.writeString(
      configFile,
      """cursor.mode = breathe
        |""".stripMargin
    )

    val config = ConfigManagerTestSupport.loadConfig(Some(configFile.toString))

    config.cursorMode shouldBe CursorMode.Breathe
    ConfigManager.configToString(config) should include("cursor.mode = breathe")
  }

  it should "load and write cursor information bar mode" in {
    val configFile = Files.createTempFile("serenity-cursor-info-config", ".conf")
    Files.writeString(
      configFile,
      """cursor.info_bar = detailed
        |cursor.info_bar.placement = pinned-bottom
        |""".stripMargin
    )

    val config = ConfigManagerTestSupport.loadConfig(Some(configFile.toString))

    config.cursorInfoBarSegments shouldBe List(CursorInfoBarSegment.Position, CursorInfoBarSegment.Title)
    config.cursorInfoBarPlacement shouldBe CursorInfoBarPlacement.PinnedBottom
    // The segment list is written under its own leaf and quoted: a bare comma made the file invalid HOCON, and
    // `cursor.info_bar` as a leaf on a path that also has children (`.placement`) is a value HOCON drops.
    ConfigManager.configToString(config) should include("cursor.info_bar.segments = \"position,title\"")
    ConfigManager.configToString(config) should include("cursor.info_bar.placement = pinned-bottom")
  }

  it should "ignore invalid cursor colour overrides" in {
    val configFile = Files.createTempFile("serenity-cursor-config", ".conf")
    Files.writeString(
      configFile,
      """cursor.active.color = not-a-colour
        |cursor.inactive.color = #xyz
        |""".stripMargin
    )

    val config = ConfigManagerTestSupport.loadConfig(Some(configFile.toString))

    config.cursorColors.active shouldBe None
    config.cursorColors.inactive shouldBe None
  }

  it should "report invalid cursor config values through the cursor schema" in {
    val configFile = Files.createTempFile("serenity-cursor-invalid-config", ".conf")
    Files.writeString(
      configFile,
      """cursor.mode = unknown
        |cursor.active.color = not-a-colour
        |cursor.info_bar = sideways
        |cursor.info_bar.placement = upside-down
        |""".stripMargin
    )

    val result = ConfigManagerTestSupport.loadConfigResult(Some(configFile.toString))

    result.report.invalidEntries.map(_.key) should contain("cursor.mode")
    result.report.invalidEntries.map(_.key) should contain("cursor.active.color")
    result.report.invalidEntries.map(_.key) should contain("cursor.info_bar")
    result.report.invalidEntries.map(_.key) should contain("cursor.info_bar.placement")
  }
