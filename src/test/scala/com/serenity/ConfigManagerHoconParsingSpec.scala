package com.serenity

import java.nio.charset.StandardCharsets
import java.nio.file.Files

import cats.effect.unsafe.implicits.global
import com.serenity.config.*
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** HOCON parsing edge cases: UTF-8 encoding, quoting/comments/substitutions/lists/commas, inline objects, legacy key
  * fallback, and file-relative includes.
  */
class ConfigManagerHoconParsingSpec extends AnyFlatSpec with Matchers with OptionValues:

  "ConfigManager" should "close loaded config files and save using UTF-8" in {
    val configFile = Files.createTempFile("serenity-config-utf8", ".conf")
    Files.writeString(
      configFile,
      """font.text.family = Sérif
        |""".stripMargin,
      StandardCharsets.UTF_8
    )

    val config = ConfigManager.loadConfig(Some(configFile.toString))
    config.editorConfig.fontConfig.textFontFamily shouldBe "Sérif"

    Files.delete(configFile)

    ConfigManager.saveConfig(config, configFile) shouldBe true
    Files.readString(configFile, StandardCharsets.UTF_8) should include("font.text.family = Sérif")
  }

  it should "round-trip HOCON quoting, comments, substitutions, lists, and commas" in {
    val configFile = Files.createTempFile("serenity-hocon-config", ".conf")
    Files.writeString(
      configFile,
      """font.text.family = "Text Font #1" # trailing comment
        |spellcheck.languages = ["en", "fr"]
        |spellcheck.dictionary_paths = ["C:\\Dictionaries\\en_US.dic", "/usr/share/hunspell/fr.dic"]
        |spellcheck.words = ["hello, world", "Café"]
        |font.ui.family = ${font.text.family}
        |""".stripMargin
    )

    val loaded = ConfigManager.loadConfig(Some(configFile.toString))

    loaded.editorConfig.fontConfig.textFontFamily shouldBe "Text Font #1"
    loaded.editorConfig.fontConfig.uiFontFamily shouldBe "Text Font #1"
    loaded.languageToolsConfig.spellCheck.languages shouldBe List("en", "fr")
    loaded.languageToolsConfig.spellCheck.dictionaryPaths shouldBe List(
      "C:\\Dictionaries\\en_US.dic",
      "/usr/share/hunspell/fr.dic"
    )
    loaded.languageToolsConfig.spellCheck.additionalWords shouldBe List("hello, world", "café")

    ConfigManager.saveConfig(loaded, configFile) shouldBe true
    val reloaded = ConfigManager.loadConfig(Some(configFile.toString))
    reloaded.editorConfig.fontConfig shouldBe loaded.editorConfig.fontConfig
    reloaded.languageToolsConfig.spellCheck shouldBe loaded.languageToolsConfig.spellCheck
  }

  it should "preserve inline slash comments without including them in unquoted values" in {
    val configFile = Files.createTempFile("serenity-hocon-slash-comment", ".conf")
    Files.writeString(
      configFile,
      """font.text.family = SansSerif // use the platform sans-serif font
        |font.ui.family = ${font.text.family}
        |""".stripMargin
    )

    val loaded = ConfigManager.loadConfig(Some(configFile.toString))

    loaded.editorConfig.fontConfig.textFontFamily shouldBe "SansSerif"
    loaded.editorConfig.fontConfig.uiFontFamily shouldBe "SansSerif"
  }

  it should "resolve substitutions inside brace-nested HOCON objects" in {
    val configFile = Files.createTempFile("serenity-hocon-nested-substitution", ".conf")
    Files.writeString(
      configFile,
      """font {
        |  text.family = "Nested Serif"
        |  ui.family = ${font.text.family}
        |}
        |""".stripMargin
    )

    val loaded = ConfigManager.loadConfig(Some(configFile.toString))

    loaded.editorConfig.fontConfig.textFontFamily shouldBe "Nested Serif"
    loaded.editorConfig.fontConfig.uiFontFamily shouldBe "Nested Serif"
  }

  it should "load supported settings from inline HOCON objects" in {
    val configFile = Files.createTempFile("serenity-hocon-inline-object", ".conf")
    Files.writeString(
      configFile,
      """font = { text = { family = "Inline Serif" }, ui = { family = ${font.text.family} } }
        |spellcheck = { enabled = true, languages = ["en", "fr"] }
        |""".stripMargin
    )

    val loaded = ConfigManager.loadConfig(Some(configFile.toString))

    loaded.editorConfig.fontConfig.textFontFamily shouldBe "Inline Serif"
    loaded.editorConfig.fontConfig.uiFontFamily shouldBe "Inline Serif"
    loaded.languageToolsConfig.spellCheck.enabled shouldBe true
    loaded.languageToolsConfig.spellCheck.languages shouldBe List("en", "fr")
  }

  it should "load legacy values through parser fallback without changing valid HOCON" in {
    val configFile = Files.createTempFile("serenity-hocon-legacy-path", ".conf")
    Files.writeString(
      configFile,
      """font.text.family = Legacy Serif
        |spellcheck.dictionary_paths = C:\Dictionaries\en_US.dic
        |viewport.width.max =
        |""".stripMargin
    )

    ConfigManager.loadConfigResultIO(Some(configFile.toString)).unsafeRunSync() match
      case Right(result) =>
        result.config.editorConfig.fontConfig.textFontFamily shouldBe "Legacy Serif"
        result.config.languageToolsConfig.spellCheck.dictionaryPaths shouldBe List("C:\\Dictionaries\\en_US.dic")
        result.config.surfaceConfig.viewportSizing.width.maxCells shouldBe None
      case Left(error) => fail(s"expected mixed legacy and HOCON config to load, received $error")
  }

  it should "resolve HOCON substitutions alongside legacy Windows path values" in {
    val configFile = Files.createTempFile("serenity-mixed-legacy-hocon", ".conf")
    Files.writeString(
      configFile,
      """font.text.family = "Text Font"
        |font.ui.family = ${font.text.family}
        |font.code.family = ${?missing.font.family}
        |spellcheck.dictionary_paths = C:\Dictionaries\en_US.dic
        |""".stripMargin
    )

    ConfigManager.loadConfigResultIO(Some(configFile.toString)).unsafeRunSync() match
      case Right(result) =>
        result.config.editorConfig.fontConfig.textFontFamily shouldBe "Text Font"
        result.config.editorConfig.fontConfig.uiFontFamily shouldBe "Text Font"
        result.config.editorConfig.fontConfig.codeFontFamily shouldBe AppConfig.default.editorConfig.fontConfig.codeFontFamily
        result.config.languageToolsConfig.spellCheck.dictionaryPaths shouldBe List("C:\\Dictionaries\\en_US.dic")
      case Left(error) => fail(s"expected mixed legacy and HOCON config to load, received $error")
  }

  it should "resolve file-relative includes and substitutions from the config path" in {
    val directory = Files.createTempDirectory("serenity-hocon-include")
    val included  = directory.resolve("included.conf")
    val root      = directory.resolve("application.conf")
    Files.writeString(
      included,
      """font.text.family = "Included Serif"
        |spellcheck.dictionary_paths = "C:\\Dictionaries\\en_US.dic"
        |""".stripMargin
    )
    Files.writeString(
      root,
      """include required("included.conf")
        |font.ui.family = ${font.text.family}
        |""".stripMargin
    )

    val loaded = ConfigManager.loadConfig(Some(root.toString))

    loaded.editorConfig.fontConfig.textFontFamily shouldBe "Included Serif"
    loaded.editorConfig.fontConfig.uiFontFamily shouldBe "Included Serif"
    loaded.languageToolsConfig.spellCheck.dictionaryPaths shouldBe List("C:\\Dictionaries\\en_US.dic")
  }
