package com.serenity

import java.nio.file.Files

import com.serenity.config.*
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Document mode and markdown view settings, plus spell-check and other language-tools configuration and its
  * schema validation.
  */
class ConfigManagerDocumentSpellCheckSpec extends AnyFlatSpec with Matchers with OptionValues:

  "ConfigManager" should "report invalid language-tool config values through the language-tools schema" in {
    val configFile = Files.createTempFile("serenity-language-tools-invalid-config", ".conf")
    Files.writeString(
      configFile,
      """syntax.highlighting = maybe
        |spellcheck.enabled = perhaps
        |""".stripMargin
    )

    val result = ConfigManager.loadConfigResult(Some(configFile.toString))

    result.report.invalidEntries.map(_.key) should contain("syntax.highlighting")
    result.report.invalidEntries.map(_.key) should contain("spellcheck.enabled")
  }

  it should "load and write the default document mode" in {
    val configFile = Files.createTempFile("serenity-default-document-mode", ".conf")
    Files.writeString(
      configFile,
      """document.default_mode = markdown
        |""".stripMargin
    )

    val config = ConfigManager.loadConfig(Some(configFile.toString))

    config.defaultDocumentMode shouldBe DefaultDocumentMode.Markdown
    ConfigManager.configToString(config) should include("document.default_mode = markdown")
  }

  it should "report invalid document config values through the document schema" in {
    val configFile = Files.createTempFile("serenity-document-invalid-config", ".conf")
    Files.writeString(
      configFile,
      """document.default_mode = wordperfect
        |document.markdown_view = preview-ish
        |""".stripMargin
    )

    val result = ConfigManager.loadConfigResult(Some(configFile.toString))

    result.report.invalidEntries.map(_.key) should contain("document.default_mode")
    result.report.invalidEntries.map(_.key) should contain("document.markdown_view")
  }

  it should "load and write the markdown view mode" in {
    val configFile = Files.createTempFile("serenity-markdown-view-mode", ".conf")
    Files.writeString(
      configFile,
      """document.markdown_view = inline-lens
        |""".stripMargin
    )

    val config = ConfigManager.loadConfig(Some(configFile.toString))

    config.markdownViewMode shouldBe MarkdownViewMode.InlineLens
    ConfigManager.configToString(config) should include("document.markdown_view = inline-lens")
  }

  it should "load and write spell-check configuration" in {
    val configFile = Files.createTempFile("serenity-spell-config", ".conf")
    Files.writeString(
      configFile,
      """spellcheck.enabled = true
        |spellcheck.languages = en,fr
        |spellcheck.dictionary_paths = C:\Dictionaries\en_US.dic,/usr/share/hunspell/fr.dic
        |spellcheck.words = Serenity,κόσμος,café
        |""".stripMargin
    )

    val config = ConfigManager.loadConfig(Some(configFile.toString))

    config.languageToolsConfig.spellCheck.enabled shouldBe true
    config.languageToolsConfig.spellCheck.languages shouldBe List("en", "fr")
    config.languageToolsConfig.spellCheck.dictionaryPaths shouldBe List(
      "C:\\Dictionaries\\en_US.dic",
      "/usr/share/hunspell/fr.dic"
    )
    config.languageToolsConfig.spellCheck.additionalWords shouldBe List("serenity", "κόσμος", "café")

    val written = ConfigManager.configToString(config)
    written should include("spellcheck.enabled = true")
    written should include("spellcheck.languages = [\"en\", \"fr\"]")
    written should include(
      "spellcheck.dictionary_paths = [\"C:\\\\Dictionaries\\\\en_US.dic\", \"/usr/share/hunspell/fr.dic\"]"
    )
    written should include("spellcheck.words = [\"serenity\", \"κόσμος\", \"café\"]")
  }
