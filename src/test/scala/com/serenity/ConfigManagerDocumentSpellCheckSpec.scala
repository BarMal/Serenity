package com.serenity

import java.nio.file.Files

import com.serenity.config.*
import com.serenity.state.models.LargeFilePolicy
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Document mode and markdown view settings, plus spell-check and other language-tools configuration and its schema
  * validation.
  */
class ConfigManagerDocumentSpellCheckSpec extends AnyFlatSpec with Matchers with OptionValues:

  "ConfigManager" should "report invalid language-tool config values through the language-tools schema" in {
    val configFile = TestTemp.file("serenity-language-tools-invalid-config", ".conf")
    Files.writeString(
      configFile,
      """editor.syntax_highlighting = maybe
        |spellcheck.enabled = perhaps
        |""".stripMargin
    )

    val result = ConfigManagerTestSupport.loadConfigResult(Some(configFile.toString))

    result.report.invalidEntries.map(_.key) should contain("editor.syntax_highlighting")
    result.report.invalidEntries.map(_.key) should contain("spellcheck.enabled")
  }

  it should "load and write the default document mode" in {
    val configFile = TestTemp.file("serenity-default-document-mode", ".conf")
    Files.writeString(
      configFile,
      """editor.default_document_mode = markdown
        |""".stripMargin
    )

    val config = ConfigManagerTestSupport.loadConfig(Some(configFile.toString))

    config.defaultDocumentMode shouldBe DefaultDocumentMode.Markdown
    ConfigManager.configToString(config) should include("editor.default_document_mode = markdown")
  }

  it should "report invalid document config values through the document schema" in {
    val configFile = TestTemp.file("serenity-document-invalid-config", ".conf")
    Files.writeString(
      configFile,
      """editor.default_document_mode = wordperfect
        |editor.markdown_view = preview-ish
        |""".stripMargin
    )

    val result = ConfigManagerTestSupport.loadConfigResult(Some(configFile.toString))

    result.report.invalidEntries.map(_.key) should contain("editor.default_document_mode")
    result.report.invalidEntries.map(_.key) should contain("editor.markdown_view")
  }

  it should "load and write the markdown view mode" in {
    val configFile = TestTemp.file("serenity-markdown-view-mode", ".conf")
    Files.writeString(
      configFile,
      """editor.markdown_view = read
        |""".stripMargin
    )

    val config = ConfigManagerTestSupport.loadConfig(Some(configFile.toString))

    config.markdownViewMode shouldBe MarkdownViewMode.Read
    ConfigManager.configToString(config) should include("editor.markdown_view = read")
  }

  it should "load a retired inline-lens markdown view as live preview and write it back as live-preview" in {
    val configFile = TestTemp.file("serenity-markdown-view-retired", ".conf")
    Files.writeString(
      configFile,
      """editor.markdown_view = inline-lens
        |""".stripMargin
    )

    val config = ConfigManagerTestSupport.loadConfig(Some(configFile.toString))

    config.markdownViewMode shouldBe MarkdownViewMode.LivePreview
    ConfigManager.configToString(config) should include("editor.markdown_view = live-preview")
  }

  it should "load and write spell-check configuration" in {
    val configFile = TestTemp.file("serenity-spell-config", ".conf")
    Files.writeString(
      configFile,
      """spellcheck.enabled = true
        |spellcheck.languages = en,fr
        |spellcheck.dictionary_paths = C:\Dictionaries\en_US.dic,/usr/share/hunspell/fr.dic
        |spellcheck.words = Serenity,κόσμος,café
        |""".stripMargin
    )

    val config = ConfigManagerTestSupport.loadConfig(Some(configFile.toString))

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

  it should "default the large-file policy and read configured thresholds" in {
    AppConfig.default.largeFiles shouldBe LargeFilePolicy.Default

    val configFile = TestTemp.file("serenity-large-file-config", ".conf")
    Files.writeString(
      configFile,
      """editor.large_file.optimizations = false
        |editor.large_file.warn_above_chars = 1000
        |editor.large_file.tooling_off_above_chars = 2000
        |""".stripMargin
    )

    val config = ConfigManagerTestSupport.loadConfig(Some(configFile.toString))

    config.largeFiles shouldBe LargeFilePolicy(
      warnAboveChars = 1000,
      toolingOffAboveChars = 2000,
      optimizations = false
    )
    ConfigManager.configToString(config) should include("editor.large_file.warn_above_chars = 1000")
  }

  it should "reject a non-positive large-file threshold and keep the default" in {
    val configFile = TestTemp.file("serenity-large-file-invalid-config", ".conf")
    Files.writeString(
      configFile,
      """editor.large_file.warn_above_chars = 0
        |editor.large_file.tooling_off_above_chars = lots
        |""".stripMargin
    )

    val result = ConfigManagerTestSupport.loadConfigResult(Some(configFile.toString))

    result.report.invalidEntries.map(_.key) should contain allOf (
      "editor.large_file.warn_above_chars",
      "editor.large_file.tooling_off_above_chars"
    )
    result.config.largeFiles shouldBe LargeFilePolicy.Default
  }
