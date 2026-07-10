package com.serenity

import com.serenity.config.*
import com.serenity.lsp.config.{LanguageId, LspServerOverride, LspUserConfig}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LanguageToolsConfigSpec extends AnyFlatSpec with Matchers:

  "LanguageToolsConfig" should "own language-tool schema metadata and dynamic prefixes" in {
    LanguageToolsConfig.Schema.currentKeys.should(contain("syntax.highlighting"))
    LanguageToolsConfig.Schema.currentKeys.should(contain("spellcheck.dictionary_paths"))
    LanguageToolsConfig.Schema.deprecatedKeys("syntax_highlighting").shouldBe("syntax.highlighting")
    LanguageToolsConfig.Schema.deprecatedKeys("spellcheck_words").shouldBe("spellcheck.words")
    LanguageToolsConfig.Schema.dynamicPrefixes.shouldBe(List("lsp."))
  }

  it should "group syntax highlighting, LSP, and spell-check settings under AppConfig" in {
    val lspConfig = LspUserConfig(
      servers = Some(
        Map(
          LanguageId.Scala.id -> LspServerOverride(
            command = Some("custom-metals"),
            args = Some(List("--stdio")),
            enabled = Some(true)
          )
        )
      )
    )
    val spellCheck = SpellCheckConfig(
      enabled = true,
      languages = List("EN", "fr"),
      dictionaryPaths = List("C:\\Dictionaries\\en_US.dic"),
      additionalWords = List("Serenity", "IO")
    )

    val config = AppConfig.default
      .withSyntaxHighlighting(true)
      .withLspUserConfig(lspConfig)
      .withSpellCheck(spellCheck)

    config.languageToolsConfig.syntaxHighlightingEnabled.shouldBe(true)
    config.languageToolsConfig.lspUserConfig.shouldBe(lspConfig)
    config.languageToolsConfig.spellCheck.shouldBe(
      SpellCheckConfig(
        enabled = true,
        languages = List("en", "fr"),
        dictionaryPaths = List("C:\\Dictionaries\\en_US.dic"),
        additionalWords = List("serenity", "io")
      )
    )
  }

  it should "parse language-tool config entries centrally" in {
    val syntaxConfig =
      LanguageToolsConfig.Schema
        .parse(AppConfig.default, "syntax_highlighting", "true")
        .getOrElse(fail("syntax parse"))
    val spellEnabledConfig =
      LanguageToolsConfig.Schema
        .parse(AppConfig.default, "spellcheck.enabled", "on")
        .getOrElse(fail("spellcheck enabled parse"))
    val languageConfig =
      LanguageToolsConfig.Schema
        .parse(AppConfig.default, "spellcheck_languages", " en,FR,,en ")
        .getOrElse(fail("spellcheck languages parse"))
    val dictionaryConfig =
      LanguageToolsConfig.Schema
        .parse(
          AppConfig.default,
          "spellcheck.dictionary.paths",
          " C:\\Dictionaries\\en_US.dic , /usr/share/hunspell/fr.dic "
        )
        .getOrElse(fail("spellcheck dictionary paths parse"))
    val wordsConfig =
      LanguageToolsConfig.Schema
        .parse(AppConfig.default, "spellcheck.words", " Serenity,IO,,serenity ")
        .getOrElse(fail("spellcheck words parse"))

    syntaxConfig.languageToolsConfig.syntaxHighlightingEnabled.shouldBe(true)
    spellEnabledConfig.spellCheck.enabled.shouldBe(true)
    languageConfig.spellCheck.languages.shouldBe(List("en", "fr"))
    dictionaryConfig.spellCheck.dictionaryPaths.shouldBe(
      List("C:\\Dictionaries\\en_US.dic", "/usr/share/hunspell/fr.dic")
    )
    wordsConfig.spellCheck.additionalWords.shouldBe(List("serenity", "io"))
    LanguageToolsConfig.Schema.parse(AppConfig.default, "syntax.highlighting", "maybe").shouldBe(None)
  }

  it should "validate language-tool config entries centrally" in {
    LanguageToolsConfig.Schema.invalidValue("syntax.highlighting", "true").shouldBe(false)
    LanguageToolsConfig.Schema.invalidValue("syntax.highlighting", "maybe").shouldBe(true)
    LanguageToolsConfig.Schema.invalidValue("spellcheck.enabled", "on").shouldBe(false)
    LanguageToolsConfig.Schema.invalidValue("spellcheck.enabled", "perhaps").shouldBe(true)
    LanguageToolsConfig.Schema.invalidValue("spellcheck.languages", "en,fr").shouldBe(false)
    LanguageToolsConfig.Schema.invalidValue("spellcheck.dictionary_paths", "").shouldBe(false)
    LanguageToolsConfig.Schema.invalidValue("spellcheck.words", "Serenity,IO").shouldBe(false)
  }
