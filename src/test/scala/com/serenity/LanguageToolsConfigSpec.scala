package com.serenity

import com.serenity.config.*
import com.serenity.lsp.config.{LanguageId, LspServerOverride, LspUserConfig}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LanguageToolsConfigSpec extends AnyFlatSpec with Matchers:

  "LanguageToolsConfig" should "own language-tool schema metadata and dynamic prefixes" in {
    ConfigKeySchema.currentKeys.should(contain("syntax.highlighting"))
    ConfigKeySchema.currentKeys.should(contain("spellcheck.dictionary_paths"))
    ConfigKeySchema.deprecatedKeys("syntax_highlighting").shouldBe("syntax.highlighting")
    ConfigKeySchema.deprecatedKeys("spellcheck_words").shouldBe("spellcheck.words")
    ConfigKeySchema.dynamicPrefixes.should(contain("lsp."))
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
      ConfigRegistry
        .read(AppConfig.default, "syntax_highlighting", "true")
        .getOrElse(fail("syntax parse"))
    val spellEnabledConfig =
      ConfigRegistry
        .read(AppConfig.default, "spellcheck.enabled", "on")
        .getOrElse(fail("spellcheck enabled parse"))
    val languageConfig =
      ConfigRegistry
        .read(AppConfig.default, "spellcheck_languages", " en,FR,,en ")
        .getOrElse(fail("spellcheck languages parse"))
    val dictionaryConfig =
      ConfigRegistry
        .read(
          AppConfig.default,
          "spellcheck.dictionary.paths",
          " C:\\Dictionaries\\en_US.dic , /usr/share/hunspell/fr.dic "
        )
        .getOrElse(fail("spellcheck dictionary paths parse"))
    val wordsConfig =
      ConfigRegistry
        .read(AppConfig.default, "spellcheck.words", " Serenity,IO,,serenity ")
        .getOrElse(fail("spellcheck words parse"))

    syntaxConfig.languageToolsConfig.syntaxHighlightingEnabled.shouldBe(true)
    spellEnabledConfig.languageToolsConfig.spellCheck.enabled.shouldBe(true)
    languageConfig.languageToolsConfig.spellCheck.languages.shouldBe(List("en", "fr"))
    dictionaryConfig.languageToolsConfig.spellCheck.dictionaryPaths.shouldBe(
      List("C:\\Dictionaries\\en_US.dic", "/usr/share/hunspell/fr.dic")
    )
    wordsConfig.languageToolsConfig.spellCheck.additionalWords.shouldBe(List("serenity", "io"))
    ConfigRegistry.read(AppConfig.default, "syntax.highlighting", "maybe").shouldBe(None)
  }

  it should "validate language-tool config entries centrally" in {
    ConfigRegistry.rejects("syntax.highlighting", "true").shouldBe(false)
    ConfigRegistry.rejects("syntax.highlighting", "maybe").shouldBe(true)
    ConfigRegistry.rejects("spellcheck.enabled", "on").shouldBe(false)
    ConfigRegistry.rejects("spellcheck.enabled", "perhaps").shouldBe(true)
    ConfigRegistry.rejects("spellcheck.languages", "en,fr").shouldBe(false)
    ConfigRegistry.rejects("spellcheck.dictionary_paths", "").shouldBe(false)
    ConfigRegistry.rejects("spellcheck.words", "Serenity,IO").shouldBe(false)
  }
