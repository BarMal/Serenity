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
