package com.serenity

import java.nio.charset.StandardCharsets
import java.nio.file.Files

import com.serenity.config.*
import com.serenity.lsp.config.{LanguageId, LspServerOverride, LspUserConfig}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LanguageToolsConfigSpec extends AnyFlatSpec with Matchers:

  "LanguageToolsConfig" should "own language-tool schema metadata and dynamic prefixes" in {
    ConfigKeySchema.currentKeys.should(contain("editor.syntax_highlighting"))
    ConfigKeySchema.currentKeys.should(contain("spellcheck.dictionary_paths"))
    ConfigKeySchema.deprecatedKeys("syntax_highlighting").shouldBe("editor.syntax_highlighting")
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
    ConfigRegistry.read(AppConfig.default, "editor.syntax_highlighting", "maybe").shouldBe(None)
  }

  it should "validate language-tool config entries centrally" in {
    ConfigRegistry.rejects("editor.syntax_highlighting", "true").shouldBe(false)
    ConfigRegistry.rejects("editor.syntax_highlighting", "maybe").shouldBe(true)
    ConfigRegistry.rejects("spellcheck.enabled", "on").shouldBe(false)
    ConfigRegistry.rejects("spellcheck.enabled", "perhaps").shouldBe(true)
    ConfigRegistry.rejects("spellcheck.languages", "en,fr").shouldBe(false)
    ConfigRegistry.rejects("spellcheck.dictionary_paths", "").shouldBe(false)
    ConfigRegistry.rejects("spellcheck.words", "Serenity,IO").shouldBe(false)
  }

  // #1175: a system with an already-installed Hunspell/MySpell dictionary (e.g. `hunspell-en-gb` on Linux) should
  // spell-check with zero config -- `discoverDictionarySourcePaths` takes the OS directory list as a parameter
  // (defaulting to `SpellCheckConfig.defaultOsDictionaryDirectories()`) precisely so these specs can point it at a
  // temp directory instead of depending on what is actually installed on the machine running the suite.
  "SpellCheckConfig.discoverDictionarySourcePaths" should
    "fall back to a language-matching dictionary in an OS-standard directory when dictionaryPaths is unconfigured" in {
      val osDirectory = Files.createTempDirectory("serenity-os-hunspell")
      val enDic       = osDirectory.resolve("en.dic")
      Files.writeString(enDic, "1\nhello", StandardCharsets.UTF_8)

      val config = SpellCheckConfig(languages = List("en"))
      val paths =
        SpellCheckConfig.discoverDictionarySourcePaths(config, osDictionaryDirectories = List(osDirectory.toString))

      paths.shouldBe(List(enDic))
    }

  it should
    "resolve no candidates when neither dictionaryPaths nor any OS-standard directory has a matching dictionary" in {
      val emptyOsDirectory = Files.createTempDirectory("serenity-os-hunspell-empty")

      val paths = SpellCheckConfig.discoverDictionarySourcePaths(
        SpellCheckConfig(),
        osDictionaryDirectories = List(emptyOsDirectory.toString, "/definitely/does/not/exist")
      )

      paths.shouldBe(Nil)
    }

  it should
    "leave an already-configured dictionaryPaths resolution unchanged, never consulting OS-standard directories" in {
      val osDirectory       = Files.createTempDirectory("serenity-os-hunspell-ignored")
      val osDic             = osDirectory.resolve("en.dic")
      val explicitDirectory = Files.createTempDirectory("serenity-explicit")
      val explicitDic       = explicitDirectory.resolve("en.dic")
      Files.writeString(osDic, "1\nhello", StandardCharsets.UTF_8)
      Files.writeString(explicitDic, "1\nworld", StandardCharsets.UTF_8)

      val config = SpellCheckConfig(languages = List("en"), dictionaryPaths = List(explicitDirectory.toString))
      val paths =
        SpellCheckConfig.discoverDictionarySourcePaths(config, osDictionaryDirectories = List(osDirectory.toString))

      paths.shouldBe(List(explicitDic))
    }

  "SpellCheckConfig.defaultOsDictionaryDirectories" should "list standard Linux Hunspell/MySpell locations" in {
    val directories = SpellCheckConfig.defaultOsDictionaryDirectories(osName = "Linux")

    directories.should(contain("/usr/share/hunspell"))
    directories.should(contain("/usr/share/myspell/dicts"))
  }

  it should "list standard macOS Hunspell/Spelling locations, including the user's own Library" in {
    val directories =
      SpellCheckConfig.defaultOsDictionaryDirectories(osName = "Mac OS X", userHome = "/Users/serenity")

    directories.should(contain("/Library/Spelling"))
    directories.should(contain("/Users/serenity/Library/Spelling"))
  }

  it should "list standard Windows dictionary locations under ProgramData, when set" in {
    val directories = SpellCheckConfig
      .defaultOsDictionaryDirectories(osName = "Windows 11", programData = Some("C:\\ProgramData"))

    directories.should(contain("C:\\ProgramData\\hunspell"))
  }

  it should "omit the ProgramData-derived Windows location when the environment variable is unset" in {
    val directories =
      SpellCheckConfig.defaultOsDictionaryDirectories(osName = "Windows 11", programData = None)

    directories.shouldBe(Nil)
  }
