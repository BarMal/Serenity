package com.serenity.spellcheck

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import com.serenity.config.SpellCheckConfig
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #1680: spell check works for British English with no configuration -- on by default, `en-GB` by default, and
  * dictionaries found under their conventional `ll_CC` file names on case-sensitive filesystems.
  */
class SpellCheckZeroConfigSpec extends AnyFlatSpec with Matchers:

  private val AmericanEnglish = SpellCheckConfig(languages = List("en-US"))

  private def dictionaryDirectory(prefix: String, names: String*): Path =
    val directory = Files.createTempDirectory(prefix)
    names.foreach { name =>
      Files.writeString(directory.resolve(s"$name.dic"), "1\ncolour", StandardCharsets.UTF_8)
      Files.writeString(directory.resolve(s"$name.aff"), "SET UTF-8", StandardCharsets.UTF_8)
    }
    directory

  "SpellCheckConfig's defaults" should "check British English prose without being switched on" in {
    val defaults = SpellCheckConfig()

    defaults.enabled shouldBe true
    defaults.languages shouldBe List("en-GB")
  }

  "Dictionary discovery" should "find an installed en_GB.dic with the default config" in {
    val osDirectory = dictionaryDirectory("serenity-zero-config", "en_GB")

    SpellCheckConfig.discoverDictionarySourcePaths(SpellCheckConfig(), List(osDirectory.toString)) shouldBe
      List(osDirectory.resolve("en_GB.dic"))
  }

  it should "find en_US.dic when American English is selected, however the language code is cased" in {
    val osDirectory = dictionaryDirectory("serenity-en-us", "en_GB", "en_US")

    List("en-US", "en_us", "EN-us").foreach { language =>
      SpellCheckConfig.discoverDictionarySourcePaths(
        SpellCheckConfig(languages = List(language)),
        List(osDirectory.toString)
      ) shouldBe List(osDirectory.resolve("en_US.dic"))
    }
  }

  it should "canonicalise language codes to language-REGION form" in {
    SpellCheckConfig(languages = List(" en_gb ", "EN-us", "fr", "en-GB")).normalized.languages shouldBe
      List("en-GB", "en-US", "fr")
  }

  it should "still find a dictionary whose file name is lower-cased" in {
    val osDirectory = dictionaryDirectory("serenity-lowercase", "en_gb")

    SpellCheckConfig.discoverDictionarySourcePaths(SpellCheckConfig(), List(osDirectory.toString)) shouldBe
      List(osDirectory.resolve("en_gb.dic"))
  }

  it should "load a language's dictionary once even when several standard directories hold it" in {
    val hunspell = dictionaryDirectory("serenity-hunspell", "en_GB")
    val myspell  = dictionaryDirectory("serenity-myspell", "en_GB")

    SpellCheckConfig.discoverDictionarySourcePaths(
      SpellCheckConfig(),
      List(hunspell.toString, myspell.toString)
    ) shouldBe List(hunspell.resolve("en_GB.dic"))
  }

  "Spell check with no dictionary installed" should "have a notice naming the searched directories" in {
    val emptyOsDirectory = Files.createTempDirectory("serenity-no-dictionary")
    val snapshot =
      DictionaryLoader.loadSnapshot(AmericanEnglish, DictionaryCache(), List(emptyOsDirectory.toString))

    snapshot.context.missingDictionary.getOrElse(fail("expected a notice")) should
      (include("en-US") and include(emptyOsDirectory.toString) and include("hunspell-en-us"))
  }

  it should "not flag ordinary words against an empty word list" in {
    val snapshot = DictionaryLoader.loadSnapshot(
      AmericanEnglish,
      DictionaryCache(),
      List(Files.createTempDirectory("serenity-no-dictionary").toString)
    )

    SpellChecker.analyzeText("Colour me unconvinced.", AmericanEnglish, snapshot.context) shouldBe Nil
  }

  it should "have no notice for the default British English, which ships a dictionary" in {
    val emptyOsDirectory = Files.createTempDirectory("serenity-no-dictionary")

    DictionaryLoader
      .loadSnapshot(SpellCheckConfig(), DictionaryCache(), List(emptyOsDirectory.toString))
      .context
      .missingDictionary shouldBe None
  }

  it should "have no notice when a dictionary was found" in {
    val osDirectory = dictionaryDirectory("serenity-found", "en_GB")

    DictionaryLoader
      .loadSnapshot(SpellCheckConfig(), DictionaryCache(), List(osDirectory.toString))
      .context
      .missingDictionary shouldBe None
  }

  "Spell check with the default config and an installed dictionary" should "flag only unknown words" in {
    val osDirectory = dictionaryDirectory("serenity-installed", "en_GB")
    val snapshot    = DictionaryLoader.loadSnapshot(SpellCheckConfig(), DictionaryCache(), List(osDirectory.toString))

    SpellChecker.analyzeText("colour wurld", SpellCheckConfig(), snapshot.context).map(_.message) shouldBe
      List("Possible spelling issue: wurld")
  }
