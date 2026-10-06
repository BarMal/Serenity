package com.serenity.spellcheck

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import com.serenity.config.SpellCheckConfig
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A bare `en` stands in for the bundled British English dictionary when nothing installed serves English, so the
  * default-era `["en"]` config still checks against a real dictionary rather than the 28-word fallback list. A
  * dictionary the user or the system supplies for English always wins.
  */
class BundledEnglishStandInSpec extends AnyFlatSpec with Matchers:

  private val generic = SpellCheckConfig(enabled = true, languages = List("en"))

  private def emptyOsDirectory: Path = Files.createTempDirectory("serenity-stand-in-empty")

  private def dictionaryIn(directory: Path, name: String, words: List[String]): Path =
    val dic = directory.resolve(s"$name.dic")
    Files.writeString(dic, (words.length.toString :: words).mkString("\n"), StandardCharsets.UTF_8)
    Files.writeString(directory.resolve(s"$name.aff"), "SET UTF-8", StandardCharsets.UTF_8)
    dic

  private def load(
    config: SpellCheckConfig,
    osDirectory: Path,
    cache: DictionaryCache = DictionaryCache()
  ): DictionarySnapshot =
    DictionaryLoader.loadSnapshot(config, cache, List(osDirectory.toString))

  private def flagged(text: String, config: SpellCheckConfig, snapshot: DictionarySnapshot): List[String] =
    SpellChecker.analyzeText(text, config, snapshot.context).map(_.message.stripPrefix("Possible spelling issue: "))

  "A bare en with no installed dictionary" should "check against the bundled British English dictionary" in {
    val snapshot = load(generic, emptyOsDirectory)

    flagged("colour organise neighbourhood wurld recieve", generic, snapshot) shouldBe List("wurld", "recieve")
  }

  it should "show no missing-dictionary notice and no load failure" in {
    val snapshot = load(generic, emptyOsDirectory)

    snapshot.context.missingDictionary shouldBe None
    snapshot.context.failures shouldBe Nil
  }

  it should "not flag the ordinary prose the 28-word fallback list used to flag" in {
    val snapshot = load(generic, emptyOsDirectory)

    flagged("The quick brown fox jumps over the lazy dog", generic, snapshot) shouldBe Nil
  }

  it should "parse the bundled dictionary only while spell check is enabled" in {
    val cache = DictionaryCache()

    load(generic.copy(enabled = false), emptyOsDirectory, cache)

    cache.size shouldBe 0
  }

  "A dictionary the system supplies for en" should "replace the bundled one" in {
    val osDirectory = Files.createTempDirectory("serenity-stand-in-system-en")
    val dic         = dictionaryIn(osDirectory, "en", List("zzcolour"))
    val cache       = DictionaryCache()
    val snapshot    = load(generic, osDirectory, cache)

    flagged("zzcolour colour", generic, snapshot) shouldBe List("colour")
    cache.size shouldBe 1
    cache.entryCount(dic) shouldBe 1
  }

  "An en_US dictionary named by dictionary_paths" should "replace the bundled one for en" in {
    val directory = Files.createTempDirectory("serenity-stand-in-en-us")
    val dic       = dictionaryIn(directory, "en_US", List("zzcolor"))
    val config    = generic.copy(dictionaryPaths = List(dic.toString))
    val cache     = DictionaryCache()
    val snapshot  = load(config, emptyOsDirectory, cache)

    flagged("zzcolor colour", config, snapshot) shouldBe List("colour")
    cache.size shouldBe 1
  }

  "A dictionary path not named for any language" should "replace the bundled one for en" in {
    val directory = Files.createTempDirectory("serenity-stand-in-unnamed")
    val dic       = dictionaryIn(directory, "words", List("zzcolour"))
    val config    = generic.copy(dictionaryPaths = List(dic.toString))
    val cache     = DictionaryCache()
    val snapshot  = load(config, emptyOsDirectory, cache)

    flagged("zzcolour colour", config, snapshot) shouldBe List("colour")
    cache.size shouldBe 1
  }

  "A dictionary path that fails to load" should "leave en on the bundled dictionary, reporting the failure" in {
    val missing  = Files.createTempDirectory("serenity-stand-in-missing").resolve("missing.dic")
    val config   = generic.copy(dictionaryPaths = List(missing.toString))
    val snapshot = load(config, emptyOsDirectory)

    snapshot.context.failures should not be empty
    flagged("colour wurld", config, snapshot) should (contain("wurld") and not contain "colour")
  }

  "A configured en-US" should "keep using the installed en_US dictionary, not the bundled one" in {
    val osDirectory = Files.createTempDirectory("serenity-stand-in-us")
    val dic         = dictionaryIn(osDirectory, "en_US", List("zzcolor"))
    val config      = SpellCheckConfig(enabled = true, languages = List("en-US"))
    val cache       = DictionaryCache()
    val snapshot    = load(config, osDirectory, cache)

    flagged("zzcolor colour", config, snapshot) shouldBe List("colour")
    cache.size shouldBe 1
    cache.entryCount(dic) shouldBe 1
  }

  it should "still show the notice, and flag nothing, when no en_US dictionary is installed" in {
    val config   = SpellCheckConfig(enabled = true, languages = List("en-US"))
    val cache    = DictionaryCache()
    val snapshot = load(config, emptyOsDirectory, cache)

    snapshot.context.missingDictionary.getOrElse(fail("expected a notice")) should include("hunspell-en-us")
    flagged("colour wurld", config, snapshot) shouldBe Nil
    cache.size shouldBe 0
  }

  "A bare en beside an installed fr" should "use the bundled dictionary for en and the installed one for fr" in {
    val osDirectory = Files.createTempDirectory("serenity-stand-in-fr")
    dictionaryIn(osDirectory, "fr", List("bonjour"))
    val config   = SpellCheckConfig(enabled = true, languages = List("en", "fr"))
    val cache    = DictionaryCache()
    val snapshot = load(config, osDirectory, cache)

    flagged("bonjour colour wurld", config, snapshot) shouldBe List("wurld")
    snapshot.context.missingDictionary shouldBe None
    cache.size shouldBe 2
  }

  "A bare en beside a fr with no dictionary" should "name only fr in the notice while still checking English" in {
    val config   = SpellCheckConfig(enabled = true, languages = List("en", "fr"))
    val snapshot = load(config, emptyOsDirectory)

    val notice = snapshot.context.missingDictionary.getOrElse(fail("expected a notice for fr"))
    notice should (include("hunspell-fr") and not include "hunspell-en")
    flagged("colour wurld", config, snapshot) shouldBe List("wurld")
  }

  "A language with only a built-in fallback list" should "show the notice and flag nothing" in
    List("fr", "el").foreach { language =>
      val config   = SpellCheckConfig(enabled = true, languages = List(language))
      val snapshot = load(config, emptyOsDirectory)

      snapshot.context.missingDictionary.getOrElse(fail(s"expected a notice for $language")) should
        include(s"hunspell-$language")
      flagged("bonjour wurld", config, snapshot) shouldBe Nil
    }
