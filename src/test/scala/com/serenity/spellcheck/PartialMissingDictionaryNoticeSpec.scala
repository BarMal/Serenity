package com.serenity.spellcheck

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import com.serenity.TestTemp
import com.serenity.config.SpellCheckConfig
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The missing-dictionary notice names every configured language with no dictionary, not only the all-missing case. */
class PartialMissingDictionaryNoticeSpec extends AnyFlatSpec with Matchers:

  private def dictionaryDirectory(prefix: String, names: String*): Path =
    val directory = TestTemp.directory(prefix)
    names.foreach { name =>
      Files.writeString(directory.resolve(s"$name.dic"), "1\ncolour", StandardCharsets.UTF_8)
      Files.writeString(directory.resolve(s"$name.aff"), "SET UTF-8", StandardCharsets.UTF_8)
    }
    directory

  private def snapshot(languages: List[String], osDirectory: Path): DictionarySnapshot =
    DictionaryLoader.loadSnapshot(
      SpellCheckConfig(languages = languages),
      DictionaryCache(),
      List(osDirectory.toString)
    )

  "A partly resolved language set" should "produce one notice naming only the language with no dictionary" in {
    val osDirectory = dictionaryDirectory("serenity-partial", "en_US")

    val notice = snapshot(List("en-US", "fr"), osDirectory).context.missingDictionary
      .getOrElse(fail("expected a notice for fr"))

    notice should (include("fr") and include("hunspell-fr") and include(osDirectory.toString))
    notice should (not include "en-US" and not include "hunspell-en-us")
  }

  it should "list every missing language and a package suggestion for each" in {
    val osDirectory = dictionaryDirectory("serenity-partial-many", "en_US")

    val notice = snapshot(List("en-US", "fr", "de"), osDirectory).context.missingDictionary
      .getOrElse(fail("expected a notice"))

    notice should (include("fr") and include("de") and include("hunspell-fr") and include("hunspell-de"))
    notice should not include "hunspell-en-us"
  }

  it should "never name the bundled British English as missing" in {
    val emptyOsDirectory = TestTemp.directory("serenity-partial-bundled")

    val notice = snapshot(List("en-GB", "fr"), emptyOsDirectory).context.missingDictionary
      .getOrElse(fail("expected a notice for fr"))

    notice should (include("hunspell-fr") and not include "en-GB" and not include "hunspell-en-gb")
  }

  it should "keep checking prose in the languages that did resolve" in {
    val osDirectory = dictionaryDirectory("serenity-partial-checks", "en_US")
    val config      = SpellCheckConfig(languages = List("en-US", "fr"))
    val context     = snapshot(config.languages, osDirectory).context

    SpellChecker.analyzeText("colour wurld", config, context).map(_.message) shouldBe
      List("Possible spelling issue: wurld")
  }

  "A language set with no dictionary at all" should "still produce a notice naming each language" in {
    val emptyOsDirectory = TestTemp.directory("serenity-none")

    val notice = snapshot(List("en-US", "de"), emptyOsDirectory).context.missingDictionary
      .getOrElse(fail("expected a notice"))

    notice should (include("en-US") and include("de") and include("hunspell-en-us") and include("hunspell-de"))
    notice should include(emptyOsDirectory.toString)
  }

  "A fully resolved language set" should "produce no notice" in {
    val osDirectory = dictionaryDirectory("serenity-all", "en_US", "fr")

    snapshot(List("en-US", "fr"), osDirectory).context.missingDictionary shouldBe None
  }

  it should "produce no notice for the bundled British English plus an installed language" in {
    val osDirectory = dictionaryDirectory("serenity-bundled-plus", "fr")

    snapshot(List("en-GB", "fr"), osDirectory).context.missingDictionary shouldBe None
  }

  "A language with no dictionary" should "still be named in the notice" in {
    val notice = snapshot(List("fr"), TestTemp.directory("serenity-fallback-fr")).context.missingDictionary
      .getOrElse(fail("expected a notice for fr"))

    notice should (include("fr") and include("hunspell-fr"))
  }

  it should "not name English when only the generic en is configured, as the bundled dictionary stands in" in {
    snapshot(List("en"), TestTemp.directory("serenity-fallback-en")).context.missingDictionary shouldBe None
  }

  it should "name only the language that has no dictionary when en is configured beside it" in {
    val notice =
      snapshot(List("en", "fr"), TestTemp.directory("serenity-fallback-both")).context.missingDictionary
        .getOrElse(fail("expected a notice"))

    notice should (include("hunspell-fr") and not include "hunspell-en")
  }

  it should "not check any word" in {
    val config  = SpellCheckConfig(languages = List("fr"))
    val context = snapshot(config.languages, TestTemp.directory("serenity-fallback-check")).context

    SpellChecker.analyzeText("bonjour monde wurld", config, context) shouldBe Nil
  }

  it should "give no notice when spell check is disabled" in {
    val config = SpellCheckConfig(enabled = false, languages = List("fr"))

    DictionaryLoader
      .loadSnapshot(config, DictionaryCache(), List(TestTemp.directory("serenity-fallback-off").toString))
      .context
      .missingDictionary shouldBe None
  }

  "The bundled British English dictionary" should "serve en-GB, so no notice names it with an empty system" in {
    snapshot(List("en-GB"), TestTemp.directory("serenity-bundled-only")).context.missingDictionary shouldBe None
  }
