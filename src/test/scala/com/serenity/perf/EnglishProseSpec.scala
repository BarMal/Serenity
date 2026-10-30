package com.serenity.perf

import com.serenity.config.AppConfig
import com.serenity.spellcheck.{DictionaryCache, DictionaryLoader, SpellChecker}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The prose the typing rows write should read like a person's: English the real dictionary accepts, with the odd
  * misspelling a writer leaves in, not the nonsense of lorem ipsum, where nearly every word is flagged.
  */
class EnglishProseSpec extends AnyFlatSpec with Matchers:

  private val spellCheck = AppConfig.default.languageToolsConfig.spellCheck
  private val dictionary = DictionaryLoader.loadSnapshot(spellCheck, DictionaryCache()).context

  private def flagged(text: String): List[String] =
    SpellChecker
      .analyzeText(text, spellCheck, dictionary)
      .filter(_.code.contains(SpellChecker.UnknownWordCode))
      .map(diagnostic =>
        text
          .split("\n", -1)(diagnostic.range.start.line)
          .substring(diagnostic.range.start.character, diagnostic.range.end.character)
      )

  private def words(text: String): Int = text.split("\\s+").count(_.nonEmpty)

  "English prose without misspellings" should "be accepted by the bundled dictionary, every word of it" in {
    val document = EnglishProse.document(paragraphs = 60, misspellingEvery = 0)
    document.misspelt shouldBe 0
    flagged(document.text) shouldBe Nil
  }

  "English prose with misspellings" should "have exactly the injected words flagged" in {
    val document = EnglishProse.document(paragraphs = 60, misspellingEvery = 100)
    document.misspelt should be > 10
    flagged(document.text) should have size document.misspelt.toLong
  }

  it should "misspell about one word in a hundred" in {
    val document = EnglishProse.document(paragraphs = 60, misspellingEvery = 100)
    val rate     = document.misspelt.toDouble / words(document.text)
    rate should (be >= 0.008 and be <= 0.012)
  }

  "A document of English prose" should "be the same every time" in {
    EnglishProse.document(40, 100) shouldBe EnglishProse.document(40, 100)
  }

  it should "be paragraphs separated by one blank line" in {
    val document = EnglishProse.document(paragraphs = 25, misspellingEvery = 100)
    document.text.split("\n\n", -1) should have length 25
    document.text should not include "\n\n\n"
  }

  it should "grow with the paragraphs asked for, past the paragraphs written" in {
    val short = EnglishProse.document(10, 100).text
    val long  = EnglishProse.document(300, 100).text
    long.length should be > short.length * 20
  }
