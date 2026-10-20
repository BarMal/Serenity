package com.serenity.spellcheck

import com.serenity.config.SpellCheckConfig
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #1866: a run of text in a script no installed dictionary covers (Han, kana, Hangul) is not spell-checked: with
  * nothing to check it against, every such word would be flagged. Latin text beside it is still checked.
  */
class SpellCheckUncoveredScriptsSpec extends AnyFlatSpec with Matchers:

  private val config = SpellCheckConfig(enabled = true, languages = List("en"))

  private def dictionaryOf(words: String*): DictionaryContext =
    DictionaryContext(Set("hello", "world") ++ words, Map.empty, Nil)

  private def flagged(text: String, dictionary: DictionaryContext = dictionaryOf()): List[(String, Int, Int)] =
    SpellChecker
      .analyzeText(text, config, dictionary)
      .map(diagnostic =>
        (
          text.substring(diagnostic.range.start.character, diagnostic.range.end.character),
          diagnostic.range.start.character,
          diagnostic.range.end.character
        )
      )

  "Spell checking" should "leave a Chinese run alone" in {
    flagged("你好世界") shouldBe empty
  }

  it should "leave Japanese kana and Korean Hangul alone" in {
    flagged("こんにちは カタカナ 안녕하세요") shouldBe empty
  }

  it should "still flag a misspelled Latin word beside them" in {
    flagged("hello teh 你好 world") shouldBe List(("teh", 6, 9))
  }

  it should "check only the Latin part of a word that runs into a Han run" in {
    flagged("teh世界") shouldBe List(("teh", 0, 3))
  }

  it should "check the Latin parts either side of a Han run inside a hyphenated word" in {
    flagged("teh-世界-wrld") shouldBe List(("teh", 0, 3), ("wrld", 7, 11))
  }

  it should "check a script a dictionary declares letters of" in {
    flagged("你好世界", dictionaryOf("你好")) shouldBe List(("你好世界", 0, 4))
    flagged("你好世界", dictionaryOf("你好世界")) shouldBe empty
  }

  it should "not change how a script it has no special case for is checked" in {
    flagged("привет hello") shouldBe List(("привет", 0, 6))
  }
