package com.serenity.spellcheck

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #1939: a misspelling with no REP entry still gets ranked corrections when they are asked for, and the background
  * analysis does not search for them.
  */
class SpellSuggesterSpec extends AnyFlatSpec with Matchers:

  private val (config, dictionary) = EnGbFixtureDictionary.load()

  private def suggestions(word: String): List[String] = SpellSuggester.suggest(word, dictionary)

  "SpellSuggester" should "correct a transposition" in {
    suggestions("wrold").headOption shouldBe Some("world")
  }

  it should "correct a deletion, an insertion and a substitution" in {
    suggestions("wrld").headOption shouldBe Some("world")
    suggestions("worlld").headOption shouldBe Some("world")
    suggestions("wurld").headOption shouldBe Some("world")
  }

  it should "correct an affixed form" in {
    suggestions("organisatons").headOption shouldBe Some("organisations")
    suggestions("colourfull").headOption shouldBe Some("colourful")
  }

  it should "offer a stem two edits away" in {
    suggestions("wrlds") should contain("world")
  }

  it should "correct a stem with its suffix put back" in {
    suggestions("jugdment").headOption shouldBe Some("judgement")
  }

  it should "split a run-together pair of words" in {
    suggestions("alot") should contain("a lot")
  }

  it should "give a typo of an apostrophe word its apostrophe" in {
    suggestions("dont") should contain("don't")
  }

  it should "carry the capitalisation of the typo over to its suggestions" in {
    suggestions("Wrold").headOption shouldBe Some("World")
    suggestions("WROLD").headOption shouldBe Some("WORLD")
  }

  it should "rank closer corrections first and offer a bounded number" in {
    val found = suggestions("recieve")

    found.headOption shouldBe Some("receive")
    found.size should be <= SpellSuggester.DefaultLimit
    found.distinct shouldBe found
  }

  it should "use the dictionary's REP table anywhere in the word" in {
    val replaced = dictionary.copy(replacements = Map("ph" -> List("f")))

    SpellSuggester.suggest("phirst", replaced).headOption shouldBe Some("first")
  }

  it should "suggest nothing for a word with nothing near it" in {
    suggestions("zzzzzzzzzz") shouldBe Nil
  }

  it should "leave suggestions out of the background analysis" in {
    val diagnostics = SpellChecker.analyzeText("a wrold", config, dictionary)

    diagnostics.map(_.message) shouldBe List("Possible spelling issue: wrold")
  }
