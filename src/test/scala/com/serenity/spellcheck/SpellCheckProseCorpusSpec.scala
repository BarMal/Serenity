package com.serenity.spellcheck

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #1808: correctly spelled British-English prose is not flagged. Each group of the corpus exercises one rule that used
  * to reject correct words: affixes that combine, possessives and contractions in both apostrophe forms, hyphenated
  * words, sentence starts, numbers, and the URLs and Markdown syntax that sit next to prose.
  */
class SpellCheckProseCorpusSpec extends AnyFlatSpec with Matchers:

  private val (config, dictionary) = EnGbFixtureDictionary.load()

  private def flagged(text: String): List[String] =
    SpellChecker
      .analyzeText(text, config, dictionary)
      .filter(_.code.contains(SpellChecker.UnknownWordCode))
      .map(diagnostic =>
        text
          .split("\n", -1)(diagnostic.range.start.line)
          .substring(diagnostic.range.start.character, diagnostic.range.end.character)
      )

  private def groups: List[(String, List[String])] = List(
    "affixed forms"            -> EnGbProseCorpus.affixes,
    "possessives"              -> EnGbProseCorpus.possessives,
    "contractions"             -> EnGbProseCorpus.contractions,
    "hyphenated words"         -> EnGbProseCorpus.hyphenated,
    "sentence starts"          -> EnGbProseCorpus.sentenceStarts,
    "numbers"                  -> EnGbProseCorpus.numbers,
    "URLs and email addresses" -> EnGbProseCorpus.urls
  )

  groups.foreach { (group, lines) =>
    "The spell checker" should s"flag no correctly spelled $group" in {
      val falsePositives = lines.flatMap(line => flagged(line).map(word => s"$word in \"$line\""))

      falsePositives shouldBe Nil
    }
  }

  it should "report an empty false-positive rate over the whole corpus" in {
    val wrong = EnGbProseCorpus.all.count(line => flagged(line).nonEmpty)

    wrong shouldBe 0
    EnGbProseCorpus.all.size should be > 80
  }

  it should "still flag genuine misspellings next to the words it now accepts" in {
    flagged("the colour's depth and the organisatoin") shouldBe List("organisatoin")
    flagged("they don’t recieve it") shouldBe List("recieve")
    flagged("a well-knwon author") shouldBe List("knwon")
    flagged("a wrold of colour-blnid readers") shouldBe List("wrold", "blnid")
  }

  it should "flag a misspelling inside a possessive or contraction" in {
    flagged("the autor's notes") shouldBe List("autor's")
    flagged("we dont't know") shouldBe List("dont't")
  }

  it should "check the prose around Markdown syntax but not the syntax" in {
    val text = List(
      "---",
      "title: Wrld",
      "---",
      "Some `wrld` and a [lnk](https://wrld.example/wrld) wrld",
      "",
      "```scala",
      "val wrld = 1",
      "```",
      "<u>wrld</u> after"
    ).mkString("\n")

    flagged(text) shouldBe List("lnk", "wrld", "wrld")
  }

  it should "give each wrong part of a hyphenated word its own range" in {
    val diagnostics = SpellChecker
      .analyzeText("a wrld-knwon thing", config, dictionary)
      .filter(_.code.contains(SpellChecker.UnknownWordCode))

    diagnostics.map(d => d.range.start.character -> d.range.end.character) shouldBe List(2 -> 6, 7 -> 12)
  }

  it should "check a hyphenated word as one word when the dictionary says BREAK 0" in {
    val (unbroken, unbrokenDictionary) = EnGbFixtureDictionary.load("BREAK 0\n")

    SpellChecker
      .analyzeText("a well-known author", unbroken, unbrokenDictionary)
      .filter(_.code.contains(SpellChecker.UnknownWordCode))
      .map(_.message) shouldBe List("Possible spelling issue: well-known")
  }

  it should "not report hunspell directives that only guide suggestions as unsupported" in {
    val (_, withSuggestionTables) =
      EnGbFixtureDictionary.load("BREAK 2\nBREAK -\nBREAK –\nMAP 1\nMAP aà\nPHONE 1\nPHONE A A\n")

    withSuggestionTables.failures shouldBe Nil
  }
