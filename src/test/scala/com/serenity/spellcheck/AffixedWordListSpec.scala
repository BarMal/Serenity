package com.serenity.spellcheck

import java.nio.charset.StandardCharsets
import java.nio.file.Files

import com.serenity.config.SpellCheckConfig
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #1939: a dictionary stays as stems and affixes are stripped when a word is looked up, rather than every surface form
  * being generated at load. Forms that only a second suffix reaches become recognisable.
  */
class AffixedWordListSpec extends AnyFlatSpec with Matchers:

  private def load(affix: String, entries: String*): DictionaryContext =
    val directory = Files.createTempDirectory("serenity-affixed-list")
    Files.writeString(directory.resolve("xx.aff"), affix, StandardCharsets.UTF_8)
    Files.writeString(
      directory.resolve("xx.dic"),
      s"${entries.size}\n${entries.mkString("\n")}\n",
      StandardCharsets.UTF_8
    )
    val config = SpellCheckConfig(enabled = true, languages = List("xx"), dictionaryPaths = List(directory.toString))
    DictionaryLoader.loadSnapshot(config, DictionaryCache()).context

  private def flagged(dictionary: DictionaryContext, text: String): List[String] =
    SpellChecker
      .analyzeText(text, SpellCheckConfig(enabled = true, languages = List("xx")), dictionary)
      .filter(_.code.contains(SpellChecker.UnknownWordCode))
      .map(d => text.substring(d.range.start.character, d.range.end.character))

  "A loaded dictionary" should "hold stems, not the surface forms its affixes generate" in {
    val dictionary = load("SFX S Y 1\nSFX S 0 s .\nPFX R Y 1\nPFX R 0 re .", "walk/SR", "talk/S")

    dictionary.words shouldBe empty
    dictionary.stems.map(_.size) shouldBe List(2)
    List("walk", "walks", "rewalk", "rewalks", "talk", "talks").forall(dictionary.knows) shouldBe true
    List("retalk", "walkss", "wal").exists(dictionary.knows) shouldBe false
  }

  it should "recognise a form only a second suffix reaches" in {
    val dictionary = load("SFX A Y 1\nSFX A 0 able/B .\nSFX B Y 1\nSFX B 0 s .", "work/A")

    flagged(dictionary, "workable workables") shouldBe Nil
    flagged(dictionary, "works") shouldBe List("works")
  }

  it should "not grant a second suffix to a stem lacking the first" in {
    val dictionary = load("SFX A Y 1\nSFX A 0 able/B .\nSFX B Y 1\nSFX B 0 s .", "work/A", "play/B")

    flagged(dictionary, "plays playable") shouldBe List("playable")
  }

  it should "keep a NEEDAFFIX stem valid only when affixed" in {
    val dictionary = load("NEEDAFFIX N\nSFX S Y 1\nSFX S 0 s .", "bound/NS")

    flagged(dictionary, "bounds") shouldBe Nil
    flagged(dictionary, "bound") shouldBe List("bound")
  }

  it should "apply a suffix condition to the stem it was stripped from" in {
    val dictionary = load("SFX E Y 2\nSFX E y ies [^aeiou]y\nSFX E 0 s [aeiou]y", "fly/E", "day/E")

    flagged(dictionary, "flies days") shouldBe Nil
    flagged(dictionary, "flys daies") shouldBe List("flys", "daies")
  }
