package com.serenity.spellcheck

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import scala.io.Source

import com.serenity.TestTemp
import com.serenity.config.SpellCheckConfig
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The LibreOffice/Hunspell en_GB dictionary ships inside the application, so British English spell checking works with
  * no dictionary installed or configured; a dictionary the user supplies always wins over it.
  */
class BundledDictionarySpec extends AnyFlatSpec with Matchers:

  private val britishEnglish = SpellCheckConfig(enabled = true, languages = List("en-gb"))

  private def emptyOsDictionaryDirectories: List[String] =
    List(TestTemp.directory("serenity-no-os-dictionaries").toString)

  private def resourceText(path: String): Option[String] =
    Option(getClass.getResourceAsStream(path)).map { stream =>
      val source = Source.fromInputStream(stream, "UTF-8")
      try source.mkString
      finally source.close()
    }

  private def snapshot(
    config: SpellCheckConfig,
    cache: DictionaryCache = DictionaryCache(),
    osDictionaryDirectories: List[String] = emptyOsDictionaryDirectories
  ): DictionarySnapshot =
    DictionaryLoader.loadSnapshot(config, cache, osDictionaryDirectories)

  private def flaggedWords(text: String, config: SpellCheckConfig, dictionary: DictionarySnapshot): List[String] =
    SpellChecker.analyzeText(text, config, dictionary.context).map(_.message.stripPrefix("Possible spelling issue: "))

  private def userDictionary(fileName: String, words: List[String]): Path =
    val directory = TestTemp.directory("serenity-user-dictionary")
    val dic       = directory.resolve(fileName)
    Files.writeString(dic, (words.length.toString :: words).mkString("\n"), StandardCharsets.UTF_8)
    dic

  "The bundled en_GB dictionary" should "ship as a dictionary pair beside the upstream README on the classpath" in {
    resourceText("/spellcheck/en_GB.dic").map(_.length).getOrElse(0) should be > 1_000_000
    resourceText("/spellcheck/en_GB.aff").exists(_.contains("SET UTF-8")) shouldBe true
    resourceText("/spellcheck/README_en_GB.txt").exists(_.contains("LGPL")) shouldBe true
  }

  it should "accept British spellings with no dictionary installed or configured" in {
    val dictionary = snapshot(britishEnglish)

    flaggedWords("colour organise neighbourhood favourite grey", britishEnglish, dictionary) shouldBe Nil
  }

  it should "accept affixed forms of its words" in {
    val dictionary = snapshot(britishEnglish)

    flaggedWords("colours coloured organised organising behavioural", britishEnglish, dictionary) shouldBe Nil
  }

  it should "flag a misspelling" in {
    val dictionary = snapshot(britishEnglish)

    flaggedWords("colour wurld recieve organise", britishEnglish, dictionary) shouldBe List("wurld", "recieve")
  }

  it should "load without reporting any affix directive as unsupported" in {
    snapshot(britishEnglish).context.failures shouldBe Nil
  }

  it should "also answer to the underscore spelling and any casing of its language tag" in {
    val config = SpellCheckConfig(enabled = true, languages = List("EN_gb"))

    flaggedWords("colour wurld", config, snapshot(config)) shouldBe List("wurld")
  }

  it should "stay unparsed while spell check is disabled" in {
    val cache  = DictionaryCache()
    val config = britishEnglish.copy(enabled = false)

    snapshot(config, cache).context.words shouldBe empty
    cache.size shouldBe 0
  }

  it should "stay unparsed when no British English language is configured" in {
    val cache  = DictionaryCache()
    val config = SpellCheckConfig(enabled = true, languages = List("en-US", "fr"))

    snapshot(config, cache)

    cache.size shouldBe 0
  }

  it should "be parsed once and shared by every later load" in {
    val cache = DictionaryCache()

    val first  = snapshot(britishEnglish, cache)
    val second = snapshot(britishEnglish, cache)

    cache.size shouldBe 1
    second should be theSameInstanceAs first
  }

  it should "be dropped from the cache once the language leaves the config" in {
    val cache = DictionaryCache()

    snapshot(britishEnglish, cache)
    cache.size shouldBe 1
    snapshot(britishEnglish.copy(languages = List("en-US")), cache)

    cache.size shouldBe 0
  }

  it should "merge additional words from the config" in {
    val config = britishEnglish.copy(additionalWords = List("serenity"))

    flaggedWords("serenity colour wurld", config, snapshot(config)) shouldBe List("wurld")
  }

  "A user-supplied dictionary" should "take precedence over the bundled one when configured by path" in {
    val cache      = DictionaryCache()
    val dic        = userDictionary("en_GB.dic", List("zzcolour"))
    val config     = britishEnglish.copy(dictionaryPaths = List(dic.toString))
    val dictionary = snapshot(config, cache)

    flaggedWords("zzcolour colour", config, dictionary) shouldBe List("colour")
    cache.size shouldBe 1
    cache.entryCount(dic) shouldBe 1
  }

  it should "take precedence over the bundled one when found in an OS dictionary directory" in {
    val cache      = DictionaryCache()
    val dic        = userDictionary("en_GB.dic", List("zzcolour"))
    val dictionary = snapshot(britishEnglish, cache, List(dic.getParent.toString))

    flaggedWords("zzcolour colour", britishEnglish, dictionary) shouldBe List("colour")
    cache.size shouldBe 1
    cache.entryCount(dic) shouldBe 1
  }

  it should "leave the bundled dictionary in place when it covers a different language" in {
    val cache  = DictionaryCache()
    val dic    = userDictionary("fr.dic", List("bonjour"))
    val config = SpellCheckConfig(enabled = true, languages = List("en-gb", "fr"), dictionaryPaths = List(dic.toString))
    val dictionary = snapshot(config, cache)

    flaggedWords("bonjour colour wurld", config, dictionary) shouldBe List("wurld")
    cache.size shouldBe 2
  }

  it should "bring the bundled dictionary back when it is removed from the config" in {
    val cache  = DictionaryCache()
    val dic    = userDictionary("en_GB.dic", List("zzcolour"))
    val config = britishEnglish.copy(dictionaryPaths = List(dic.toString))

    snapshot(config, cache)
    val afterwards = snapshot(britishEnglish, cache)

    flaggedWords("colour zzcolour", britishEnglish, afterwards) shouldBe List("zzcolour")
    cache.entryCount(dic) shouldBe 0
  }
