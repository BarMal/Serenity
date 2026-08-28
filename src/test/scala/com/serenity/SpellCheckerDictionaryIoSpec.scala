package com.serenity

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.attribute.FileTime

import com.serenity.config.{AppConfig, SpellCheckConfig}
import com.serenity.rope.{Balance, Rope}
import com.serenity.spellcheck.SpellChecker
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Covers the split between pure spell-check analysis and explicit dictionary discovery/loading IO (issue #860):
  * `analyzeText`/`analysisFingerprints`/`applyIfCurrent` take already-loaded, immutable dictionary data and never touch
  * the filesystem themselves; the bounded dictionary cache; and the handwritten Hunspell affix parser's explicit
  * rejection of directives it does not implement.
  */
class SpellCheckerDictionaryIoSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private def writeDic(name: String, words: List[String]): java.nio.file.Path =
    val path = Files.createTempFile(name, ".dic")
    Files.writeString(path, (words.length.toString :: words).mkString("\n"), StandardCharsets.UTF_8)
    path

  private def writeHunspellDictionary(
    name: String,
    words: List[String],
    affixRules: List[String]
  ): (java.nio.file.Path, java.nio.file.Path) =
    val directory = Files.createTempDirectory(name)
    val dic       = directory.resolve(s"$name.dic")
    val aff       = directory.resolve(s"$name.aff")
    Files.writeString(dic, (words.length.toString :: words).mkString("\n"), StandardCharsets.UTF_8)
    Files.writeString(aff, affixRules.mkString("\n"), StandardCharsets.UTF_8)
    dic -> aff

  "SpellChecker.analyzeText" should "analyze text against an already-loaded dictionary with no dictionary paths configured" in {
    // `analyzeText` takes a `DictionaryContext` rather than a `SpellCheckConfig` path list, so there is nothing in
    // its signature capable of reaching the filesystem: discovery/loading is a separate, explicit step
    // (`loadDictionarySnapshot`) that pure analysis never performs itself.
    val config = SpellCheckConfig(enabled = true)
    val dictionary =
      SpellChecker.DictionaryContext(words = Set("hand", "built"), replacements = Map.empty, failures = Nil)

    val diagnostics = SpellChecker.analyzeText("hand built wurld", config, dictionary)

    diagnostics.map(_.message) shouldBe List("Possible spelling issue: wurld")
  }

  it should "surface dictionary load failures from a precomputed context without re-reading the filesystem" in {
    val config = SpellCheckConfig(enabled = true)
    val dictionary =
      SpellChecker.DictionaryContext(words = Set.empty, replacements = Map.empty, failures = List("boom"))

    val diagnostics = SpellChecker.analyzeText("hi", config, dictionary)

    diagnostics.map(_.code) shouldBe List(Some("dictionary-load-failed"))
  }

  "SpellChecker.analysisFingerprints and applyIfCurrent" should "accept precomputed dictionary fingerprints instead of reading the filesystem themselves" in {
    // Neither method takes a `SpellCheckConfig`'s raw dictionary paths without also being handed the fingerprints
    // for them, so a caller cannot invoke either from inside `Ref.update` and have it silently touch disk.
    val config     = SpellCheckConfig(enabled = true)
    val bufferId   = BufferId(0)
    val baseBuffer = AppState.initial.persisted.buffers(bufferId)
    val buffer     = baseBuffer.copy(document = baseBuffer.document.copy(content = Rope("hello")))
    val state = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        config = AppConfig.default.withSpellCheck(config),
        buffers = Map(bufferId -> buffer)
      )
    )

    val fingerprints = SpellChecker.analysisFingerprints(state, dictionaryFingerprints = Nil)

    fingerprints.values.flatMap(_.dictionaryFingerprints).toList shouldBe Nil
  }

  "SpellChecker dictionary cache" should "hold at most one entry per normalized dictionary path across repeated edits" in {
    val dictionary = writeDic("serenity-bounded-cache", List("hello"))
    val config     = SpellCheckConfig(enabled = true, dictionaryPaths = List(dictionary.toString))

    val before = SpellChecker.dictionaryCacheSize
    SpellChecker.loadDictionarySnapshot(config)
    val afterFirstLoad = SpellChecker.dictionaryCacheSize

    (1 to 5).foreach { revision =>
      Files.writeString(dictionary, s"1\nrevision$revision", StandardCharsets.UTF_8)
      Files.setLastModifiedTime(dictionary, FileTime.fromMillis(System.currentTimeMillis() + revision * 10_000L))
      SpellChecker.loadDictionarySnapshot(config)
    }

    afterFirstLoad shouldBe before + 1
    SpellChecker.dictionaryCacheSize shouldBe afterFirstLoad
  }

  it should "pick up a dictionary's latest content after repeated edits despite the bounded cache" in {
    val dictionary = writeDic("serenity-bounded-cache-content", List("hello"))
    val config     = SpellCheckConfig(enabled = true, dictionaryPaths = List(dictionary.toString))

    SpellChecker.loadDictionarySnapshot(config)

    Files.writeString(dictionary, "1\nlatest", StandardCharsets.UTF_8)
    Files.setLastModifiedTime(dictionary, FileTime.fromMillis(System.currentTimeMillis() + 10_000L))

    val diagnostics =
      SpellChecker.analyzeText("latest hello", config, SpellChecker.loadDictionarySnapshot(config).context)

    diagnostics.map(_.message) shouldBe List("Possible spelling issue: hello")
  }

  "SpellChecker Hunspell affix support" should "explicitly report unsupported affix directives instead of silently ignoring them" in {
    // COMPOUNDFLAG and CIRCUMFIX (issue #1182) remain unimplemented -- compounding is deferred to a follow-up
    // issue, see the PR description for the real-dictionary evidence behind that call.
    val (dictionary, _) = writeHunspellDictionary(
      "serenity-unsupported-affix",
      List("hello/A"),
      List(
        "SET UTF-8",
        "COMPOUNDFLAG A",
        "CIRCUMFIX A"
      )
    )
    val config = SpellCheckConfig(enabled = true, dictionaryPaths = List(dictionary.toString))

    val diagnostics = SpellChecker.check("hello wurld", config)

    diagnostics.map(_.code) should contain(Some("dictionary-load-failed"))
    val failureMessages = diagnostics.filter(_.code.contains("dictionary-load-failed")).map(_.message)
    failureMessages.exists(_.contains("COMPOUNDFLAG")) shouldBe true
    failureMessages.exists(_.contains("CIRCUMFIX")) shouldBe true
    diagnostics.map(_.message) should contain("Possible spelling issue: wurld")
  }

  it should "not report supported directives as unsupported" in {
    val (dictionary, _) = writeHunspellDictionary(
      "serenity-supported-affix",
      List("draft/G"),
      List("SET UTF-8", "SFX G Y 1", "SFX G 0 ing .")
    )
    val config = SpellCheckConfig(enabled = true, dictionaryPaths = List(dictionary.toString))

    val diagnostics = SpellChecker.check("draft drafting", config)

    diagnostics.map(_.code) should not contain Some("dictionary-load-failed")
  }

  // Real-shaped fixture: en_US.aff / fr_FR.aff and friends (SubtitleEdit, LibreOffice dictionaries) declare
  // exactly this ICONV pair to normalize the "fi" ligature typed by many PDF/typeset copy-paste sources into
  // plain ASCII before matching against the dictionary, and OCONV to convert a plain apostrophe in generated
  // suggestions into a typographic one.
  it should "apply ICONV input conversion so ligature variants of a dictionary word are recognized" in {
    val (dictionary, _) = writeHunspellDictionary(
      "serenity-iconv-ligature",
      List("file"),
      List(
        "SET UTF-8",
        "ICONV 1",
        "ICONV ﬁ fi"
      )
    )
    val config = SpellCheckConfig(enabled = true, dictionaryPaths = List(dictionary.toString))

    // "ﬁle" is the ligature-typed variant of "file" (fi-ligature + "le").
    val diagnostics = SpellChecker.check("ﬁle wurld", config)

    diagnostics.map(_.code) should not contain Some("dictionary-load-failed")
    diagnostics.map(_.message) shouldBe List("Possible spelling issue: wurld")
  }

  it should "apply OCONV output conversion to REP-based suggestions" in {
    val (dictionary, _) = writeHunspellDictionary(
      "serenity-oconv-suggestion",
      List("café/A"),
      List(
        "SET UTF-8",
        "OCONV 1",
        "OCONV ' ’",
        "REP 1",
        "REP cafe caf'e"
      )
    )
    val config = SpellCheckConfig(enabled = true, dictionaryPaths = List(dictionary.toString))

    val diagnostics = SpellChecker.check("cafe", config)

    diagnostics.map(_.message) shouldBe List("Possible spelling issue: cafe (suggestion: caf’e)")
  }

  // Real-shaped fixture: NEEDAFFIX marks "draft" (flag X) as a virtual stem -- valid only when affixed, per
  // hunspell(5): "words only valid when affixed". Flag G separately supplies the "-ing" suffix.
  it should "reject the bare form of a NEEDAFFIX-flagged root while accepting its affixed forms" in {
    val (dictionary, _) = writeHunspellDictionary(
      "serenity-needaffix-root-rejected",
      List("draft/XG"),
      List(
        "SET UTF-8",
        "NEEDAFFIX X",
        "SFX G Y 1",
        "SFX G 0 ing ."
      )
    )
    val config = SpellCheckConfig(enabled = true, dictionaryPaths = List(dictionary.toString))

    val diagnostics = SpellChecker.check("draft drafting", config)

    diagnostics.map(_.message) shouldBe List("Possible spelling issue: draft")
  }
end SpellCheckerDictionaryIoSpec
