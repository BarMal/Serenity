package com.serenity

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.attribute.FileTime

import com.serenity.config.{AppConfig, SpellCheckConfig}
import com.serenity.rope.{Balance, Rope}
import com.serenity.spellcheck.{
  CompoundCandidateIndex,
  DictionaryCache,
  DictionaryContext,
  DictionaryLoader,
  SpellChecker
}
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
    // (`DictionaryLoader.loadSnapshot`) that pure analysis never performs itself.
    val config = SpellCheckConfig(enabled = true)
    val dictionary =
      DictionaryContext(words = Set("hand", "built"), replacements = Map.empty, failures = Nil)

    val diagnostics = SpellChecker.analyzeText("hand built wurld", config, dictionary)

    diagnostics.map(_.message) shouldBe List("Possible spelling issue: wurld")
  }

  it should "surface dictionary load failures from a precomputed context without re-reading the filesystem" in {
    val config = SpellCheckConfig(enabled = true)
    val dictionary =
      DictionaryContext(words = Set.empty, replacements = Map.empty, failures = List("boom"))

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
    // Scoped to this one dictionary's own cache entry (`DictionaryCache.entryCount`), not `DictionaryCache`'s total
    // size: `DictionaryCache` is a single process-wide map every spec exercising `DictionaryLoader.loadSnapshot`/`check`
    // shares, and sbt/ScalaTest run different suites concurrently in the same JVM by default -- a size-based
    // assertion would spuriously fail whenever an unrelated, concurrently-running suite's own (different-path)
    // dictionary load happened to land its own cache entry inside this test's window.
    val dictionary = writeDic("serenity-bounded-cache", List("hello"))
    val config     = SpellCheckConfig(enabled = true, dictionaryPaths = List(dictionary.toString))

    DictionaryCache.entryCount(dictionary) shouldBe 0
    DictionaryLoader.loadSnapshot(config)
    DictionaryCache.entryCount(dictionary) shouldBe 1

    (1 to 5).foreach { revision =>
      Files.writeString(dictionary, s"1\nrevision$revision", StandardCharsets.UTF_8)
      Files.setLastModifiedTime(dictionary, FileTime.fromMillis(System.currentTimeMillis() + revision * 10_000L))
      DictionaryLoader.loadSnapshot(config)
      DictionaryCache.entryCount(dictionary) shouldBe 1
    }
  }

  it should "pick up a dictionary's latest content after repeated edits despite the bounded cache" in {
    val dictionary = writeDic("serenity-bounded-cache-content", List("hello"))
    val config     = SpellCheckConfig(enabled = true, dictionaryPaths = List(dictionary.toString))

    DictionaryLoader.loadSnapshot(config)

    Files.writeString(dictionary, "1\nlatest", StandardCharsets.UTF_8)
    Files.setLastModifiedTime(dictionary, FileTime.fromMillis(System.currentTimeMillis() + 10_000L))

    val diagnostics =
      SpellChecker.analyzeText("latest hello", config, DictionaryLoader.loadSnapshot(config).context)

    diagnostics.map(_.message) shouldBe List("Possible spelling issue: hello")
  }

  "SpellChecker Hunspell affix support" should "explicitly report unsupported affix directives instead of silently ignoring them" in {
    // COMPOUNDSYLLABLE/SYLLABLENUM (issue #1198) remain unimplemented -- both are Hungarian-specific
    // syllable-counting compounding limits needing a per-language vowel-counting heuristic no real fixture surveyed
    // for #1198 exercises with enough confidence to implement; see the PR description for the full reasoning. Every
    // other CHECKCOMPOUND*/SIMPLIFIEDTRIPLE/CHECKCOMPOUNDPATTERN/ONLYINCOMPOUND directive is implemented (below).
    val (dictionary, _) = writeHunspellDictionary(
      "serenity-unsupported-affix",
      List("hello/A"),
      List(
        "SET UTF-8",
        "COMPOUNDSYLLABLE 6 aeiou",
        "SYLLABLENUM I"
      )
    )
    val config = SpellCheckConfig(enabled = true, dictionaryPaths = List(dictionary.toString))

    val diagnostics = SpellChecker.check("hello wurld", config)

    diagnostics.map(_.code) should contain(Some("dictionary-load-failed"))
    val failureMessages = diagnostics.filter(_.code.contains("dictionary-load-failed")).map(_.message)
    failureMessages.exists(_.contains("COMPOUNDSYLLABLE")) shouldBe true
    failureMessages.exists(_.contains("SYLLABLENUM")) shouldBe true
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

  it should "not report COMPOUNDFLAG/COMPOUNDBEGIN/COMPOUNDMIDDLE/COMPOUNDEND/COMPOUNDWORDMAX as unsupported" in {
    val (dictionary, _) = writeHunspellDictionary(
      "serenity-supported-compoundflag-directives",
      List("un/U", "der/W", "stand/V"),
      List(
        "SET UTF-8",
        "COMPOUNDMIN 2",
        "COMPOUNDBEGIN U",
        "COMPOUNDMIDDLE V",
        "COMPOUNDEND W",
        "COMPOUNDWORDMAX 3"
      )
    )
    val config = SpellCheckConfig(enabled = true, dictionaryPaths = List(dictionary.toString))

    val diagnostics = SpellChecker.check("understand", config)

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

  // Real-shaped fixture verified verbatim against hunspell's own tests/compoundrule2.{aff,dic,good,wrong} (issue
  // #1187): `A*B*C*` matches zero-or-more flag-A words, then zero-or-more flag-B words, then zero-or-more flag-C
  // words, in that order -- confirming this project's COMPOUNDRULE grammar (star/question-mark quantifiers over a
  // single compound flag) against the upstream reference implementation's own primary-source test corpus.
  it should "accept a word matching a starred COMPOUNDRULE flag sequence and reject one violating the flag order" in {
    val (dictionary, _) = writeHunspellDictionary(
      "serenity-compoundrule-star-order",
      List("a/A", "b/B", "c/C"),
      List(
        "SET UTF-8",
        "COMPOUNDMIN 1",
        "COMPOUNDRULE 1",
        "COMPOUNDRULE A*B*C*"
      )
    )
    val config = SpellCheckConfig(enabled = true, dictionaryPaths = List(dictionary.toString))

    // "abc" and "aac" are in hunspell's own compoundrule2.good; "cba" (reversed flag order) is not generated by
    // any A*B*C* derivation and is absent from compoundrule2.good.
    val diagnostics = SpellChecker.check("abc aac cba", config)

    diagnostics.map(_.message) shouldBe List("Possible spelling issue: cba")
  }

  // Issue #1445: `DictionaryLoader.loadSnapshot` must build `compoundCandidateIndex` once, at load time, from the same
  // `compoundWordFlags` it merges -- not leave `HunspellCompoundMatcher.matches` to rebuild it on every check. This
  // asserts the wiring directly against the loader's own output rather than only through `SpellChecker.check`'s
  // accept/reject behaviour, which would pass even if the index were (again) rebuilt per call.
  it should "build compoundCandidateIndex once at dictionary load, from the same compoundWordFlags it merges" in {
    val (dictionary, _) = writeHunspellDictionary(
      "serenity-compoundrule-candidate-index",
      List("a/A", "b/B", "c/C"),
      List(
        "SET UTF-8",
        "COMPOUNDMIN 1",
        "COMPOUNDRULE 1",
        "COMPOUNDRULE A*B*C*"
      )
    )
    val config = SpellCheckConfig(enabled = true, dictionaryPaths = List(dictionary.toString))

    val context = DictionaryLoader.loadSnapshot(config).context

    context.compoundCandidateIndex shouldBe CompoundCandidateIndex.build(context.compoundWordFlags)
    context.compoundCandidateIndex.buckets should not be empty
  }

  it should "leave compoundCandidateIndex empty when no dictionary declares COMPOUNDRULE" in {
    val config = SpellCheckConfig(enabled = true)

    val context = DictionaryLoader.loadSnapshot(config).context

    context.compoundCandidateIndex shouldBe CompoundCandidateIndex.empty
  }

  // Real-shaped fixture: German-style noun compounding via "zero-or-more flag-A words followed by one flag-B
  // word" -- the grammar called out by name in issue #1187 -- with COMPOUNDMIN left at its hunspell(5) default of
  // 3 letters so the too-short "zu" cannot itself serve as a compound member.
  it should "recognize German-style noun compounds via COMPOUNDRULE and reject compounds in the wrong flag order" in {
    val (dictionary, _) = writeHunspellDictionary(
      "serenity-compoundrule-german",
      List("garten/A", "haus/A", "tuer/B", "zu/A"),
      List(
        "SET UTF-8",
        "COMPOUNDRULE 1",
        "COMPOUNDRULE A*B"
      )
    )
    val config = SpellCheckConfig(enabled = true, dictionaryPaths = List(dictionary.toString))

    val diagnostics = SpellChecker.check("gartentuer haustuer tuerhaus zutuer", config)

    // gartentuer/haustuer: one-or-more flag-A word(s) then the mandatory flag-B word -- valid.
    // tuerhaus: flag-B word before the flag-A word -- violates the required order.
    // zutuer: "zu" (2 letters) is below the default COMPOUNDMIN of 3, so it cannot stand as a compound member.
    diagnostics.map(_.message) shouldBe List(
      "Possible spelling issue: tuerhaus",
      "Possible spelling issue: zutuer"
    )
  }

  // Real-shaped fixture verified verbatim against hunspell's own tests/circumfix.{aff,dic,good,wrong} (issue
  // #1187): the canonical Hungarian superlative, where "leg-" (or "legesleg-" for the double superlative) may
  // only combine with "-obb" as a pair -- CIRCUMFIX rejects the prefix or suffix appearing alone.
  it should "accept a Hungarian superlative circumfix pairing and reject the bare unpaired prefix" in {
    val (dictionary, _) = writeHunspellDictionary(
      "serenity-circumfix-hungarian-superlative",
      List("nagy/C"),
      List(
        "SET UTF-8",
        "CIRCUMFIX X",
        "PFX A Y 1",
        "PFX A 0 leg/X .",
        "PFX B Y 1",
        "PFX B 0 legesleg/X .",
        "SFX C Y 3",
        "SFX C 0 obb .",
        "SFX C 0 obb/AX .",
        "SFX C 0 obb/BX ."
      )
    )
    val config = SpellCheckConfig(enabled = true, dictionaryPaths = List(dictionary.toString))

    val diagnostics = SpellChecker.check("nagy nagyobb legnagyobb legeslegnagyobb legnagy legeslegnagy", config)

    diagnostics.map(_.message) shouldBe List(
      "Possible spelling issue: legnagy",
      "Possible spelling issue: legeslegnagy"
    )
  }

  // Real-shaped fixture verified verbatim against hunspell's own tests/compoundflag.{aff,dic,good,wrong} (issue
  // #1198): COMPOUNDMIN 3, COMPOUNDFLAG A, dictionary foo/A bar/A xy/A yz/A. "foobar"/"barfoo"/"foobarfoo" are in
  // compoundflag.good (free-form: any order, any count of 2+); "fooxy" is in compoundflag.wrong because "xy" is
  // below COMPOUNDMIN.
  "SpellChecker free-form COMPOUNDFLAG support" should
    "accept two- and three-word free-form compounds in any order and reject a COMPOUNDMIN-violating segment" in {
      val (dictionary, _) = writeHunspellDictionary(
        "serenity-compoundflag-hunspell-corpus",
        List("foo/A", "bar/A", "xy/A", "yz/A"),
        List(
          "SET UTF-8",
          "COMPOUNDMIN 3",
          "COMPOUNDFLAG A"
        )
      )
      val config = SpellCheckConfig(enabled = true, dictionaryPaths = List(dictionary.toString))

      val diagnostics = SpellChecker.check("foobar barfoo foobarfoo fooxy", config)

      diagnostics.map(_.message) shouldBe List("Possible spelling issue: fooxy")
    }

  it should "reject a single compound-flagged dictionary word -- COMPOUNDFLAG needs two or more members" in {
    val (dictionary, _) = writeHunspellDictionary(
      "serenity-compoundflag-single-word",
      List("foo/A", "bar/A"),
      List("SET UTF-8", "COMPOUNDMIN 3", "COMPOUNDFLAG A")
    )
    val config = SpellCheckConfig(enabled = true, dictionaryPaths = List(dictionary.toString))

    val diagnostics = SpellChecker.check("foo bar foobar", config)

    diagnostics shouldBe Nil
  }

  // Real-word fixture: Norwegian "sol" (sun) + "skinn" (shine/skin) -> "solskinn" (sunshine), a genuine Norwegian
  // compound -- the free-form COMPOUNDFLAG mechanism this PR implements is well-attested in Norwegian .aff files
  // per #1198's investigation.
  it should "accept a real Norwegian compound word formed from two compound-flagged roots" in {
    val (dictionary, _) = writeHunspellDictionary(
      "serenity-compoundflag-norwegian",
      List("sol/A", "skinn/A"),
      List("SET UTF-8", "COMPOUNDMIN 3", "COMPOUNDFLAG A")
    )
    val config = SpellCheckConfig(enabled = true, dictionaryPaths = List(dictionary.toString))

    val diagnostics = SpellChecker.check("solskinn", config)

    diagnostics shouldBe Nil
  }

  it should "enforce COMPOUNDWORDMAX, rejecting a compound with more segments than the declared maximum" in {
    val (dictionary, _) = writeHunspellDictionary(
      "serenity-compoundflag-wordmax",
      List("aa/A", "bb/A", "cc/A", "dd/A"),
      List("SET UTF-8", "COMPOUNDMIN 2", "COMPOUNDFLAG A", "COMPOUNDWORDMAX 2")
    )
    val config = SpellCheckConfig(enabled = true, dictionaryPaths = List(dictionary.toString))

    val diagnostics = SpellChecker.check("aabb aabbcc", config)

    // "aabb" (2 members) is within COMPOUNDWORDMAX 2; "aabbcc" would need 3 members, exceeding it.
    diagnostics.map(_.message) shouldBe List("Possible spelling issue: aabbcc")
  }

  // Real syntax verified against hunspell's own tests/germancompounding.aff (`COMPOUNDBEGIN`/`COMPOUNDMIDDLE`/
  // `COMPOUNDEND`, one flag letter each).
  it should "enforce COMPOUNDBEGIN/COMPOUNDMIDDLE/COMPOUNDEND positional roles, rejecting a word in the wrong position" in {
    val (dictionary, _) = writeHunspellDictionary(
      "serenity-compoundflag-positional",
      List("un/U", "der/W", "stand/V"),
      List(
        "SET UTF-8",
        "COMPOUNDMIN 2",
        "COMPOUNDBEGIN U",
        "COMPOUNDMIDDLE V",
        "COMPOUNDEND W"
      )
    )
    val config = SpellCheckConfig(enabled = true, dictionaryPaths = List(dictionary.toString))

    val diagnostics = SpellChecker.check("unstandder derstandun", config)

    // "unstandder": begin-flagged "un" + middle-flagged "stand" + end-flagged "der" -- valid role order.
    // "derstandun": end-flagged "der" leading and begin-flagged "un" trailing -- both roles violated.
    diagnostics.map(_.message) shouldBe List("Possible spelling issue: derstandun")
  }

  // Confirms neither compounding mechanism masks the other's rejection when a dictionary declares both COMPOUNDRULE
  // and free-form COMPOUNDFLAG (issue #1198: real Croatian/Persian .aff files do this).
  it should "accept compounds via either COMPOUNDRULE or COMPOUNDFLAG when a dictionary declares both, without either masking the other's rejection" in {
    val (dictionary, _) = writeHunspellDictionary(
      "serenity-compoundrule-and-compoundflag-coexist",
      List("garten/R", "haus/R", "foo/C", "bar/C"),
      List(
        "SET UTF-8",
        "COMPOUNDMIN 3",
        "COMPOUNDRULE 1",
        "COMPOUNDRULE R*",
        "COMPOUNDFLAG C"
      )
    )
    val config = SpellCheckConfig(enabled = true, dictionaryPaths = List(dictionary.toString))

    val diagnostics = SpellChecker.check("gartenhaus foobar bargarten", config)

    // gartenhaus: matches only COMPOUNDRULE (R*, flag R members). foobar: matches only COMPOUNDFLAG (flag C
    // members). bargarten mixes a COMPOUNDFLAG-only word ("bar", flag C) with a COMPOUNDRULE-only word ("garten",
    // flag R) -- neither mechanism alone can segment it, so it must still be rejected.
    diagnostics.map(_.message) shouldBe List("Possible spelling issue: bargarten")
  }

  // CHECKCOMPOUND*/SIMPLIFIEDTRIPLE/CHECKCOMPOUNDPATTERN/ONLYINCOMPOUND (issue #1198, PR 2 of 2): end-to-end
  // coverage layered on top of the free-form COMPOUNDFLAG mechanism above, confirming these directives are parsed
  // from a real `.aff` file, no longer reported as unsupported, and actually reach `SpellChecker.check`.

  // Verified against hunspell's own tests/checkcompoundcase.{aff,dic,wrong}.
  "SpellChecker CHECKCOMPOUNDCASE support" should "reject an upper-case letter at a compound boundary" in {
    val (dictionary, _) = writeHunspellDictionary(
      "serenity-checkcompoundcase",
      List("foo/A", "Bar/A"),
      List("SET UTF-8", "COMPOUNDMIN 1", "COMPOUNDFLAG A", "CHECKCOMPOUNDCASE")
    )
    val config = SpellCheckConfig(enabled = true, dictionaryPaths = List(dictionary.toString))

    val diagnostics = SpellChecker.check("fooBar", config)

    diagnostics.map(_.code) should not contain Some("dictionary-load-failed")
    diagnostics.map(_.message) shouldBe List("Possible spelling issue: fooBar")
  }

  "SpellChecker CHECKCOMPOUNDDUP support" should "reject a compound made of the same word repeated" in {
    val (dictionary, _) = writeHunspellDictionary(
      "serenity-checkcompounddup",
      List("foo/A", "bar/A"),
      List("SET UTF-8", "COMPOUNDMIN 1", "COMPOUNDFLAG A", "CHECKCOMPOUNDDUP")
    )
    val config = SpellCheckConfig(enabled = true, dictionaryPaths = List(dictionary.toString))

    val diagnostics = SpellChecker.check("foofoo foobar", config)

    diagnostics.map(_.message) shouldBe List("Possible spelling issue: foofoo")
  }

  // Verified against the mechanism of hunspell's own tests/checkcompoundrep.{aff,dic,good,wrong} (Hungarian
  // "szerviz" example), reusing the existing REP table support from issue #1188.
  "SpellChecker CHECKCOMPOUNDREP support" should
    "reject a compound whose REP-substituted spelling matches a standalone dictionary word" in {
      val (dictionary, _) = writeHunspellDictionary(
        "serenity-checkcompoundrep",
        List("szer/A", "wiz/A", "szerviz"),
        List("SET UTF-8", "COMPOUNDMIN 1", "COMPOUNDFLAG A", "CHECKCOMPOUNDREP", "REP 1", "REP w v")
      )
      val config = SpellCheckConfig(enabled = true, dictionaryPaths = List(dictionary.toString))

      val diagnostics = SpellChecker.check("szerwiz", config)

      diagnostics.map(_.message) shouldBe List("Possible spelling issue: szerwiz")
    }

  // Verified verbatim against hunspell's own tests/checkcompoundtriple.{aff,dic,good,wrong}.
  "SpellChecker CHECKCOMPOUNDTRIPLE support" should "reject a triple-letter run spanning the compound boundary" in {
    val (dictionary, _) = writeHunspellDictionary(
      "serenity-checkcompoundtriple",
      List("foo/A", "opera/A"),
      List("SET UTF-8", "COMPOUNDMIN 1", "COMPOUNDFLAG A", "CHECKCOMPOUNDTRIPLE")
    )
    val config = SpellCheckConfig(enabled = true, dictionaryPaths = List(dictionary.toString))

    val diagnostics = SpellChecker.check("fooopera operafoo", config)

    diagnostics.map(_.message) shouldBe List("Possible spelling issue: fooopera")
  }

  // Verified verbatim against hunspell's own tests/simplifiedtriple.{aff,dic,good,wrong}: "glasssko" (unreduced) is
  // forbidden, "glassko" (one letter elided) is accepted.
  "SpellChecker SIMPLIFIEDTRIPLE support" should
    "accept the simplified spelling of a triple-letter compound boundary while still rejecting the literal one" in {
      val (dictionary, _) = writeHunspellDictionary(
        "serenity-simplifiedtriple",
        List("glass/A", "sko/A"),
        List("SET UTF-8", "COMPOUNDMIN 2", "COMPOUNDFLAG A", "CHECKCOMPOUNDTRIPLE", "SIMPLIFIEDTRIPLE")
      )
      val config = SpellCheckConfig(enabled = true, dictionaryPaths = List(dictionary.toString))

      val diagnostics = SpellChecker.check("glasssko glassko", config)

      diagnostics.map(_.message) shouldBe List("Possible spelling issue: glasssko")
    }

  // Verified verbatim against hunspell's own tests/checkcompoundpattern2.{aff,dic,good,wrong}: "CHECKCOMPOUNDPATTERN
  // o b z" forbids the literal "foobar" but accepts its replacement-elided spelling "fozar".
  "SpellChecker CHECKCOMPOUNDPATTERN support" should
    "reject a literal forbidden compound boundary while accepting its replacement-elided spelling" in {
      val (dictionary, _) = writeHunspellDictionary(
        "serenity-checkcompoundpattern",
        List("foo/A", "bar/A"),
        List(
          "SET UTF-8",
          "COMPOUNDMIN 1",
          "COMPOUNDFLAG A",
          "CHECKCOMPOUNDPATTERN 1",
          "CHECKCOMPOUNDPATTERN o b z"
        )
      )
      val config = SpellCheckConfig(enabled = true, dictionaryPaths = List(dictionary.toString))

      val diagnostics = SpellChecker.check("foobar fozar", config)

      diagnostics.map(_.message) shouldBe List("Possible spelling issue: foobar")
    }

  // Verified verbatim against hunspell's own tests/checkcompoundpattern3.{aff,dic,good,wrong}: flag-conditioned
  // "CHECKCOMPOUNDPATTERN o/X b/Y z" forbids "booban" but not "boobar" (bar lacks the required Y flag).
  it should "apply a flag-conditioned CHECKCOMPOUNDPATTERN rule only when both flags are present" in {
    val (dictionary, _) = writeHunspellDictionary(
      "serenity-checkcompoundpattern-flags",
      List("boo/AX", "bar/A", "ban/AY"),
      List(
        "SET UTF-8",
        "COMPOUNDMIN 1",
        "COMPOUNDFLAG A",
        "CHECKCOMPOUNDPATTERN 1",
        "CHECKCOMPOUNDPATTERN o/X b/Y z"
      )
    )
    val config = SpellCheckConfig(enabled = true, dictionaryPaths = List(dictionary.toString))

    val diagnostics = SpellChecker.check("booban boobar bozan", config)

    diagnostics.map(_.message) shouldBe List("Possible spelling issue: booban")
  }

  // Verified verbatim against hunspell's own tests/onlyincompound.{aff,dic,good,wrong}: a root flagged
  // ONLYINCOMPOUND is compound-member-only -- its bare form and its own affixed forms are rejected standalone, but
  // it (and forms built from it) may still appear as a compound member.
  "SpellChecker ONLYINCOMPOUND support" should
    "reject the bare and affixed forms of an ONLYINCOMPOUND-flagged root standalone while allowing it as a compound member" in {
      val (dictionary, _) = writeHunspellDictionary(
        "serenity-onlyincompound",
        List("foo/A", "pseudo/OAB"),
        List(
          "SET UTF-8",
          "COMPOUNDMIN 1",
          "COMPOUNDFLAG A",
          "ONLYINCOMPOUND O",
          "SFX B Y 1",
          "SFX B 0 s ."
        )
      )
      val config = SpellCheckConfig(enabled = true, dictionaryPaths = List(dictionary.toString))

      val diagnostics = SpellChecker.check("pseudo pseudos pseudofoo foopseudo", config)

      diagnostics.map(_.message) shouldBe List(
        "Possible spelling issue: pseudo",
        "Possible spelling issue: pseudos"
      )
    }
end SpellCheckerDictionaryIoSpec
