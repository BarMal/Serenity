package com.serenity

import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.Files
import java.nio.file.attribute.FileTime

import scala.concurrent.duration.*

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.config.{AppConfig, SpellCheckConfig}
import com.serenity.keystroke.events.InsertChar
import com.serenity.rope.{Balance, Leaf, Rope}
import com.serenity.spellcheck.SpellChecker
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class SpellCheckerSpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  // `Rope` is sealed, so a test double can no longer extend it directly; it delegates to a real `Leaf`/`Node` tree
  // while itself extending the still-open `Leaf` purely to satisfy the type system -- every method that matters for
  // this test forwards to `delegate` rather than using anything inherited from `Leaf`.
  final class NonCollectingRope(delegate: Rope) extends Leaf(delegate.collect()):
    override def weight: Int =
      delegate.weight

    override def height: Int =
      delegate.height

    override val newlineCount: Int =
      delegate.newlineCount

    override val lastLineLength: Int =
      delegate.lastLineLength

    override val endsWithNewline: Boolean =
      delegate.endsWithNewline

    override def isWeightBalanced: Boolean =
      delegate.isWeightBalanced

    override def isHeightBalanced: Boolean =
      delegate.isHeightBalanced

    override def rebalance: Rope =
      this

    override def index(i: Int): Option[Char] =
      delegate.index(i)

    override def splitAt(index: Int): Option[(Rope, Rope)] =
      delegate.splitAt(index)

    override def lineCount: Int =
      delegate.lineCount

    override def getLine(lineIndex: Int): Option[String] =
      delegate.getLine(lineIndex)

    override def lineColumnToOffset(line: Int, column: Int): Int =
      delegate.lineColumnToOffset(line, column)

    override def offsetToLineColumn(offset: Int): (Int, Int) =
      delegate.offsetToLineColumn(offset)

    override def collect(): String =
      throw AssertionError("unchanged cached spell-check diagnostics should not materialise content")

  object NonCollectingRope:
    def apply(delegate: Rope): NonCollectingRope = new NonCollectingRope(delegate)

  private def writeDic(name: String, words: List[String]): java.nio.file.Path =
    val path = Files.createTempFile(name, ".dic")
    Files.writeString(path, (words.length.toString :: words).mkString("\n"), StandardCharsets.UTF_8)
    path

  private def writeHunspellDictionary(
    name: String,
    words: List[String],
    affixRules: List[String],
    charset: Charset = StandardCharsets.UTF_8
  ): (java.nio.file.Path, java.nio.file.Path) =
    val directory = Files.createTempDirectory(name)
    val dic       = directory.resolve(s"$name.dic")
    val aff       = directory.resolve(s"$name.aff")
    Files.writeString(dic, (words.length.toString :: words).mkString("\n"), charset)
    Files.writeString(aff, affixRules.mkString("\n"), charset)
    dic -> aff

  "SpellChecker" should "report unknown words with unicode-aware ranges" in {
    val config = SpellCheckConfig(
      enabled = true,
      languages = List("en", "fr"),
      additionalWords = List("κόσμος")
    )

    val diagnostics = SpellChecker.check("hello wurld\ncafé κόσμος", config)

    diagnostics.map(_.message) shouldBe List("Possible spelling issue: wurld")
    diagnostics.head.range.start.line shouldBe 0
    diagnostics.head.range.start.character shouldBe 6
    diagnostics.head.range.end.character shouldBe 11
  }

  it should "return no diagnostics when disabled" in {
    SpellChecker.check("wurld", SpellCheckConfig(enabled = false)) shouldBe Nil
  }

  it should "accept configured French and Greek dictionaries with diacritics and non-Latin letters" in {
    val french = "bonjour caf\u00e9 fran\u00e7ais r\u00e9sum\u00e9"
    val greek  = "\u03ba\u03cc\u03c3\u03bc\u03bf\u03c2 \u03b3\u03b5\u03b9\u03ac"
    val config = SpellCheckConfig(
      enabled = true,
      languages = List("en", "fr", "el")
    )

    val diagnostics = SpellChecker.check(s"$french\n$greek\nwrld", config)

    diagnostics.map(_.message) shouldBe List("Possible spelling issue: wrld")
  }

  it should "keep words joined by a curly apostrophe in one diagnostic range" in {
    val diagnostics = SpellChecker.check("l\u2019amour", SpellCheckConfig(enabled = true))

    diagnostics.map(_.message) shouldBe List("Possible spelling issue: l\u2019amour")
    diagnostics.head.range.start.character shouldBe 0
    diagnostics.head.range.end.character shouldBe 7
  }

  it should "ignore code-like tokens and short words" in {
    val config = SpellCheckConfig(enabled = true)

    SpellChecker.check("id parse_json v2 ok", config) shouldBe Nil
  }

  it should "accept words loaded from configured Hunspell dictionaries" in {
    val dictionary = writeDic("serenity-en", List("external", "serenity/AB", "calm"))
    val config = SpellCheckConfig(
      enabled = true,
      dictionaryPaths = List(dictionary.toString),
      additionalWords = List("drafting")
    )

    val diagnostics = SpellChecker.check("external serenity calm drafting wurld", config)

    diagnostics.map(_.message) shouldBe List("Possible spelling issue: wurld")
  }

  it should "expand Hunspell suffix rules from sibling affix files" in {
    val (dictionary, _) = writeHunspellDictionary(
      "serenity-suffix",
      List("draft/G", "city/S"),
      List(
        "SET UTF-8",
        "SFX G Y 1",
        "SFX G 0 ing .",
        "SFX S Y 1",
        "SFX S y ies [^aeiou]y"
      )
    )
    val config = SpellCheckConfig(enabled = true, dictionaryPaths = List(dictionary.toString))

    val diagnostics = SpellChecker.check("draft drafting city cities citie wurld", config)

    diagnostics.map(_.message) shouldBe List(
      "Possible spelling issue: citie",
      "Possible spelling issue: wurld"
    )
  }

  it should "expand Hunspell prefix rules from sibling affix files" in {
    val (dictionary, _) = writeHunspellDictionary(
      "serenity-prefix",
      List("kind/U", "clear/U"),
      List(
        "SET UTF-8",
        "PFX U Y 1",
        "PFX U 0 un ."
      )
    )
    val config = SpellCheckConfig(enabled = true, dictionaryPaths = List(dictionary.toString))

    val diagnostics = SpellChecker.check("kind unkind clear unclear unklear", config)

    diagnostics.map(_.message) shouldBe List("Possible spelling issue: unklear")
  }

  it should "combine Hunspell prefix and suffix rules when both rules are combinable" in {
    val (dictionary, _) = writeHunspellDictionary(
      "serenity-cross-product",
      List("kind/US"),
      List(
        "SET UTF-8",
        "PFX U Y 1",
        "PFX U 0 un .",
        "SFX S Y 1",
        "SFX S 0 ness ."
      )
    )
    val config = SpellCheckConfig(enabled = true, dictionaryPaths = List(dictionary.toString))

    val diagnostics = SpellChecker.check("kind unkind kindness unkindness unkindish", config)

    diagnostics.map(_.message) shouldBe List("Possible spelling issue: unkindish")
  }

  it should "load a Hunspell dictionary when the configured path points at the affix file" in {
    val (_, affix) = writeHunspellDictionary(
      "serenity-affix-path",
      List("draft/G"),
      List(
        "SET UTF-8",
        "SFX G Y 1",
        "SFX G 0 ing ."
      )
    )
    val config = SpellCheckConfig(enabled = true, dictionaryPaths = List(affix.toString))

    val diagnostics = SpellChecker.check("draft drafting drafter", config)

    diagnostics.map(_.message) shouldBe List("Possible spelling issue: drafter")
  }

  it should "expand Hunspell long and numeric affix flags" in {
    val (longDictionary, _) = writeHunspellDictionary(
      "serenity-long-flags",
      List("kind/AB"),
      List(
        "FLAG long",
        "SFX AB Y 1",
        "SFX AB 0 ness ."
      )
    )
    val (numericDictionary, _) = writeHunspellDictionary(
      "serenity-numeric-flags",
      List("soft/12"),
      List(
        "FLAG num",
        "SFX 12 Y 1",
        "SFX 12 0 ly ."
      )
    )
    val config = SpellCheckConfig(
      enabled = true,
      dictionaryPaths = List(longDictionary.toString, numericDictionary.toString)
    )

    val diagnostics = SpellChecker.check("kind kindness soft softly softless", config)

    diagnostics.map(_.message) shouldBe List("Possible spelling issue: softless")
  }

  it should "expand Hunspell affix flag aliases from AF declarations" in {
    val (dictionary, _) = writeHunspellDictionary(
      "serenity-flag-aliases",
      List("kind/2", "clear/1"),
      List(
        "SET UTF-8",
        "AF 2",
        "AF U",
        "AF US",
        "PFX U Y 1",
        "PFX U 0 un .",
        "SFX S Y 1",
        "SFX S 0 ness ."
      )
    )
    val config = SpellCheckConfig(enabled = true, dictionaryPaths = List(dictionary.toString))

    val diagnostics = SpellChecker.check("kind unkind kindness unkindness clear unclear clearness", config)

    diagnostics.map(_.message) shouldBe List("Possible spelling issue: clearness")
  }

  it should "include Hunspell REP replacement suggestions in unknown-word diagnostics" in {
    val (dictionary, _) = writeHunspellDictionary(
      "serenity-replacements",
      List("the", "world"),
      List(
        "SET UTF-8",
        "REP 2",
        "REP teh the",
        "REP wurld world"
      )
    )
    val config = SpellCheckConfig(enabled = true, dictionaryPaths = List(dictionary.toString))

    val diagnostics = SpellChecker.check("teh wurld wrld", config)

    diagnostics.map(_.message) shouldBe List(
      "Possible spelling issue: teh (suggestion: the)",
      "Possible spelling issue: wurld (suggestion: world)",
      "Possible spelling issue: wrld"
    )
  }

  it should "load Hunspell dictionaries using the affix SET charset" in {
    val (dictionary, _) = writeHunspellDictionary(
      "serenity-latin1",
      List("caf\u00e9"),
      List("SET ISO-8859-1"),
      StandardCharsets.ISO_8859_1
    )
    val config = SpellCheckConfig(enabled = true, dictionaryPaths = List(dictionary.toString))

    val diagnostics = SpellChecker.check("caf\u00e9 wurld", config)

    diagnostics.map(_.message) shouldBe List("Possible spelling issue: wurld")
  }

  it should "report invalid Hunspell SET charset declarations without crashing" in {
    val (dictionary, _) = writeHunspellDictionary(
      "serenity-invalid-charset",
      List("hello"),
      List("SET NOT_A_CHARSET")
    )
    val config = SpellCheckConfig(enabled = true, dictionaryPaths = List(dictionary.toString))

    val diagnostics = SpellChecker.check("hello wurld", config)

    diagnostics.map(_.code) should contain(Some("dictionary-load-failed"))
    diagnostics.map(_.message) should contain("Possible spelling issue: wurld")
  }

  it should "combine multiple external dictionaries for multilingual spell checking" in {
    val english = writeDic("serenity-en", List("external"))
    val french  = writeDic("serenity-fr", List("bonjour", "café"))
    val config = SpellCheckConfig(
      enabled = true,
      languages = List("en", "fr"),
      dictionaryPaths = List(english.toString, french.toString)
    )

    val diagnostics = SpellChecker.check("external bonjour café wrld", config)

    diagnostics.map(_.message) shouldBe List("Possible spelling issue: wrld")
  }

  it should "report dictionary load failures without preventing fallback spell checks" in {
    val missing = Files.createTempDirectory("serenity-missing-dictionaries").resolve("missing.dic")
    val config = SpellCheckConfig(
      enabled = true,
      dictionaryPaths = List(missing.toString)
    )

    val diagnostics = SpellChecker.check("hello wurld", config)

    diagnostics.map(_.code) should contain(Some("dictionary-load-failed"))
    diagnostics.map(_.message) should contain("Possible spelling issue: wurld")
  }

  it should "reuse cached diagnostics for unchanged buffers without materialising content" in {
    val config      = SpellCheckConfig(enabled = true)
    val bufferId    = BufferId(0)
    val diagnostics = SpellChecker.check("wurld", config)
    val content     = NonCollectingRope(Rope("wurld"))
    val baseBuffer  = AppState.initial.persisted.buffers(bufferId)
    val buffer      = baseBuffer.copy(document = baseBuffer.document.copy(content = content))
    val uri         = SpellChecker.diagnosticsUri(buffer)
    val fingerprint = SpellCheckFingerprint.from(buffer, config)
    val state = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        config = AppConfig.default.withSpellCheck(config),
        buffers = Map(bufferId -> buffer)
      ),
      runtime = AppState.initial.runtime.copy(
        diagnosticsState = AppState.initial.runtime.diagnosticsState.copy(
          diagnostics = Map(uri -> diagnostics),
          spellCheckCache = Map(uri -> SpellCheckCacheEntry(fingerprint, diagnostics))
        )
      )
    )

    val refreshed = SpellChecker.refreshDiagnostics(state)

    refreshed.runtime.diagnosticsState.diagnostics.getOrElse(uri, Nil) shouldBe diagnostics
    refreshed.runtime.diagnosticsState.spellCheckCache.get(uri).map(_.fingerprint) shouldBe Some(fingerprint)
  }

  it should "invalidate cached spell-check diagnostics when buffer content changes" in {
    val config           = SpellCheckConfig(enabled = true)
    val bufferId         = BufferId(0)
    val staleContent     = Rope("wurld")
    val updatedContent   = Rope("hello")
    val baseBuffer       = AppState.initial.persisted.buffers(bufferId)
    val staleBuffer      = baseBuffer.copy(document = baseBuffer.document.copy(content = staleContent))
    val updatedBuffer    = staleBuffer.copy(document = staleBuffer.document.copy(content = updatedContent))
    val uri              = SpellChecker.diagnosticsUri(updatedBuffer)
    val staleDiagnostics = SpellChecker.check("wurld", config)
    val state = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        config = AppConfig.default.withSpellCheck(config),
        buffers = Map(bufferId -> updatedBuffer)
      ),
      runtime = AppState.initial.runtime.copy(
        diagnosticsState = AppState.initial.runtime.diagnosticsState.copy(
          diagnostics = Map(uri -> staleDiagnostics),
          spellCheckCache = Map(
            uri -> SpellCheckCacheEntry(SpellCheckFingerprint.from(staleBuffer, config), staleDiagnostics)
          )
        )
      )
    )

    val refreshed = SpellChecker.refreshDiagnostics(state)

    refreshed.runtime.diagnosticsState.diagnostics.get(uri) shouldBe None
    refreshed.runtime.diagnosticsState.spellCheckCache.get(uri).map(_.diagnostics) shouldBe Some(Nil)
    refreshed.runtime.diagnosticsState.spellCheckCache.get(uri).map(_.fingerprint) shouldBe Some(
      SpellCheckFingerprint.from(updatedBuffer, config)
    )
  }

  it should "invalidate cached spell-check diagnostics when dictionary file content changes" in {
    val dictionary = writeDic("serenity-cache", List("hello"))
    val config     = SpellCheckConfig(enabled = true, dictionaryPaths = List(dictionary.toString))
    val bufferId   = BufferId(0)
    val baseBuffer = AppState.initial.persisted.buffers(bufferId)
    val buffer     = baseBuffer.copy(document = baseBuffer.document.copy(content = Rope("hello added")))
    val uri        = SpellChecker.diagnosticsUri(buffer)
    val staleState = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        config = AppConfig.default.withSpellCheck(config),
        buffers = Map(bufferId -> buffer)
      )
    )
    val staleDiagnostics = SpellChecker.refreshDiagnostics(staleState)
    staleDiagnostics.runtime.diagnosticsState.diagnostics.getOrElse(uri, Nil).map(_.message) shouldBe
      List("Possible spelling issue: added")

    Files.writeString(dictionary, "2\nhello\nadded\n", StandardCharsets.UTF_8)
    Files.setLastModifiedTime(dictionary, FileTime.fromMillis(System.currentTimeMillis() + 10_000L))
    val refreshed = SpellChecker.refreshDiagnostics(staleDiagnostics)

    refreshed.runtime.diagnosticsState.diagnostics.get(uri) shouldBe None
  }

  it should "invalidate cached spell-check diagnostics when affix file content changes" in {
    val (dictionary, affix) = writeHunspellDictionary(
      "serenity-affix-cache",
      List("draft/G"),
      List("SET UTF-8")
    )
    val config     = SpellCheckConfig(enabled = true, dictionaryPaths = List(dictionary.toString))
    val bufferId   = BufferId(0)
    val baseBuffer = AppState.initial.persisted.buffers(bufferId)
    val buffer     = baseBuffer.copy(document = baseBuffer.document.copy(content = Rope("drafting")))
    val uri        = SpellChecker.diagnosticsUri(buffer)
    val staleState = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        config = AppConfig.default.withSpellCheck(config),
        buffers = Map(bufferId -> buffer)
      )
    )
    val staleDiagnostics = SpellChecker.refreshDiagnostics(staleState)
    staleDiagnostics.runtime.diagnosticsState.diagnostics.getOrElse(uri, Nil).map(_.message) shouldBe
      List("Possible spelling issue: drafting")

    Files.writeString(
      affix,
      List("SET UTF-8", "SFX G Y 1", "SFX G 0 ing .").mkString("\n"),
      StandardCharsets.UTF_8
    )
    Files.setLastModifiedTime(affix, FileTime.fromMillis(System.currentTimeMillis() + 10_000L))
    val refreshed = SpellChecker.refreshDiagnostics(staleDiagnostics)

    refreshed.runtime.diagnosticsState.diagnostics.get(uri) shouldBe None
  }

  it should "drop stale spell-check analysis results when the buffer changes before publication" in {
    val config        = SpellCheckConfig(enabled = true)
    val bufferId      = BufferId(0)
    val baseBuffer    = AppState.initial.persisted.buffers(bufferId)
    val staleBuffer   = baseBuffer.copy(document = baseBuffer.document.copy(content = Rope("wurld")))
    val currentBuffer = staleBuffer.copy(document = staleBuffer.document.copy(content = Rope("hello")))
    val staleState = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        config = AppConfig.default.withSpellCheck(config),
        buffers = Map(bufferId -> staleBuffer)
      )
    )
    val currentState =
      staleState.copy(persisted = staleState.persisted.copy(buffers = Map(bufferId -> currentBuffer)))
    val expected = SpellChecker.analysisFingerprints(staleState)
    val analyzed = SpellChecker.refreshDiagnostics(staleState)

    val published = SpellChecker.applyIfCurrent(currentState, analyzed, expected)

    published shouldBe currentState
  }

  "StateManager" should "refresh spell-check diagnostics after prose edits" in {
    val logger = LoggerFactory[IO].getLogger(using LoggerName("SpellCheckerSpec"))
    val stateManager = StateManager
      .apply(logger, initialConfig = AppConfig.default.withSpellCheck(SpellCheckConfig(enabled = true)))
      .unsafeRunSync()

    "wurld".foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())

    val immediateState = stateManager.getCurrentState.unsafeRunSync()
    immediateState.runtime.diagnosticsState.diagnostics
      .getOrElse(SpellChecker.bufferDiagnosticsUri(BufferId(0)), Nil) shouldBe Nil

    IO.sleep(300.millis).unsafeRunSync()

    val state = stateManager.getCurrentState.unsafeRunSync()
    val diagnostics =
      state.runtime.diagnosticsState.diagnostics.getOrElse(SpellChecker.bufferDiagnosticsUri(BufferId(0)), Nil)

    diagnostics.map(_.source) shouldBe List(Some("spell-check"))
    diagnostics.map(_.message) shouldBe List("Possible spelling issue: wurld")
  }
end SpellCheckerSpec
