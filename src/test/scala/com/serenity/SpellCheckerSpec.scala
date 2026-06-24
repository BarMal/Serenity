package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.config.{AppConfig, SpellCheckConfig}
import com.serenity.keystroke.events.InsertChar
import com.serenity.rope.{Balance, Rope}
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

  final case class NonCollectingRope(delegate: Rope) extends Rope:
    override def weight: Int =
      delegate.weight

    override def height: Int =
      delegate.height

    override def newlineCount: Int =
      delegate.newlineCount

    override def lastLineLength: Int =
      delegate.lastLineLength

    override def endsWithNewline: Boolean =
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

  it should "reuse cached diagnostics for unchanged buffers without materialising content" in {
    val config      = SpellCheckConfig(enabled = true)
    val bufferId    = BufferId(0)
    val diagnostics = SpellChecker.check("wurld", config)
    val content     = NonCollectingRope(Rope("wurld"))
    val buffer      = AppState.initial.buffers(bufferId).copy(content = content)
    val uri         = SpellChecker.diagnosticsUri(buffer)
    val fingerprint = SpellCheckFingerprint.from(buffer, config)
    val state = AppState.initial.copy(
      config = AppConfig.default.withSpellCheck(config),
      buffers = Map(bufferId -> buffer),
      diagnostics = Map(uri -> diagnostics),
      spellCheckCache = Map(uri -> SpellCheckCacheEntry(fingerprint, diagnostics))
    )

    val refreshed = SpellChecker.refreshDiagnostics(state)

    refreshed.diagnostics.getOrElse(uri, Nil) shouldBe diagnostics
    refreshed.spellCheckCache.get(uri).map(_.fingerprint) shouldBe Some(fingerprint)
  }

  it should "invalidate cached spell-check diagnostics when buffer content changes" in {
    val config           = SpellCheckConfig(enabled = true)
    val bufferId         = BufferId(0)
    val staleContent     = Rope("wurld")
    val updatedContent   = Rope("hello")
    val staleBuffer      = AppState.initial.buffers(bufferId).copy(content = staleContent)
    val updatedBuffer    = staleBuffer.copy(content = updatedContent)
    val uri              = SpellChecker.diagnosticsUri(updatedBuffer)
    val staleDiagnostics = SpellChecker.check("wurld", config)
    val state = AppState.initial.copy(
      config = AppConfig.default.withSpellCheck(config),
      buffers = Map(bufferId -> updatedBuffer),
      diagnostics = Map(uri -> staleDiagnostics),
      spellCheckCache = Map(
        uri -> SpellCheckCacheEntry(SpellCheckFingerprint.from(staleBuffer, config), staleDiagnostics)
      )
    )

    val refreshed = SpellChecker.refreshDiagnostics(state)

    refreshed.diagnostics.get(uri) shouldBe None
    refreshed.spellCheckCache.get(uri).map(_.diagnostics) shouldBe Some(Nil)
    refreshed.spellCheckCache.get(uri).map(_.fingerprint) shouldBe Some(
      SpellCheckFingerprint.from(updatedBuffer, config)
    )
  }

  "StateManager" should "refresh spell-check diagnostics after prose edits" in {
    val logger = LoggerFactory[IO].getLogger(using LoggerName("SpellCheckerSpec"))
    val stateManager = StateManager
      .apply(logger, initialConfig = AppConfig.default.withSpellCheck(SpellCheckConfig(enabled = true)))
      .unsafeRunSync()

    "wurld".foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())

    val state       = stateManager.getCurrentState.unsafeRunSync()
    val diagnostics = state.diagnostics.getOrElse(SpellChecker.bufferDiagnosticsUri(BufferId(0)), Nil)

    diagnostics.map(_.source) shouldBe List(Some("spell-check"))
    diagnostics.map(_.message) shouldBe List("Possible spelling issue: wurld")
  }
end SpellCheckerSpec
