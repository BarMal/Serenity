package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.config.{AppConfig, SpellCheckConfig}
import com.serenity.keystroke.events.InsertChar
import com.serenity.rope.Balance
import com.serenity.spellcheck.SpellChecker
import com.serenity.state.manager.StateManager
import com.serenity.state.models.BufferId
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class SpellCheckerSpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

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
