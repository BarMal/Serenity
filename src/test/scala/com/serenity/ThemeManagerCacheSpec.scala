package com.serenity

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

import com.serenity.lsp.config.LanguageId
import com.serenity.lsp.model.SemanticToken
import com.serenity.ui.theme.{Theme, ThemeManager}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Covers the `Ref`-backed replacement for `ThemeManager`'s `LinkedHashMap`+`synchronized` highlight cache (issue
  * #1412): repeated calls still hit the memoized result, concurrent callers don't corrupt it, and pushing past the
  * bounded cache size doesn't lose correctness for the most recently computed entries. The cache key now includes
  * this line's semantic tokens (issue #859/#1177) rather than a lexical state, so these cases exercise that shape.
  */
class ThemeManagerCacheSpec extends AnyFlatSpec with Matchers:

  private val theme = Theme.dark

  private def keywordToken(length: Int): List[SemanticToken] =
    List(SemanticToken(line = 0, startCharacter = 0, length = length, tokenType = "keyword", tokenModifiers = Set.empty))

  "highlightLine" should "return an equal, memoized result for a repeated call" in {
    val first  = ThemeManager.highlightLine("val x = 1", theme, Some(LanguageId.Scala), Some(keywordToken(3)))
    val second = ThemeManager.highlightLine("val x = 1", theme, Some(LanguageId.Scala), Some(keywordToken(3)))

    first shouldBe second
  }

  it should "produce consistent results under concurrent access from many threads" in {
    val lines = (0 until 200).map(i => s"val concurrentCacheProbe$i = $i")

    val futures = Future.traverse(lines) { line =>
      Future {
        val tokens = Some(keywordToken(3))
        val a      = ThemeManager.highlightLine(line, theme, Some(LanguageId.Scala), tokens)
        val b      = ThemeManager.highlightLine(line, theme, Some(LanguageId.Scala), tokens)
        (line, a, b)
      }
    }

    val results = Await.result(futures, Duration(30, "s"))
    results.foreach {
      case (line, a, b) =>
        a shouldBe b
        a.map(_.content).mkString shouldBe line
    }
  }

  it should "stay correct for entries pushed in after the cache exceeds its bound" in {
    // MaxHighlightCacheEntries is 4096 -- pushing well past that must not raise, and the most recently computed
    // entries must still round-trip correctly (eviction is bounded-FIFO, not a correctness hazard).
    (0 until 4200).foreach(i =>
      ThemeManager.highlightLine(s"val evictionProbe$i = $i", theme, Some(LanguageId.Scala), Some(keywordToken(3)))
    )

    val recent =
      ThemeManager.highlightLine("val evictionProbe4199 = 4199", theme, Some(LanguageId.Scala), Some(keywordToken(3)))
    recent.map(_.content).mkString shouldBe "val evictionProbe4199 = 4199"
  }

  it should "not reuse a cached result when this line's semantic tokens differ" in {
    val first  = ThemeManager.highlightLine("val x = 1", theme, Some(LanguageId.Scala), Some(keywordToken(3)))
    val second = ThemeManager.highlightLine("val x = 1", theme, Some(LanguageId.Scala), None)

    second should not be theSameInstanceAs(first)
  }
