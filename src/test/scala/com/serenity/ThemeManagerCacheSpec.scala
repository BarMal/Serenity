package com.serenity

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

import com.serenity.lsp.config.LanguageId
import com.serenity.ui.theme.{Theme, ThemeManager}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Covers the `Ref`-backed replacement for `ThemeManager`'s `LinkedHashMap`+`synchronized` highlight/lex caches
  * (issue #1412): repeated calls still hit the memoized result, concurrent callers don't corrupt it, and pushing
  * past the bounded cache size doesn't lose correctness for the most recently computed entries.
  */
class ThemeManagerCacheSpec extends AnyFlatSpec with Matchers:

  private val theme = Theme.dark

  "highlightLine" should "return an equal, memoized result for a repeated call" in {
    val first  = ThemeManager.highlightLine("val x = 1", theme, Some(LanguageId.Scala))
    val second = ThemeManager.highlightLine("val x = 1", theme, Some(LanguageId.Scala))

    first shouldBe second
  }

  it should "produce consistent results under concurrent access from many threads" in {
    val lines = (0 until 200).map(i => s"val concurrentCacheProbe$i = $i")

    val futures = Future.traverse(lines) { line =>
      Future {
        val a = ThemeManager.highlightLine(line, theme, Some(LanguageId.Scala))
        val b = ThemeManager.highlightLine(line, theme, Some(LanguageId.Scala))
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
    (0 until 4200).foreach(i => ThemeManager.highlightLine(s"val evictionProbe$i = $i", theme, Some(LanguageId.Scala)))

    val recent = ThemeManager.highlightLine("val evictionProbe4199 = 4199", theme, Some(LanguageId.Scala))
    recent.map(_.content).mkString shouldBe "val evictionProbe4199 = 4199"
  }

  "lineStartStates" should "return an equal, memoized result for a repeated call on the same document" in {
    val lines = Vector("val a = 1", "val b = /* open", "still open")

    val first  = ThemeManager.lineStartStates("cache-spec-doc", lines, Some(LanguageId.Scala))
    val second = ThemeManager.lineStartStates("cache-spec-doc", lines, Some(LanguageId.Scala))

    first shouldBe second
  }
