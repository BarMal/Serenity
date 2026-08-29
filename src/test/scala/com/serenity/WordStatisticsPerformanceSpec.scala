package com.serenity

import com.serenity.rope.{Balance, Rope}
import com.serenity.text.TextStatistics
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Perf coverage for #1203's acceptance criterion: no per-keystroke full-buffer rescan on a novel-length (~100k word)
  * buffer. `TextStatistics.of` only reads `Rope`'s own incrementally-maintained `wordCount`/`nonWhitespaceCount` fields
  * (see `RopeSpec`'s "keep an edit's word-count update proportional to tree depth" for the structural guarantee that an
  * edit only rebuilds the spine down to the touched leaf); this spec logs the real wall-clock cost of repeated
  * single-character edits against such a buffer for human review, in the same spirit as
  * `StartupPageNavigationPerformanceSpec` -- see that spec's doc for why these timings aren't asserted on directly.
  */
class WordStatisticsPerformanceSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  "TextStatistics.of on a novel-length buffer" should "update per-keystroke without a full rescan" in {
    val words   = (1 to 100000).map(i => s"word$i").mkString(" ")
    val initial = Rope(words)
    TextStatistics.of(initial).wordCount shouldBe 100000

    val editTimings = (1 to 200).map { i =>
      val start  = System.nanoTime()
      val edited = initial.insert(initial.weight / 2, s"x$i ").getOrElse(fail("expected insert to succeed"))
      val _      = TextStatistics.of(edited)
      (System.nanoTime() - start) / 1000000L
    }

    // Correctness: an insert in the middle of the buffer adds exactly one word.
    val onceEdited = initial.insert(initial.weight / 2, "inserted ").getOrElse(fail("expected insert to succeed"))
    TextStatistics.of(onceEdited).wordCount shouldBe 100001

    info(
      "per-keystroke word-count update on a 100k-word buffer -- " +
        s"average: ${editTimings.sum / editTimings.length}ms, max: ${editTimings.max}ms"
    )
  }
