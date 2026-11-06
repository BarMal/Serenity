package com.serenity.ui.layout

import java.util.concurrent.atomic.AtomicReference

import com.serenity.state.models.BufferId
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #2506: the three row counts one pane is scrolled and painted by, reported on one line so a machine where the wheel
  * stops short of the end of a document can say which count is off.
  */
class ScrollRowCountsSpec extends AnyFlatSpec with Matchers:

  private val buffer = BufferId(7)

  "A pane whose limit and painter agree" should "report the three counts and an agreeing verdict" in {
    ScrollRowCounts(gridLines = 40, limitRows = 40, paintedLines = 40).logLine(buffer) shouldBe
      "[SCROLL-ROWS] buffer=7 grid.visibleLines=40 limit.rowsThatFit=40 painted.visibleLines=40 verdict=agree"
  }

  "A pane whose limit counts more rows than the painter shows" should "name the surplus" in {
    ScrollRowCounts(gridLines = 40, limitRows = 42, paintedLines = 40).logLine(buffer) shouldBe
      "[SCROLL-ROWS] buffer=7 grid.visibleLines=40 limit.rowsThatFit=42 painted.visibleLines=40 verdict=limit-over-by-2"
  }

  "A pane whose limit counts fewer rows than the painter shows" should "name the shortfall" in {
    ScrollRowCounts(gridLines = 40, limitRows = 38, paintedLines = 40).logLine(buffer) shouldBe
      "[SCROLL-ROWS] buffer=7 grid.visibleLines=40 limit.rowsThatFit=38 painted.visibleLines=40 verdict=limit-under-by-2"
  }

  private def recording(enabled: Boolean): (ScrollRowsReporter, () => Vector[String]) =
    val lines = AtomicReference(Vector.empty[String])
    (ScrollRowsReporter(enabled, line => lines.updateAndGet(_ :+ line)), () => lines.get)

  "A reporter that is off" should "say nothing, and not measure the counts" in {
    val measured         = AtomicReference(false)
    val (reporter, said) = recording(enabled = false)

    reporter.report(
      buffer, {
        measured.set(true)
        ScrollRowCounts(40, 40, 40)
      }
    )

    said() shouldBe empty
    measured.get shouldBe false
  }

  "A reporter that is on" should "say a buffer's counts once, and again when they change" in {
    val (reporter, said) = recording(enabled = true)

    reporter.report(buffer, ScrollRowCounts(40, 42, 40))
    reporter.report(buffer, ScrollRowCounts(40, 42, 40))
    reporter.report(buffer, ScrollRowCounts(41, 43, 41))

    said().size shouldBe 2
    said().last should include("grid.visibleLines=41")
  }

  it should "track each buffer on its own" in {
    val (reporter, said) = recording(enabled = true)

    reporter.report(BufferId(1), ScrollRowCounts(40, 40, 40))
    reporter.report(BufferId(2), ScrollRowCounts(40, 40, 40))

    said().size shouldBe 2
  }
