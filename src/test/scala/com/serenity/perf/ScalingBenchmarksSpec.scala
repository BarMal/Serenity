package com.serenity.perf

import cats.effect.unsafe.implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The rows that ask how performance scales with many open buffers and split panes (#2478). */
class ScalingBenchmarksSpec extends AnyFlatSpec with Matchers:

  private lazy val rows = ScalingBenchmarks.benchmarks()

  private val expectedNames: Set[String] =
    (for
      open <- List(1, 20)
      kind <- List("code", "prose")
    yield s"buffers.switch_cold_frame.open_$open.$kind").toSet ++
      (for
        panes  <- List(1, 2, 4)
        action <- List("typing_frame", "scroll_line_frame", "page_down_frame")
      yield s"panes.split$panes.$action").toSet +
      "scrollbar.four_panes.all_bars"

  "The scaling rows" should "be the four cold-frame rows, the nine split-frame rows and the late-bar row" in {
    rows.map(_.name).toSet shouldBe expectedNames
    rows.map(_.name).distinct should have size rows.size
  }

  it should "all be excluded from the perf gate, so none gates a build before the owner has read its numbers" in {
    rows.map(_.name).filterNot(PerfGateExclusions.covers) shouldBe Nil
  }

  it should "verify, and then run once, without failing" in
    rows.foreach { row =>
      withClue(row.name) {
        row.verify()
        row.run()
      }
    }

  "The pane-edge allowance (#2481)" should "cover exactly the five device columns the damaged frame is known to get wrong" in {
    ScalingBenchmarks.edgeColumns(1540) shouldBe (1537 to 1541)
  }

  it should "let through differences inside those five columns and nothing outside them" in {
    val inside  = (1537 to 1541).map(x => (x, 3))
    val outside = Vector((1536, 3), (1542, 3), (0, 0))

    ScalingBenchmarks.outsideEdgeColumns(inside ++ outside, List(1540)) shouldBe outside
  }

  it should "let nothing through when there is no shared edge" in {
    ScalingBenchmarks.outsideEdgeColumns(Vector((1539, 2)), Nil) shouldBe Vector((1539, 2))
  }
