package com.serenity.perf

import cats.effect.unsafe.implicits.global
import com.serenity.perf.FrameCountBaseline.Change
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** What the planner and the surfaces do for a fixed set of interactions, counted exactly and compared with the
  * checked-in `frame-counts.tsv`. A frame that starts redrawing a row it used to keep, an idle frame that paints, a
  * keystroke that shapes more text or a terminal frame that writes more bytes fails here on the pull request that
  * causes it, with the number that moved. Pixel equality with a frame drawn from scratch stays with the frame specs.
  */
class FrameCountRatchetSpec extends AnyFlatSpec with Matchers:

  private lazy val measured = FrameCountScenarios.entries(FrameCountBaseline.DeviceScale)

  private def failure(changes: List[Change]): String =
    s"""Frame counts moved from the checked-in baseline:
       |${FrameCountBaseline.table(changes)}
       |
       |A count that went up is work a frame now does that it did not; one that went down is reuse that was gained.
       |If the change is intended, run `${FrameCountBaseline.RegenerateCommand}`, commit src/test/resources/perf/frame-counts.tsv
       |and say in the PR why the counts moved.""".stripMargin

  "The frame counts" should "equal the checked-in baseline exactly" in {
    val baseline = FrameCountBaseline.load().fold(fail(_), identity)

    val changes = FrameCountBaseline.changes(baseline, measured)

    withClue(failure(changes)) {
      changes shouldBe empty
    }
  }

  it should "be the same when every scenario is run again" in {
    val again = FrameCountScenarios.entries(FrameCountBaseline.DeviceScale)

    FrameCountBaseline.changes(measured, again) shouldBe empty
  }

  "An idle frame" should "draw, shape and present nothing and keep every row, whatever the baseline says" in {
    val counts = measured.toMap
    val idle   = s"${FrameCountScenarios.IdleScenario}."

    List("rows-drawn", "runs", "shapings", "present-px", "chrome-painted").foreach { counter =>
      withClue(s"$idle$counter: ")(counts(idle + counter) shouldBe 0L)
    }
    counts(idle + "rows-preserved") shouldBe counts("first_frame.rows-drawn")
  }

  "The baseline" should "round-trip through its file format" in {
    val entries = Vector("first_frame.rows-drawn" -> 41L, "idle_frame.runs" -> 0L)

    FrameCountBaseline.parse(FrameCountBaseline.render(entries).linesIterator.toList) shouldBe Right(entries)
  }

  it should "reject a line that is not a scenario.counter and a number" in {
    FrameCountBaseline.parse(List("first_frame.runs\tmany")).isLeft shouldBe true
    FrameCountBaseline.parse(List("first_frame.runs")).isLeft shouldBe true
  }

  "A comparison" should "report a count that went up, one that went down and one on either side only" in {
    val baseline = Vector("a.runs" -> 10L, "a.rows-drawn" -> 5L, "b.runs" -> 1L, "gone.runs" -> 2L)
    val now      = Vector("a.runs" -> 11L, "a.rows-drawn" -> 4L, "b.runs" -> 1L, "new.runs" -> 3L)

    FrameCountBaseline.changes(baseline, now) shouldBe List(
      Change("a.runs", Some(10L), Some(11L)),
      Change("a.rows-drawn", Some(5L), Some(4L)),
      Change("gone.runs", Some(2L), None),
      Change("new.runs", None, Some(3L))
    )
  }

  it should "report nothing when every count is the same" in {
    val counts = Vector("a.runs" -> 10L, "b.runs" -> 0L)

    FrameCountBaseline.changes(counts, counts) shouldBe empty
  }

  it should "print scenario, counter, baseline and now" in {
    val text = FrameCountBaseline.table(List(Change("scroll_3_lines.rows-drawn", Some(8L), Some(9L))))

    text.linesIterator.toList shouldBe List(
      "scenario        counter     baseline  now",
      "scroll_3_lines  rows-drawn  8         9"
    )
  }
