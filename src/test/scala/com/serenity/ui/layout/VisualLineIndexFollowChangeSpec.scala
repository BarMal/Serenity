package com.serenity.ui.layout

import com.serenity.rope.{Balance, Rope}
import com.serenity.testkit.UniqueTextEdits
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Following an edit from the change that made it (#1838) keeps exactly the measurements following it by comparing the
  * two texts keeps, without reading either text.
  */
class VisualLineIndexFollowChangeSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private def rowsOf(content: Rope)(line: Int): Int = 1 + content.getLine(line).getOrElse("").length % 5

  private def fullyMeasured(content: Rope): VisualLineIndex =
    (0 until content.lineCount).foldLeft(VisualLineIndex.unmeasured(content.lineCount))((index, line) =>
      index.measured(line, rowsOf(content)(line))
    )

  private def measurements(index: VisualLineIndex): Vector[Option[Int]] =
    Vector.tabulate(index.lineCount)(index.measuredRows)

  "Following a change" should "agree with following the texts on which lines stay measured" in {
    val cases = UniqueTextEdits.cases(seed = 1838, count = 400)
    cases.size should be > 100
    cases.foreach { found =>
      val index    = fullyMeasured(found.before)
      val byChange = VisualLineIndexStore.followChange(index, found.before, found.after, found.change)
      val byTexts  = VisualLineIndexStore.followEdit(index, found.before, found.after)
      withClue(s"${found.change}: ")(measurements(byChange) shouldBe measurements(byTexts))
    }
  }

  it should "leave only true measurements, over the lines of the new text" in
    UniqueTextEdits.cases(seed = 7, count = 400).foreach { found =>
      val followed = VisualLineIndexStore.followChange(
        fullyMeasured(found.before),
        found.before,
        found.after,
        found.change
      )
      followed.lineCount shouldBe found.after.lineCount
      measurements(followed).zipWithIndex.foreach {
        case (rows, line) => rows.foreach(_ shouldBe rowsOf(found.after)(line))
      }
    }

  it should "drop the index when it does not describe the text before the change" in {
    val found    = UniqueTextEdits.cases(seed = 3, count = 5).headOption.getOrElse(fail("no edit drawn"))
    val stale    = fullyMeasured(Rope("one\ntwo"))
    val followed = VisualLineIndexStore.followChange(stale, found.before, found.after, found.change)
    followed.lineCount shouldBe found.after.lineCount
    measurements(followed).flatten shouldBe empty
  }

  "A store given a revision" should "follow the recorded change and answer as walking every line does" in {
    val store = new VisualLineIndexStore[Unit](1)
    UniqueTextEdits.cases(seed = 11, count = 100).foreach { found =>
      val first = store.counts((), found.before, rowsOf(found.before), revision = Some(ContentRevision(0L, _ => None)))
      val _     = first.rowsBetween(0, found.before.lineCount)
      val next = store.counts(
        (),
        found.after,
        rowsOf(found.after),
        revision = Some(ContentRevision(1L, since => Option.when(since == 0L)(found.change)))
      )
      val walking = VisualRowCounts.walking(found.after.lineCount, rowsOf(found.after))
      next.rowsBetween(0, found.after.lineCount) shouldBe walking.rowsBetween(0, found.after.lineCount)
    }
  }

  it should "fall back to comparing the texts when the revision cannot say what changed" in {
    val store = new VisualLineIndexStore[Unit](1)
    UniqueTextEdits.cases(seed = 12, count = 100).foreach { found =>
      val first = store.counts((), found.before, rowsOf(found.before), revision = Some(ContentRevision(0L, _ => None)))
      val _     = first.rowsBetween(0, found.before.lineCount)
      val next  = store.counts((), found.after, rowsOf(found.after), revision = Some(ContentRevision(5L, _ => None)))
      val walking = VisualRowCounts.walking(found.after.lineCount, rowsOf(found.after))
      next.rowsBetween(0, found.after.lineCount) shouldBe walking.rowsBetween(0, found.after.lineCount)
    }
  }
