package com.serenity.ui.layout

import com.serenity.ui.layout.TextMeasureBaseline.{Change, Entry}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** What AWT measures for the bundled fonts and the corpus, compared with the checked-in `text-measure/baseline.tsv`. A
  * number that moves fails here with the number; if the file is missing the failure carries the whole table, which is
  * how it is first generated from a CI log (#2660 item 2, step A).
  */
class TextMeasureBaselineSpec extends AnyFlatSpec with Matchers:

  private lazy val measured = TextMeasureBaseline.entries()

  private def failure(changes: List[Change]): String =
    val table = changes
      .map(change =>
        s"${change.key}\t${change.baseline.fold("-")(_.mkString(","))}\t${change.now.fold("-")(_.mkString(","))}"
      )
      .mkString("\n")
    s"""Text measurement moved from the checked-in baseline:
       |key\tbaseline\tnow
       |$table
       |
       |If the change is intended, run `${TextMeasureBaseline.RegenerateCommand}`, commit
       |src/test/resources/text-measure/baseline.tsv and say in the PR why the numbers moved.""".stripMargin

  "The text measurements" should "equal the checked-in baseline, integers exactly and floats within the tolerance" in {
    TextMeasureBaseline.load() match
      case Left(reason) =>
        fail(
          s"$reason. Generate it with `${TextMeasureBaseline.RegenerateCommand}`; the table follows.\n" +
            s"TEXT_MEASURE_BASELINE_BEGIN\n${TextMeasureBaseline.render(measured)}TEXT_MEASURE_BASELINE_END"
        )
      case Right(baseline) =>
        val changes = TextMeasureBaseline.changes(baseline, measured)
        withClue(failure(changes))(changes shouldBe empty)
  }

  "The baseline" should "round-trip through its file format" in {
    val entries =
      Vector(Entry("courier_prime.cell", Vector("9", "17", "14")), Entry("a.b.carets", Vector("0.0000", "8.4000")))

    TextMeasureBaseline.parse(TextMeasureBaseline.render(entries).linesIterator.toList) shouldBe Right(entries)
  }

  it should "reject a line that is not a key and numbers" in {
    TextMeasureBaseline.parse(List("courier_prime.cell\tmany")).isLeft shouldBe true
    TextMeasureBaseline.parse(List("courier_prime.cell")).isLeft shouldBe true
  }

  "Comparing" should "let a float differ by the tolerance and no more, and an integer not at all" in {
    val baseline = Vector(Entry("f.l.carets", Vector("1.0000")), Entry("f.l.rows@240", Vector("3")))

    TextMeasureBaseline.changes(
      baseline,
      Vector(Entry("f.l.carets", Vector("1.0100")), Entry("f.l.rows@240", Vector("3")))
    ) shouldBe Nil
    TextMeasureBaseline
      .changes(baseline, Vector(Entry("f.l.carets", Vector("1.0300")), Entry("f.l.rows@240", Vector("3"))))
      .map(_.key) shouldBe
      List("f.l.carets")
    TextMeasureBaseline
      .changes(baseline, Vector(Entry("f.l.carets", Vector("1.0000")), Entry("f.l.rows@240", Vector("4"))))
      .map(_.key) shouldBe
      List("f.l.rows@240")
  }

  it should "report a key only one side has" in {
    val baseline = Vector(Entry("f.cell", Vector("9")))

    TextMeasureBaseline.changes(baseline, Vector(Entry("g.cell", Vector("9")))).map(_.key) shouldBe List(
      "f.cell",
      "g.cell"
    )
  }
