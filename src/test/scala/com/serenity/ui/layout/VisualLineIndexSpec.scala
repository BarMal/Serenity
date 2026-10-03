package com.serenity.ui.layout

import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/** [[VisualLineIndex]] against the obvious model: a vector of per-line row counts, `None` for a line not yet measured
  * (one row), walked from the top for every answer.
  */
class VisualLineIndexSpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  given generatorConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(minSuccessful = 200)

  private enum Op:
    case Measure(line: Int, rows: Int)
    case Replace(from: Int, removed: Int, inserted: Int)

  private val genOp: Gen[Op] =
    Gen.frequency(
      3 -> (for
        line <- Gen.chooseNum(-2, 70)
        rows <- Gen.chooseNum(1, 9)
      yield Op.Measure(line, rows)),
      1 -> (for
        from     <- Gen.chooseNum(0, 70)
        removed  <- Gen.chooseNum(0, 6)
        inserted <- Gen.chooseNum(0, 6)
      yield Op.Replace(from, removed, inserted))
    )

  private val genScenario: Gen[(Int, List[Op])] =
    for
      lines <- Gen.chooseNum(0, 60)
      ops   <- Gen.listOf(genOp)
    yield (lines, ops)

  private def applyToModel(model: Vector[Option[Int]], op: Op): Vector[Option[Int]] =
    op match
      case Op.Measure(line, rows) =>
        if line < 0 || line >= model.length then model else model.updated(line, Some(rows))
      case Op.Replace(from, removed, inserted) =>
        val start = math.min(from, model.length)
        model.patch(start, Vector.fill(inserted)(None), removed)

  private def applyToIndex(index: VisualLineIndex, op: Op): VisualLineIndex =
    op match
      case Op.Measure(line, rows)              => index.measured(line, rows)
      case Op.Replace(from, removed, inserted) => index.replacedLines(from, removed, inserted)

  private def rowsOf(model: Vector[Option[Int]]): Vector[Int] = model.map(_.getOrElse(1))

  private def run(lines: Int, ops: List[Op]): (VisualLineIndex, Vector[Option[Int]]) =
    ops.foldLeft((VisualLineIndex.unmeasured(lines), Vector.fill(lines)(Option.empty[Int]))) {
      case ((index, model), op) => (applyToIndex(index, op), applyToModel(model, op))
    }

  "VisualLineIndex" should "answer every row query the way walking the lines does" in
    forAll(genScenario) { (lines, ops) =>
      val (index, model) = run(lines, ops)
      val rows           = rowsOf(model)
      val prefix         = rows.scanLeft(0)(_ + _)
      index.lineCount shouldBe model.length
      index.isBalanced shouldBe true
      (0 to model.length).foreach(line => index.rowOfLine(line) shouldBe prefix(line))
      (-1 to prefix.last).foreach { row =>
        val expected = rows.indices
          .find(line => row >= prefix(line) && row < prefix(line + 1))
          .map(line => (line, row - prefix(line)))
        index.lineAtRow(row) shouldBe expected
      }
      model.indices.foreach(line => index.measuredRows(line) shouldBe model(line))
    }

  it should "find the nearest unmeasured line in either direction" in
    forAll(genScenario) { (lines, ops) =>
      val (index, model) = run(lines, ops)
      (0 to model.length + 1).foreach { line =>
        index.lastUnmeasuredBefore(line) shouldBe model.indices.filter(_ < line).findLast(model(_).isEmpty)
        index.firstUnmeasuredFrom(line) shouldBe model.indices.find(l => l >= line && model(l).isEmpty)
      }
    }

  it should "count the rows between two lines" in
    forAll(genScenario, Gen.chooseNum(0, 80), Gen.chooseNum(0, 80)) { (scenario, a, b) =>
      val (index, model) = run(scenario._1, scenario._2)
      val rows           = rowsOf(model)
      val from           = math.min(a, model.length)
      val until          = math.min(b, model.length)
      index.rowsBetween(from, until) shouldBe rows.slice(from, until).sum - rows.slice(until, from).sum
    }

  it should "stay balanced when lines are measured one after another down a long document" in {
    val index = (0 until 5000).foldLeft(VisualLineIndex.unmeasured(5000))((acc, line) => acc.measured(line, 2))
    index.isBalanced shouldBe true
    index.rowOfLine(5000) shouldBe 10000
    index.lineAtRow(9999) shouldBe Some((4999, 1))
  }
