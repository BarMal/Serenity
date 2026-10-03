package com.serenity.ui.layout

import java.util.concurrent.atomic.AtomicInteger

import com.serenity.rope.{Balance, Rope}
import com.serenity.testkit.Generators
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/** An indexed [[VisualRowCounts]] answers exactly what walking the lines answers, across edits and wrap widths, and its
  * index follows an edit by re-measuring only the lines the edit touched.
  */
class VisualRowCountsSpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  given Balance                                     = Balance.default
  given generatorConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(minSuccessful = 80)

  /** A stand-in for wrapping: a line of `n` characters folds into `ceil(n / width)` rows, and never fewer than one. */
  private def rowsFor(content: Rope, width: Int)(line: Int): Int =
    math.max(1, (content.getLine(line).getOrElse("").length + width - 1) / width)

  private val genLine: Gen[String] =
    Gen.frequency(
      1 -> Gen.const(""),
      4 -> Gen.chooseNum(1, 40).flatMap(n => Gen.listOfN(n, Gen.alphaChar)).map(_.mkString)
    )

  private val genDocument: Gen[String] =
    Gen.chooseNum(1, 40).flatMap(n => Gen.listOfN(n, genLine)).map(_.mkString("\n"))

  private enum Edit:
    case Insert(at: Int, text: String)
    case Delete(at: Int, length: Int)

  private val genEdit: Gen[Edit] =
    Gen.oneOf(
      for
        at   <- Gen.chooseNum(0, 2000)
        text <- Gen.listOf(Gen.frequency(6 -> Gen.alphaChar, 1 -> Gen.const('\n'))).map(_.mkString)
      yield Edit.Insert(at, text),
      for
        at     <- Gen.chooseNum(0, 2000)
        length <- Gen.chooseNum(0, 60)
      yield Edit.Delete(at, length)
    )

  private def edited(content: Rope, edit: Edit): Rope =
    edit match
      case Edit.Insert(at, text) => content.insert(at % (content.weight + 1), text).getOrElse(content)
      case Edit.Delete(at, length) =>
        val start = at % (content.weight + 1)
        content.delete(start, math.min(content.weight, start + length)).getOrElse(content)

  private enum Query:
    case RowsIn(line: Int)
    case Between(from: Int, until: Int)
    case Above(line: Int, rows: Int)
    case Below(line: Int, rows: Int)

  private val genQuery: Gen[Query] =
    val line = Gen.chooseNum(0, 60)
    Gen.oneOf(
      line.map(Query.RowsIn(_)),
      Gen.zip(line, line).map(Query.Between(_, _)),
      Gen.zip(line, Gen.chooseNum(1, 50)).map(Query.Above(_, _)),
      Gen.zip(line, Gen.chooseNum(0, 50)).map(Query.Below(_, _))
    )

  private def answer(counts: VisualRowCounts, query: Query): Any =
    def inDocument(line: Int) = math.min(line, math.max(0, counts.lineCount - 1))
    query match
      case Query.RowsIn(line)         => counts.rowsIn(inDocument(line))
      case Query.Between(from, until) => counts.rowsBetween(inDocument(from), inDocument(until))
      case Query.Above(line, rows)    => counts.rowAbove(math.min(line, counts.lineCount), rows)
      case Query.Below(line, rows)    => counts.rowBelow(inDocument(line), rows)

  private val genStep: Gen[(Edit, Int, List[Query])] =
    for
      edit    <- genEdit
      width   <- Gen.oneOf(3, 7, 20)
      queries <- Gen.listOfN(4, genQuery)
    yield (edit, width, queries)

  "An indexed VisualRowCounts" should "answer as walking every line does, through edits and width changes" in
    forAll(genDocument, Gen.listOf(genStep)) { (document, steps) =>
      val store = new VisualLineIndexStore[Int](4)
      steps.foldLeft(Rope(document)) {
        case (content, (edit, width, queries)) =>
          val next    = edited(content, edit)
          val indexed = store.counts(width, next, rowsFor(next, width))
          val walking = VisualRowCounts.walking(next.lineCount, rowsFor(next, width))
          queries.foreach(query =>
            withClue(s"$query at width $width: ")(answer(indexed, query) shouldBe answer(walking, query))
          )
          next
      }
    }

  it should "carry an index across edits to exactly the index a rebuild measures" in
    forAll(genDocument, Gen.listOf(genEdit)) { (document, edits) =>
      val store = new VisualLineIndexStore[Unit](1)
      val last = edits.foldLeft(Rope(document)) { (content, edit) =>
        val counts = store.counts((), content, rowsFor(content, 5))
        val _      = counts.rowsBetween(0, content.lineCount)
        edited(content, edit)
      }
      val followed = store.indexFor((), last)
      val rebuilt =
        (0 until last.lineCount).foldLeft(VisualLineIndex.unmeasured(last.lineCount)) { (index, line) =>
          index.measured(line, rowsFor(last, 5)(line))
        }
      (0 until last.lineCount).foreach(line => followed.measuredRows(line).foreach(_ shouldBe rowsFor(last, 5)(line)))
      val _         = store.counts((), last, rowsFor(last, 5)).rowsBetween(0, last.lineCount)
      val refreshed = store.indexFor((), last)
      (0 to last.lineCount).foreach(line => refreshed.rowOfLine(line) shouldBe rebuilt.rowOfLine(line))
    }

  it should "keep only true measurements when following an edit between ropes of any shape" in
    forAll(genDocument, genEdit) { (document, edit) =>
      val original = Rope(document)
      val changed  = edited(original, edit).collect()
      forAll(Generators.ropeOfShape(document), Generators.ropeOfShape(changed)) { (before, after) =>
        val measured =
          (0 until before.lineCount).foldLeft(VisualLineIndex.unmeasured(before.lineCount)) { (index, line) =>
            index.measured(line, rowsFor(before, 3)(line))
          }
        val followed = VisualLineIndexStore.followEdit(measured, before, after)
        followed.lineCount shouldBe after.lineCount
        (0 until after.lineCount).foreach { line =>
          followed.measuredRows(line).foreach(_ shouldBe rowsFor(after, 3)(line))
        }
      }
    }

  it should "re-measure only the lines an edit touched" in {
    val content  = Rope((0 until 200).map(line => "x" * (line % 30)).mkString("\n"))
    val store    = new VisualLineIndexStore[Unit](1)
    val measured = new AtomicInteger(0)
    def counting(rope: Rope)(line: Int): Int =
      val _ = measured.incrementAndGet()
      rowsFor(rope, 4)(line)
    val _ = store.counts((), content, counting(content)).rowsBetween(0, 200)
    measured.get() shouldBe 200

    measured.set(0)
    val typed = content.insert(content.lineColumnToOffset(120, 3), "abc").getOrElse(content)
    store.counts((), typed, counting(typed)).rowsBetween(0, 200) shouldBe
      VisualRowCounts.walking(200, rowsFor(typed, 4)).rowsBetween(0, 200)
    measured.get() shouldBe 1

    measured.set(0)
    val split = typed.insert(typed.lineColumnToOffset(40, 2), "\n").getOrElse(typed)
    store.counts((), split, counting(split)).rowsBetween(0, 201) shouldBe
      VisualRowCounts.walking(201, rowsFor(split, 4)).rowsBetween(0, 201)
    measured.get() shouldBe 2
  }

  it should "measure only the lines a placement reads, not the whole document" in {
    val content  = Rope(("word " * 12 + "\n") * 5000)
    val store    = new VisualLineIndexStore[Unit](1)
    val measured = new AtomicInteger(0)
    def counting(line: Int): Int =
      val _ = measured.incrementAndGet()
      rowsFor(content, 10)(line)
    val counts = store.counts((), content, counting)
    counts
      .rowAbove(4000, 30) shouldBe VisualRowCounts.walking(content.lineCount, rowsFor(content, 10)).rowAbove(4000, 30)
    measured.get() should be <= 30
    measured.set(0)
    val _ = counts.rowAbove(4000, 30)
    measured.get() shouldBe 0
  }

  it should "keep a separate index per key, so a new wrap width starts from scratch" in {
    val content = Rope((0 until 50).map(line => "y" * line).mkString("\n"))
    val store   = new VisualLineIndexStore[Int](4)
    List(5, 9, 5).foreach { width =>
      store.counts(width, content, rowsFor(content, width)).rowsBetween(0, 50) shouldBe
        VisualRowCounts.walking(50, rowsFor(content, width)).rowsBetween(0, 50)
    }
    store.size shouldBe 2
  }
