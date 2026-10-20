package com.serenity.state.reducers

import java.util.concurrent.atomic.AtomicInteger

import com.serenity.rope.{Balance, ChangeSet, Leaf, OverlapPolicy, Rope}
import com.serenity.state.models.{Annotations, CursorPosition, DocumentComment, Placeholder}
import com.serenity.state.reducers.EditorEditSupport.MultiCursorEdit
import com.serenity.testkit.ChangeSetGenerators.{genParts, genShortText}
import org.scalacheck.{Gen, Shrink}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/** Annotations carried across a batch of edits by line/column arithmetic must land exactly where the offset-based remap
  * (`remapEditBoundary` between two rope conversions) puts them, and must not convert per position.
  */
class EditRemapSpec extends AnyFlatSpec with ScalaCheckPropertyChecks with Matchers:

  given Balance = Balance.default

  given generatorConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(minSuccessful = 300)

  given [A]: Shrink[A] = Shrink.shrinkAny

  final private case class Batch(edits: List[MultiCursorEdit], before: Rope, after: Rope)

  private val batch: Gen[Batch] =
    for
      shortText <- genShortText
      text = shortText.replace('x', '\n') + "\nab\n\ncd"
      parts <- genParts(text.length, minGap = 1).suchThat(_.nonEmpty)
      edits  = parts.zipWithIndex.map((part, index) => MultiCursorEdit(index, part.from, part.to, part.insert))
      before = Rope(text)
      after = ChangeSet
        .fromIntents(text.length, parts, OverlapPolicy.ConcatAtPoint)
        .applyTo(before)
        .getOrElse(before)
    yield Batch(edits, before, after)

  private def allPositions(rope: Rope): List[CursorPosition] =
    (0 to rope.weight).toList.map(rope.offsetToLineColumn).map((line, column) => CursorPosition(line, column))

  private def viaOffsets(position: CursorPosition, in: Batch, moves: Boolean): CursorPosition =
    val offset         = in.before.lineColumnToOffset(position.line, position.column)
    val mapped         = EditorEditSupport.remapEditBoundary(offset, in.edits.sortBy(e => (e.start, e.end)), moves)
    val (line, column) = in.after.offsetToLineColumn(mapped)
    CursorPosition(line, column)

  "Placeholders and bookmarks" should "land where the offset remap puts them, at every position" in
    forAll(batch) { in =>
      val positions = allPositions(in.before)
      val expected  = positions.map(viaOffsets(_, in, moves = false))
      EditorEditSupport
        .adjustPlaceholders(positions.map(Placeholder(_, "")), in.before, in.after, in.edits)
        .map(_.position) shouldBe expected
      EditorEditSupport.adjustBookmarks(positions, in.before, in.after, in.edits) shouldBe expected.distinct
    }

  "Comments" should "land where the offset remap puts their ends, at every start and end" in
    forAll(batch) { in =>
      val positions = allPositions(in.before)
      val comments  = positions.zip(positions.drop(2)).map((from, to) => DocumentComment(from, to, "note"))
      val expected = comments.map { comment =>
        val start = viaOffsets(comment.start, in, moves = true)
        val end   = viaOffsets(comment.end, in, moves = false)
        val clamped =
          if end.line < start.line || (end.line == start.line && end.column < start.column) then start else end
        (start, clamped)
      }
      EditorEditSupport
        .adjustDocumentComments(comments, in.before, in.after, in.edits)
        .map(comment => (comment.anchor, comment.focus)) shouldBe expected
    }

  private class CountingLeaf(text: String) extends Leaf(text):
    val conversions = new AtomicInteger(0)

    override def offsetToLineColumn(offset: Int): (Int, Int) =
      conversions.incrementAndGet()
      super.offsetToLineColumn(offset)

    override def lineColumnToOffset(line: Int, column: Int): Int =
      conversions.incrementAndGet()
      super.lineColumnToOffset(line, column)

    override def offsetsToLineColumns(ascendingOffsets: IndexedSeq[Int]): Vector[(Int, Int)] =
      conversions.addAndGet(ascendingOffsets.size)
      super.offsetsToLineColumns(ascendingOffsets)

  private def conversionsToAdjust(annotationCount: Int): Int =
    val text      = (0 until 400).map(line => s"line $line of the manuscript").mkString("\n")
    val before    = new CountingLeaf(text)
    val after     = new CountingLeaf(text.patch(5, "x", 0))
    val positions = (0 until annotationCount).toList.map(index => CursorPosition(index % 400, index % 7))
    val annotations = Annotations(
      bookmarks = positions,
      placeholders = positions.map(Placeholder(_, "todo")),
      documentComments = positions.map(position => DocumentComment(position, position.moveRight, "note"))
    )
    val _ = EditorEditSupport.adjustAnnotations(annotations, before, after, List(MultiCursorEdit(0, 5, 5, "x")))
    before.conversions.get + after.conversions.get

  "A keystroke" should "convert the same number of positions however many annotations the buffer holds" in {
    conversionsToAdjust(annotationCount = 1_000) shouldBe conversionsToAdjust(annotationCount = 10)
  }

  it should "convert at most two positions per edit" in {
    conversionsToAdjust(annotationCount = 1_000) should be <= 2
  }
