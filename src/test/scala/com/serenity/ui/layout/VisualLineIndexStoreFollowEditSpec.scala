package com.serenity.ui.layout

import java.lang.management.ManagementFactory

import com.serenity.rope.{Balance, Rope}
import com.serenity.testkit.Generators
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/** `followEdit` carries an index across an edit by finding the text the two ropes share at both ends. It must agree
  * with [[LegacyFollowEdit]], the leaf-vector diff it replaced, on every pair of ropes, and cost only what the changed
  * region and the tree's depth cost.
  */
class VisualLineIndexStoreFollowEditSpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  given Balance                                     = Balance.default
  given generatorConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(minSuccessful = 300)

  private val tightBalance = Balance(weightBalance = 8, heightBalance = 1, leafChunkSize = 4)

  private def rowsOf(line: Int): Int = 1 + (line * 7) % 5

  /** An index over `rope` in which every line except each third one (offset by `skew`) is measured. */
  private def partlyMeasured(rope: Rope, skew: Int): VisualLineIndex =
    (0 until rope.lineCount).foldLeft(VisualLineIndex.unmeasured(rope.lineCount)) { (index, line) =>
      if (line + skew) % 3 == 0 then index else index.measured(line, rowsOf(line))
    }

  private def fullyMeasured(rope: Rope): VisualLineIndex = partlyMeasured(rope, skew = 1).measuredEverywhere

  extension (index: VisualLineIndex)
    private def measuredEverywhere: VisualLineIndex =
      (0 until index.lineCount).foldLeft(index)((measured, line) => measured.measured(line, rowsOf(line)))

  final private case class Shape(
      lineCount: Int,
      rowOffsets: Vector[Int],
      measuredRows: Vector[Option[Int]],
      firstUnmeasured: Option[Int],
      lastUnmeasured: Option[Int]
  )

  private def shapeOf(index: VisualLineIndex): Shape =
    Shape(
      index.lineCount,
      (0 to index.lineCount).map(index.rowOfLine).toVector,
      (0 until index.lineCount).map(index.measuredRows).toVector,
      index.firstUnmeasuredFrom(0),
      index.lastUnmeasuredBefore(index.lineCount)
    )

  private def shouldFollowAsLegacyDoes(index: VisualLineIndex, before: Rope, after: Rope): Unit =
    val expected = LegacyFollowEdit.followEdit(index, before, after)
    val actual   = VisualLineIndexStore.followEdit(index, before, after)
    shapeOf(actual) shouldBe shapeOf(expected)
    (actual eq index) shouldBe (expected eq index)

  private enum Edit:
    case Insert(at: Int, text: String)
    case Delete(at: Int, length: Int)
    case Replace(at: Int, length: Int, text: String)
    case InsertNewline(at: Int)
    case DeleteNewline(nth: Int)

  private val genEditedText: Gen[String] =
    Gen.listOf(Gen.frequency(6 -> Gen.alphaChar, 1 -> Gen.const('\n'), 1 -> Gen.const(' '))).map(_.mkString)

  private val genEdit: Gen[Edit] =
    val position = Gen.chooseNum(0, 4000)
    Gen.oneOf(
      Gen.zip(position, genEditedText).map(Edit.Insert(_, _)),
      Gen.zip(position, Gen.chooseNum(0, 80)).map(Edit.Delete(_, _)),
      Gen.zip(position, Gen.chooseNum(0, 80), genEditedText).map(Edit.Replace(_, _, _)),
      position.map(Edit.InsertNewline(_)),
      Gen.chooseNum(0, 40).map(Edit.DeleteNewline(_))
    )

  private def applied(rope: Rope, edit: Edit): Rope =
    def insert(content: Rope, at: Int, text: String) =
      content.insert(at % (content.weight + 1), text).getOrElse(content)
    def delete(content: Rope, at: Int, length: Int) =
      val start = at % (content.weight + 1)
      content.delete(start, math.min(content.weight, start + length)).getOrElse(content)
    edit match
      case Edit.Insert(at, text)          => insert(rope, at, text)
      case Edit.Delete(at, length)        => delete(rope, at, length)
      case Edit.Replace(at, length, text) => insert(delete(rope, at, length), at, text)
      case Edit.InsertNewline(at)         => insert(rope, at, "\n")
      case Edit.DeleteNewline(nth) =>
        val newlines = rope.searchAll("\n")
        newlines.lift(nth % math.max(1, newlines.length)).fold(rope)(at => delete(rope, at, 1))

  private val genEdits: Gen[List[Edit]] = Gen.chooseNum(1, 5).flatMap(Gen.listOfN(_, genEdit))

  private val genShapedEdit: Gen[(Rope, Rope)] =
    for
      text   <- Generators.genText
      before <- Generators.ropeOfShape(text)
      edits  <- genEdits
    yield (before, edits.foldLeft(before)(applied))

  "followEdit" should "follow an edit exactly as the leaf-vector diff did, for ropes of any shape" in
    forAll(genShapedEdit, Gen.chooseNum(0, 2)) {
      case ((before, after), skew) => shouldFollowAsLegacyDoes(partlyMeasured(before, skew), before, after)
    }

  it should "follow every step of an edit chain, and the chain as a whole" in
    forAll(Generators.genText, genEdits) { (text, edits) =>
      val ropes = edits.scanLeft(Rope(text))(applied)
      ropes.sliding(2).foreach {
        case List(before, after) => shouldFollowAsLegacyDoes(partlyMeasured(before, 0), before, after)
        case _                   => ()
      }
      shouldFollowAsLegacyDoes(fullyMeasured(ropes.head), ropes.head, ropes.last)
    }

  it should "follow rebuilds that rebalancing triggers under a tight balance" in
    forAll(Generators.genText, genEdits, genEdits) { (text, first, second) =>
      val start = Rope(text)(using tightBalance)
      val ropes = (first ++ second).scanLeft(start)(applied)
      ropes.sliding(2).foreach {
        case List(before, after) => shouldFollowAsLegacyDoes(partlyMeasured(before, 1), before, after)
        case _                   => ()
      }
      shouldFollowAsLegacyDoes(fullyMeasured(start), start, ropes.last)
    }

  it should "follow between unrelated ropes of unrelated shapes" in
    forAll(Generators.ropeWithText, Generators.ropeWithText) {
      case ((before, _), (after, _)) => shouldFollowAsLegacyDoes(partlyMeasured(before, 2), before, after)
    }

  it should "follow between differently shaped ropes over the same text" in
    forAll(Generators.differentlyShapedRopes) {
      case (before, after, _) => shouldFollowAsLegacyDoes(fullyMeasured(before), before, after)
    }

  it should "start afresh when the index does not describe the rope it is followed from" in
    forAll(Generators.ropeWithText, Generators.ropeWithText, Generators.ropeWithText) {
      case ((described, _), (before, _), (after, _)) =>
        shouldFollowAsLegacyDoes(partlyMeasured(described, 0), before, after)
    }

  it should "return the very same index when nothing changed" in {
    val rope  = Rope("alpha\nbeta\ngamma")
    val index = fullyMeasured(rope)
    VisualLineIndexStore.followEdit(index, rope, rope) should be theSameInstanceAs index
  }

  it should "keep every measurement when a freshly built rope holds the same text" in {
    val text   = (0 until 300).map(line => "line " * (line % 7)).mkString("\n")
    val before = Rope(text)
    val after  = Rope(text)
    (before eq after) shouldBe false
    val index = fullyMeasured(before)
    VisualLineIndexStore.followEdit(index, before, after) should be theSameInstanceAs index
  }

  it should "unmeasure only the line a typed character lands in" in {
    val before   = Rope((0 until 100).map(line => "x" * (line % 9)).mkString("\n"))
    val after    = before.insert(before.lineColumnToOffset(40, 1), "y").getOrElse(before)
    val index    = fullyMeasured(before)
    val followed = VisualLineIndexStore.followEdit(index, before, after)
    (0 until 100).foreach(line => followed.measuredRows(line).isDefined shouldBe line != 40)
  }

  it should "unmeasure the two lines a newline splits and shift those after them" in {
    val before   = Rope("one\ntwo\nthree\nfour")
    val after    = before.insert(before.lineColumnToOffset(1, 1), "\n").getOrElse(before)
    val followed = VisualLineIndexStore.followEdit(fullyMeasured(before), before, after)
    followed.lineCount shouldBe 5
    (0 until 5).map(followed.measuredRows) shouldBe
      Vector(Some(rowsOf(0)), None, None, Some(rowsOf(2)), Some(rowsOf(3)))
  }

  it should "unmeasure the merged line when a newline is deleted" in {
    val before   = Rope("one\ntwo\nthree\nfour")
    val after    = before.delete(3, 4).getOrElse(before)
    val followed = VisualLineIndexStore.followEdit(fullyMeasured(before), before, after)
    followed.lineCount shouldBe 3
    (0 until 3).map(followed.measuredRows) shouldBe Vector(None, Some(rowsOf(2)), Some(rowsOf(3)))
  }

  it should "unmeasure the first line for an edit at the start and the last for one at the end" in {
    val before  = Rope("one\ntwo\nthree")
    val index   = fullyMeasured(before)
    val atStart = VisualLineIndexStore.followEdit(index, before, before.insert(0, "x").getOrElse(before))
    val atEnd   = VisualLineIndexStore.followEdit(index, before, before.insert(13, "x").getOrElse(before))
    (0 until 3).map(atStart.measuredRows) shouldBe Vector(None, Some(rowsOf(1)), Some(rowsOf(2)))
    (0 until 3).map(atEnd.measuredRows) shouldBe Vector(Some(rowsOf(0)), Some(rowsOf(1)), None)
  }

  it should "follow to and from an empty rope" in {
    val text  = Rope("a\nb")
    val empty = Rope("")
    shouldFollowAsLegacyDoes(fullyMeasured(empty), empty, text)
    shouldFollowAsLegacyDoes(fullyMeasured(text), text, empty)
    shouldFollowAsLegacyDoes(fullyMeasured(empty), empty, empty)
    VisualLineIndexStore.followEdit(fullyMeasured(text), text, empty).lineCount shouldBe 1
  }

  it should "start afresh when the line count the index describes is not the rope's" in {
    val before   = Rope("a\nb\nc")
    val after    = Rope("a\nb\nc\nd")
    val followed = VisualLineIndexStore.followEdit(fullyMeasured(Rope("a")), before, after)
    followed.lineCount shouldBe 4
    (0 until 4).foreach(line => followed.measuredRows(line) shouldBe None)
  }

  it should "allocate in proportion to the depth, not the leaf count, for one typed character" in {
    val leafChunk = Balance(weightBalance = 1000, heightBalance = 5, leafChunkSize = 4)
    val text      = (0 until 100000).map(n => f"${n % 1000}%03d\n").mkString
    val before    = Rope(text)(using leafChunk)
    val after     = before.insert(before.weight / 2, "x").getOrElse(before)
    val index     = fullyMeasured(before)

    def allocatedByFollowing(): Long =
      val bytes = ManagementFactory.getThreadMXBean match
        case counted: com.sun.management.ThreadMXBean => counted.getCurrentThreadAllocatedBytes
        case _                                        => 0L
      val _ = VisualLineIndexStore.followEdit(index, before, after)
      val used = ManagementFactory.getThreadMXBean match
        case counted: com.sun.management.ThreadMXBean => counted.getCurrentThreadAllocatedBytes
        case _                                        => 0L
      used - bytes

    (0 until 20).foreach(_ => allocatedByFollowing())
    val followed = (0 until 5).map(_ => allocatedByFollowing()).max
    followed should be < 64L * 1024
    shapeOf(VisualLineIndexStore.followEdit(index, before, after)) shouldBe
      shapeOf(LegacyFollowEdit.followEdit(index, before, after))
  }
