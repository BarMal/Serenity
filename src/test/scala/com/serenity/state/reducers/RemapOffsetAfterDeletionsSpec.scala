package com.serenity.state.reducers

import com.serenity.VerticalNavSupport
import com.serenity.keystroke.events.DeleteWordBackward
import com.serenity.rope.{Balance, Rope}
import com.serenity.state.models.*
import com.serenity.testkit.EditingStateFixtures
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/** A merged multi-cursor deletion must land every cursor where a plain string edit would put it: offsets after a
  * deletion shift by the lengths of all deletions before them, and offsets inside a deletion collapse to its (shifted)
  * start. Comparing against a character-by-character model keeps the expectation independent of the shifting arithmetic
  * under test.
  */
class RemapOffsetAfterDeletionsSpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  given Balance = Balance.default

  private val paneId   = PaneId(0)
  private val bufferId = BufferId(0)

  private def isDeleted(index: Int, deletions: List[(Int, Int)]): Boolean =
    deletions.exists { case (start, end) => index >= start && index < end }

  private def survivingBefore(offset: Int, deletions: List[(Int, Int)]): Int =
    (0 until offset).count(index => !isDeleted(index, deletions))

  private def stateWithCursors(content: String, cursors: List[CursorPosition]): AppState =
    val initial = AppState.initial.persisted.buffers(bufferId)
    val buffer = initial.copy(
      document = initial.document.copy(content = Rope(content)),
      editing = EditingStateFixtures(cursors = cursors)
    )
    AppState.initial.copy(persisted =
      AppState.initial.persisted.copy(buffers = AppState.initial.persisted.buffers.updated(bufferId, buffer))
    )

  private val genDeletions: Gen[List[(Int, Int)]] =
    for
      pieces <- Gen.choose(0, 6)
      spans  <- Gen.listOfN(pieces, Gen.zip(Gen.choose(0, 5), Gen.choose(1, 5)))
    yield spans
      .foldLeft((0, List.empty[(Int, Int)])) {
        case ((cursor, acc), (gap, length)) =>
          val start = cursor + gap
          (start + length, (start, start + length) :: acc)
      }
      ._2
      .reverse

  "remapOffsetAfterDeletions" should "subtract every earlier deletion from an offset after several ranges" in {
    EditorEditSupport.remapOffsetAfterDeletions(9, List((2, 4), (6, 8))) shouldBe 5
  }

  it should "collapse an offset inside a deletion to the deletion's shifted start" in {
    EditorEditSupport.remapOffsetAfterDeletions(6, List((2, 4), (6, 8))) shouldBe 4
    EditorEditSupport.remapOffsetAfterDeletions(7, List((2, 4), (6, 8))) shouldBe 4
    EditorEditSupport.remapOffsetAfterDeletions(8, List((2, 4), (6, 8))) shouldBe 4
  }

  it should "leave offsets before the first deletion alone" in {
    EditorEditSupport.remapOffsetAfterDeletions(1, List((2, 4), (6, 8))) shouldBe 1
  }

  it should "agree with a character-level deletion model for arbitrary sorted non-overlapping deletions" in
    forAll(genDeletions, Gen.choose(0, 60)) { (deletions, offset) =>
      EditorEditSupport.remapOffsetAfterDeletions(offset, deletions) shouldBe survivingBefore(offset, deletions)
    }

  "A three-cursor word deletion that merges into three ranges" should "place every cursor like a plain string edit" in {
    val content   = "aa bb cc dd"
    val deletions = List((0, 2), (3, 5), (6, 8))
    val offsets   = List(2, 5, 8)
    val state     = stateWithCursors(content, offsets.map(CursorPosition(0, _)))

    val buffer = VerticalNavSupport.dispatch(DeleteWordBackward, paneId, state).state.persisted.buffers(bufferId)

    val expectedContent = content.zipWithIndex.collect { case (char, index) if !isDeleted(index, deletions) => char }
    buffer.document.content.collect() shouldBe expectedContent.mkString
    buffer.editing.cursorPositions shouldBe offsets.map(offset => CursorPosition(0, survivingBefore(offset, deletions)))
  }
