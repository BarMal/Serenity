package com.serenity.testkit

import cats.data.NonEmptyList
import com.serenity.state.models.{Cursor, CursorPosition, EditingState, Viewport}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class StateDifferenceSpec extends AnyFlatSpec with Matchers:

  private val viewport = Viewport(visibleLines = 36, visibleColumns = 110)
  private val cursors  = EditingState(NonEmptyList.one(Cursor(CursorPosition(2, 4))))

  private def shown(content: String = "lorem ipsum", editing: EditingState = cursors, at: Viewport = viewport) =
    Some((content, editing, at))

  "Two editors that agree" should "have nothing to describe" in {
    StateDifference.describe(shown(), shown(), perEventDepth = 3, batchedDepth = 3) shouldBe Nil
  }

  "Different text" should "name the first offset that differs and what is there" in {
    val parts = StateDifference.describe(shown("lorem ipsum"), shown("lorem ipsvm"), 3, 3)
    parts should have size 1
    parts.head should include("text differs at offset 9")
    parts.head should include("expected 'u'")
    parts.head should include("actual 'v'")
  }

  "Text of different lengths" should "name the length and the first offset where one ends" in {
    val parts = StateDifference.describe(shown("lorem"), shown("lorem ipsum"), 3, 3)
    parts.head should include("text differs at offset 5")
    parts.head should include("lengths 5 and 11")
  }

  "Different cursors" should "say so, with both" in {
    val moved = EditingState(NonEmptyList.one(Cursor(CursorPosition(2, 5))))
    val parts = StateDifference.describe(shown(), shown(editing = moved), 3, 3)
    parts should have size 1
    parts.head should startWith("cursors differ")
    parts.head should include("column=4")
    parts.head should include("column=5")
  }

  "A different viewport" should "say so, with both" in {
    val scrolled = viewport.copy(topLine = 7)
    val parts    = StateDifference.describe(shown(), shown(at = scrolled), 3, 3)
    parts should have size 1
    parts.head should startWith("viewport differs")
    parts.head should include("topLine=0")
    parts.head should include("topLine=7")
  }

  "A different undo depth" should "give both depths, per-event first" in {
    StateDifference.describe(shown(), shown(), perEventDepth = 4, batchedDepth = 3) shouldBe
      List("undo depth: 4 one event at a time, 3 batched")
  }

  "A buffer one editor does not hold" should "be named" in {
    StateDifference.describe(shown(), None, 3, 3).head should include("batched editor has no buffer")
    StateDifference.describe(None, shown(), 3, 3).head should include("one-at-a-time editor has no buffer")
  }

  "Several differences at once" should "each be listed" in {
    val parts = StateDifference.describe(shown("a"), shown("b", at = viewport.copy(topLine = 1)), 2, 1)
    parts.map(_.takeWhile(_ != ':').takeWhile(_ != ' ')) shouldBe List("text", "viewport", "undo")
  }
