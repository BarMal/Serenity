package com.serenity.state.models

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** `Selection` and `DocumentComment` share the `DirectedRange` anchor/focus ordering (`#1053`) -- both directions of
  * authoring (anchor before focus, and the reverse) must resolve to the same `start`/`end`/`contains` regardless of
  * which case class is used.
  */
class DirectedRangeSpec extends AnyFlatSpec with Matchers:

  private val early = CursorPosition(1, 0)
  private val late  = CursorPosition(3, 5)

  "Selection.start/end" should "keep anchor as start when anchor precedes focus" in {
    val selection = Selection(anchor = early, focus = late)
    selection.start shouldBe early
    selection.end shouldBe late
  }

  it should "flip start/end when focus precedes anchor" in {
    val selection = Selection(anchor = late, focus = early)
    selection.start shouldBe early
    selection.end shouldBe late
  }

  it should "treat an anchor and focus on the same line by column" in {
    val forward  = Selection(CursorPosition(2, 1), CursorPosition(2, 9))
    val backward = Selection(CursorPosition(2, 9), CursorPosition(2, 1))
    forward.start shouldBe CursorPosition(2, 1)
    forward.end shouldBe CursorPosition(2, 9)
    backward.start shouldBe CursorPosition(2, 1)
    backward.end shouldBe CursorPosition(2, 9)
  }

  it should "collapse to a single point when anchor equals focus" in {
    val selection = Selection(CursorPosition(4, 4), CursorPosition(4, 4))
    selection.start shouldBe CursorPosition(4, 4)
    selection.end shouldBe CursorPosition(4, 4)
  }

  "DocumentComment.start/end" should "match Selection's ordering for the same anchor/focus pair" in {
    val forward  = DocumentComment(anchor = early, focus = late, text = "note")
    val backward = DocumentComment(anchor = late, focus = early, text = "note")
    forward.start shouldBe early
    forward.end shouldBe late
    backward.start shouldBe early
    backward.end shouldBe late
  }

  "DocumentComment.contains" should "include the start and end boundaries" in {
    val comment = DocumentComment(anchor = late, focus = early, text = "note") // authored backward
    comment.contains(early) shouldBe true
    comment.contains(late) shouldBe true
  }

  it should "include positions strictly between start and end" in {
    val comment = DocumentComment(CursorPosition(1, 0), CursorPosition(3, 5), "note")
    comment.contains(CursorPosition(2, 0)) shouldBe true
  }

  it should "exclude positions before start or after end" in {
    val comment = DocumentComment(CursorPosition(1, 0), CursorPosition(3, 5), "note")
    comment.contains(CursorPosition(0, 9)) shouldBe false
    comment.contains(CursorPosition(3, 6)) shouldBe false
  }

  it should "compare by column when the cursor shares start's line" in {
    val comment = DocumentComment(CursorPosition(1, 5), CursorPosition(3, 0), "note")
    comment.contains(CursorPosition(1, 4)) shouldBe false
    comment.contains(CursorPosition(1, 5)) shouldBe true
  }

  it should "compare by column when the cursor shares end's line" in {
    val comment = DocumentComment(CursorPosition(1, 0), CursorPosition(3, 5), "note")
    comment.contains(CursorPosition(3, 6)) shouldBe false
    comment.contains(CursorPosition(3, 5)) shouldBe true
  }
