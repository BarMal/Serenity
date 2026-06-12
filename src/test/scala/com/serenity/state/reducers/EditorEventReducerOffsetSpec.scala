package com.serenity.state.reducers

import com.serenity.rope.{Balance, Rope}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class EditorEventReducerOffsetSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  "EditorEventReducer.lineColumnToOffset" should "resolve line and column positions" in {
    val rope = Rope("alpha\nbeta\ngamma")

    EditorEventReducer.lineColumnToOffset(rope, 0, 0) shouldBe 0
    EditorEventReducer.lineColumnToOffset(rope, 0, 3) shouldBe 3
    EditorEventReducer.lineColumnToOffset(rope, 1, 0) shouldBe 6
    EditorEventReducer.lineColumnToOffset(rope, 1, 2) shouldBe 8
    EditorEventReducer.lineColumnToOffset(rope, 2, 5) shouldBe rope.weight
  }

  it should "clamp negative and out-of-range positions" in {
    val rope = Rope("alpha\nbeta")

    EditorEventReducer.lineColumnToOffset(rope, -1, -4) shouldBe 0
    EditorEventReducer.lineColumnToOffset(rope, 0, 20) shouldBe 5
    EditorEventReducer.lineColumnToOffset(rope, 20, 0) shouldBe rope.weight
  }

  it should "stop at the requested line without depending on later large content" in {
    val rope = Rope(("target\n" + ("tail\n" * 50000)).stripSuffix("\n"))

    EditorEventReducer.lineColumnToOffset(rope, 0, 3) shouldBe 3
    EditorEventReducer.lineColumnToOffset(rope, 1, 2) shouldBe 9
  }
