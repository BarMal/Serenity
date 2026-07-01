package com.serenity.state.reducers

import com.serenity.keystroke.events.*
import com.serenity.rope.{Balance, Rope}
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class EditorEventReducerOffsetSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val bufferId = BufferId(0)
  private val paneId   = PaneId(0)

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

  it should "move left and right across surrogate-pair emoji as one grapheme" in {
    reduceTextEvent("a🙂b", CursorPosition(0, 3), MoveLeft).cursors shouldBe List(CursorPosition(0, 1))
    reduceTextEvent("a🙂b", CursorPosition(0, 1), MoveRight).cursors shouldBe List(CursorPosition(0, 3))
  }

  it should "move left and right across combining-mark accents as one grapheme" in {
    reduceTextEvent("cafe\u0301!", CursorPosition(0, 5), MoveLeft).cursors shouldBe List(CursorPosition(0, 3))
    reduceTextEvent("cafe\u0301!", CursorPosition(0, 3), MoveRight).cursors shouldBe List(CursorPosition(0, 5))
  }

  it should "move left and right by word boundaries" in {
    reduceTextEvent("alpha, beta gamma", CursorPosition(0, 17), MoveWordLeft).cursors shouldBe
      List(CursorPosition(0, 12))
    reduceTextEvent("alpha, beta gamma", CursorPosition(0, 12), MoveWordLeft).cursors shouldBe
      List(CursorPosition(0, 7))
    reduceTextEvent("alpha, beta gamma", CursorPosition(0, 0), MoveWordRight).cursors shouldBe
      List(CursorPosition(0, 5))
    reduceTextEvent("alpha, beta gamma", CursorPosition(0, 5), MoveWordRight).cursors shouldBe
      List(CursorPosition(0, 7))
  }

  it should "delete complete graphemes for backward and forward deletes" in {
    val deleteEmojiBackward = reduceTextEvent("a🙂b", CursorPosition(0, 3), DeleteBackward)
    deleteEmojiBackward.content.collect() shouldBe "ab"
    deleteEmojiBackward.cursors shouldBe List(CursorPosition(0, 1))

    val deleteAccentForward = reduceTextEvent("cafe\u0301!", CursorPosition(0, 3), DeleteForward)
    deleteAccentForward.content.collect() shouldBe "caf!"
    deleteAccentForward.cursors shouldBe List(CursorPosition(0, 3))
  }

  it should "delete complete graphemes when the cursor starts inside one" in {
    val emoji = "\uD83D\uDE42"

    val deleteEmojiForward = reduceTextEvent(s"a${emoji}b", CursorPosition(0, 2), DeleteForward)
    deleteEmojiForward.content.collect() shouldBe "ab"
    deleteEmojiForward.cursors shouldBe List(CursorPosition(0, 1))

    val deleteAccentBackward = reduceTextEvent("cafe\u0301!", CursorPosition(0, 4), DeleteBackward)
    deleteAccentBackward.content.collect() shouldBe "caf!"
    deleteAccentBackward.cursors shouldBe List(CursorPosition(0, 3))
  }

  it should "replace whole graphemes when selection endpoints split them" in {
    val buffer = Buffer
      .fromString(bufferId, "cafe\u0301!")
      .copy(
        cursors = List(CursorPosition(0, 5)),
        selection = Some(Selection(CursorPosition(0, 4), CursorPosition(0, 5)))
      )
    val state = AppState.initial.copy(buffers = Map(bufferId -> buffer))

    val updatedBuffer = EditorEventReducer.reduce(InsertChar('X'), paneId, state).state.buffers(bufferId)

    updatedBuffer.content.collect() shouldBe "cafX!"
    updatedBuffer.cursors shouldBe List(CursorPosition(0, 4))
  }

  it should "insert beside a grapheme when the cursor starts inside one" in {
    val updatedBuffer = reduceTextEvent("cafe\u0301!", CursorPosition(0, 4), InsertChar('X'))

    updatedBuffer.content.collect() shouldBe "cafe\u0301X!"
    updatedBuffer.cursors shouldBe List(CursorPosition(0, 6))
  }

  private def reduceTextEvent(text: String, cursor: CursorPosition, event: TextEntryEvent): Buffer =
    val buffer = Buffer.fromString(bufferId, text).copy(cursors = List(cursor))
    val state  = AppState.initial.copy(buffers = Map(bufferId -> buffer))

    EditorEventReducer.reduce(event, paneId, state).state.buffers(bufferId)
