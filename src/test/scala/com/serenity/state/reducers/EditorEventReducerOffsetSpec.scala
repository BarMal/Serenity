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
    reduceTextEvent("a🙂b", CursorPosition(0, 3), MoveLeft).editing.cursors shouldBe List(CursorPosition(0, 1))
    reduceTextEvent("a🙂b", CursorPosition(0, 1), MoveRight).editing.cursors shouldBe List(CursorPosition(0, 3))
  }

  it should "move left and right across emoji skin-tone modifier sequences as one grapheme" in {
    val text = "a\uD83D\uDC4D\uD83C\uDFFDb"

    reduceTextEvent(text, CursorPosition(0, 5), MoveLeft).editing.cursors shouldBe List(CursorPosition(0, 1))
    reduceTextEvent(text, CursorPosition(0, 1), MoveRight).editing.cursors shouldBe List(CursorPosition(0, 5))
  }

  it should "move left and right across combining-mark accents as one grapheme" in {
    reduceTextEvent("cafe\u0301!", CursorPosition(0, 5), MoveLeft).editing.cursors shouldBe List(CursorPosition(0, 3))
    reduceTextEvent("cafe\u0301!", CursorPosition(0, 3), MoveRight).editing.cursors shouldBe List(CursorPosition(0, 5))
  }

  it should "move left and right by word boundaries" in {
    reduceTextEvent("alpha, beta gamma", CursorPosition(0, 17), MoveWordLeft).editing.cursors shouldBe
      List(CursorPosition(0, 12))
    reduceTextEvent("alpha, beta gamma", CursorPosition(0, 12), MoveWordLeft).editing.cursors shouldBe
      List(CursorPosition(0, 7))
    reduceTextEvent("alpha, beta gamma", CursorPosition(0, 0), MoveWordRight).editing.cursors shouldBe
      List(CursorPosition(0, 5))
    reduceTextEvent("alpha, beta gamma", CursorPosition(0, 5), MoveWordRight).editing.cursors shouldBe
      List(CursorPosition(0, 7))
  }

  it should "extend the selection by word boundaries" in {
    val leftExtended = reduceTextEvent("alpha, beta gamma", CursorPosition(0, 17), ExtendSelectionWordLeft)
    leftExtended.editing.cursors shouldBe List(CursorPosition(0, 12))
    leftExtended.editing.selection shouldBe Some(Selection(CursorPosition(0, 17), CursorPosition(0, 12)))

    val rightExtended = reduceTextEvent("alpha, beta gamma", CursorPosition(0, 0), ExtendSelectionWordRight)
    rightExtended.editing.cursors shouldBe List(CursorPosition(0, 5))
    rightExtended.editing.selection shouldBe Some(Selection(CursorPosition(0, 0), CursorPosition(0, 5)))
  }

  it should "delete complete graphemes for backward and forward deletes" in {
    val deleteEmojiBackward = reduceTextEvent("a🙂b", CursorPosition(0, 3), DeleteBackward)
    deleteEmojiBackward.document.content.collect() shouldBe "ab"
    deleteEmojiBackward.editing.cursors shouldBe List(CursorPosition(0, 1))

    val deleteAccentForward = reduceTextEvent("cafe\u0301!", CursorPosition(0, 3), DeleteForward)
    deleteAccentForward.document.content.collect() shouldBe "caf!"
    deleteAccentForward.editing.cursors shouldBe List(CursorPosition(0, 3))
  }

  it should "delete complete graphemes when the cursor starts inside one" in {
    val emoji = "\uD83D\uDE42"

    val deleteEmojiForward = reduceTextEvent(s"a${emoji}b", CursorPosition(0, 2), DeleteForward)
    deleteEmojiForward.document.content.collect() shouldBe "ab"
    deleteEmojiForward.editing.cursors shouldBe List(CursorPosition(0, 1))

    val deleteAccentBackward = reduceTextEvent("cafe\u0301!", CursorPosition(0, 4), DeleteBackward)
    deleteAccentBackward.document.content.collect() shouldBe "caf!"
    deleteAccentBackward.editing.cursors shouldBe List(CursorPosition(0, 3))
  }

  it should "replace whole graphemes when selection endpoints split them" in {
    val buffer = Buffer
      .fromString(bufferId, "cafe\u0301!")
      .copy(editing =
        EditingState(
          cursors = List(CursorPosition(0, 5)),
          selection = Some(Selection(CursorPosition(0, 4), CursorPosition(0, 5)))
        )
      )
    val state = AppState.initial.copy(persisted = AppState.initial.persisted.copy(buffers = Map(bufferId -> buffer)))

    val updatedBuffer = EditorEventReducer.reduce(InsertChar('X'), paneId, state).state.persisted.buffers(bufferId)

    updatedBuffer.document.content.collect() shouldBe "cafX!"
    updatedBuffer.editing.cursors shouldBe List(CursorPosition(0, 4))
  }

  it should "insert beside a grapheme when the cursor starts inside one" in {
    val updatedBuffer = reduceTextEvent("cafe\u0301!", CursorPosition(0, 4), InsertChar('X'))

    updatedBuffer.document.content.collect() shouldBe "cafe\u0301X!"
    updatedBuffer.editing.cursors shouldBe List(CursorPosition(0, 6))
  }

  it should "move, delete, and replace regional-indicator flag pairs as one grapheme" in {
    val flag = "\uD83C\uDDFA\uD83C\uDDF8"
    val text = s"a$flag!"

    reduceTextEvent(text, CursorPosition(0, 5), MoveLeft).editing.cursors shouldBe List(CursorPosition(0, 1))
    reduceTextEvent(text, CursorPosition(0, 1), MoveRight).editing.cursors shouldBe List(CursorPosition(0, 5))
    reduceTextEvent(text, CursorPosition(0, 1), DeleteForward).document.content.collect() shouldBe "a!"
    reduceTextEvent(text, CursorPosition(0, 5), DeleteBackward).document.content.collect() shouldBe "a!"

    val buffer = Buffer
      .fromString(bufferId, text)
      .copy(editing =
        EditingState(
          cursors = List(CursorPosition(0, 5)),
          selection = Some(Selection(CursorPosition(0, 2), CursorPosition(0, 4)))
        )
      )
    val state = AppState.initial.copy(persisted = AppState.initial.persisted.copy(buffers = Map(bufferId -> buffer)))

    EditorEventReducer
      .reduce(InsertChar('X'), paneId, state)
      .state
      .persisted
      .buffers(bufferId)
      .document
      .content
      .collect() shouldBe "aX!"
  }

  private def reduceTextEvent(text: String, cursor: CursorPosition, event: TextEntryEvent): Buffer =
    val buffer = Buffer.fromString(bufferId, text).copy(editing = EditingState(cursors = List(cursor)))
    val state  = AppState.initial.copy(persisted = AppState.initial.persisted.copy(buffers = Map(bufferId -> buffer)))

    EditorEventReducer.reduce(event, paneId, state).state.persisted.buffers(bufferId)
