package com.serenity.state.reducers

import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A keycap sequence, a digit then the emoji variation selector then the combining enclosing keycap (U+0031 U+FE0F
  * U+20E3), is one grapheme to the caret: left, right and the two deletes cross it whole, whatever clusters the shaper
  * draws it in (#2660 item 2, step D2: Skia reports the variation selector as a cluster of its own).
  */
class KeycapGraphemeSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val bufferId = BufferId(0)
  private val paneId   = PaneId(0)

  private val keycap = "1️⃣"
  private val text   = s"a${keycap}b"

  private def reduce(cursor: CursorPosition, event: TextEntryEvent): Buffer =
    val buffer = Buffer.fromString(bufferId, text).copy(editing = EditingState(List(cursor)))
    val state  = AppState.initial.copy(persisted = AppState.initial.persisted.copy(buffers = Map(bufferId -> buffer)))
    EditorEventReducer.reduce(event, paneId, state).state.persisted.buffers(bufferId)

  "The caret" should "move right across a keycap sequence as one unit" in {
    reduce(CursorPosition(0, 1), MoveRight).editing.cursorPositions shouldBe List(CursorPosition(0, 4))
  }

  it should "move left across a keycap sequence as one unit" in {
    reduce(CursorPosition(0, 4), MoveLeft).editing.cursorPositions shouldBe List(CursorPosition(0, 1))
  }

  it should "delete a keycap sequence whole with backspace" in {
    val deleted = reduce(CursorPosition(0, 4), DeleteBackward)

    deleted.document.content.collect() shouldBe "ab"
    deleted.editing.cursorPositions shouldBe List(CursorPosition(0, 1))
  }

  it should "delete a keycap sequence whole with delete" in {
    val deleted = reduce(CursorPosition(0, 1), DeleteForward)

    deleted.document.content.collect() shouldBe "ab"
    deleted.editing.cursorPositions shouldBe List(CursorPosition(0, 1))
  }
