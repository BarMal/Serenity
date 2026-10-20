package com.serenity.state.reducers

import cats.data.NonEmptyList
import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Select next/skip/select-all occurrence (#1994, #1951): which cursors the commands leave behind, how a selection that
  * began as a bare caret differs from one the user made, and that typing then reaches every cursor.
  */
class EditorOccurrenceEventReducerSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val bufferId = BufferId(0)
  private val paneId   = PaneId(0)

  private def caret(line: Int, column: Int): Cursor = Cursor(CursorPosition(line, column))

  private def selecting(startLine: Int, startColumn: Int, endLine: Int, endColumn: Int): Cursor =
    Cursor(Selection(CursorPosition(startLine, startColumn), CursorPosition(endLine, endColumn)))

  private def stateWith(text: String, cursors: Cursor*): AppState =
    val buffer = Buffer
      .fromString(bufferId, text)
      .copy(editing = EditingState(NonEmptyList.fromListUnsafe(cursors.toList)))
    AppState.initial.copy(persisted = AppState.initial.persisted.copy(buffers = Map(bufferId -> buffer)))

  private def reduced(state: AppState, event: TextEntryEvent): AppState =
    EditorEventReducer.reduce(event, paneId, state).state

  private def buffer(state: AppState): Buffer = state.persisted.buffers(bufferId)

  private def pressed(state: AppState, events: TextEntryEvent*): AppState =
    events.foldLeft(state)(reduced)

  /** Each selected range as (startLine, startColumn, endLine, endColumn), in document order. */
  private def ranges(state: AppState): List[(Int, Int, Int, Int)] =
    buffer(state).allSelections
      .map(selection => (selection.start.line, selection.start.column, selection.end.line, selection.end.column))
      .sorted

  private def typed(state: AppState, text: String): String =
    pressed(state, text.map(InsertChar(_))*).persisted.buffers(bufferId).document.content.collect()

  "SelectNextOccurrence on a bare caret" should "select the word the caret is inside" in {
    val after = reduced(stateWith("cat dog cat", caret(0, 1)), SelectNextOccurrence)

    ranges(after) shouldBe List((0, 0, 0, 3))
    buffer(after).editing.cursors.size shouldBe 1
  }

  it should "select the word the caret is touching at either edge" in {
    ranges(reduced(stateWith("cat dog", caret(0, 3)), SelectNextOccurrence)) shouldBe List((0, 0, 0, 3))
    ranges(reduced(stateWith("cat dog", caret(0, 4)), SelectNextOccurrence)) shouldBe List((0, 4, 0, 7))
  }

  it should "do nothing when the caret is in no word" in {
    val before = stateWith("cat  dog", caret(0, 4))

    buffer(reduced(before, SelectNextOccurrence)).editing shouldBe buffer(before).editing
  }

  it should "give every bare caret the word it is in" in {
    val after = reduced(stateWith("cat dog", caret(0, 1), caret(0, 5)), SelectNextOccurrence)

    ranges(after) shouldBe List((0, 0, 0, 3), (0, 4, 0, 7))
  }

  "A second SelectNextOccurrence" should "add the next whole-word match as the newest cursor" in {
    val after = pressed(stateWith("cat concatenate cat", caret(0, 1)), SelectNextOccurrence, SelectNextOccurrence)

    ranges(after) shouldBe List((0, 0, 0, 3), (0, 16, 0, 19))
    buffer(after).editing.cursors.head.position shouldBe CursorPosition(0, 19)
  }

  it should "keep matching whole words on every later press" in {
    val after = pressed(
      stateWith("cat cat concatenate", caret(0, 1)),
      SelectNextOccurrence,
      SelectNextOccurrence,
      SelectNextOccurrence
    )

    ranges(after) shouldBe List((0, 0, 0, 3), (0, 4, 0, 7))
  }

  it should "match inside longer words when the selection was made by the user" in {
    val after = reduced(stateWith("cat concatenate", selecting(0, 0, 0, 3)), SelectNextOccurrence)

    ranges(after) shouldBe List((0, 0, 0, 3), (0, 7, 0, 10))
  }

  it should "wrap round to a match before the selection" in {
    val after = reduced(stateWith("cat dog cat", selecting(0, 8, 0, 11)), SelectNextOccurrence)

    ranges(after) shouldBe List((0, 0, 0, 3), (0, 8, 0, 11))
  }

  it should "search across lines" in {
    val after = reduced(stateWith("cat\ndog\ncat", selecting(0, 0, 0, 3)), SelectNextOccurrence)

    ranges(after) shouldBe List((0, 0, 0, 3), (2, 0, 2, 3))
  }

  it should "match case exactly" in {
    val after = pressed(stateWith("cat Cat cat", caret(0, 1)), SelectNextOccurrence, SelectNextOccurrence)

    ranges(after) shouldBe List((0, 0, 0, 3), (0, 8, 0, 11))
  }

  it should "leave the cursors alone once every match is selected" in {
    val all   = pressed(stateWith("cat dog cat", caret(0, 1)), SelectNextOccurrence, SelectNextOccurrence)
    val after = reduced(all, SelectNextOccurrence)

    ranges(after) shouldBe ranges(all)
    buffer(after).editing.cursors.size shouldBe 2
  }

  private def lookingFor(state: AppState, search: OccurrenceSearch): AppState =
    state.copy(persisted =
      state.persisted.copy(buffers =
        Map(bufferId -> buffer(state).copy(editing = buffer(state).editing.copy(occurrenceSearch = Some(search))))
      )
    )

  it should "keep matching whole words while the selection is still what an earlier press was looking for" in {
    val before = lookingFor(
      stateWith("concatenate cat", selecting(0, 12, 0, 15)),
      OccurrenceSearch("cat", wholeWord = true)
    )

    ranges(reduced(before, SelectNextOccurrence)) shouldBe List((0, 12, 0, 15))
  }

  it should "match plain text again once the selection is not what an earlier press was looking for" in {
    val before = lookingFor(
      stateWith("concatenate cat", selecting(0, 12, 0, 15)),
      OccurrenceSearch("dog", wholeWord = true)
    )

    ranges(reduced(before, SelectNextOccurrence)) shouldBe List((0, 3, 0, 6), (0, 12, 0, 15))
  }

  it should "forget what it was looking for when the cursors move" in {
    val moved = pressed(stateWith("cat cat", caret(0, 1)), SelectNextOccurrence, MoveRight)

    buffer(moved).editing.occurrenceSearch shouldBe None
  }

  "SkipOccurrence" should "replace the newest cursor with the match after it" in {
    val after = pressed(
      stateWith("cat cat cat", caret(0, 1)),
      SelectNextOccurrence,
      SelectNextOccurrence,
      SkipOccurrence
    )

    ranges(after) shouldBe List((0, 0, 0, 3), (0, 8, 0, 11))
    buffer(after).editing.cursors.head.position shouldBe CursorPosition(0, 11)
  }

  it should "move a lone selection on to the next match" in {
    val after = pressed(stateWith("cat cat", caret(0, 1)), SelectNextOccurrence, SkipOccurrence)

    ranges(after) shouldBe List((0, 4, 0, 7))
  }

  it should "do nothing without a selection" in {
    val before = stateWith("cat cat", caret(0, 1))

    buffer(reduced(before, SkipOccurrence)).editing shouldBe buffer(before).editing
  }

  "SelectAllOccurrences" should "select every whole-word match of the word under a bare caret" in {
    val after = reduced(stateWith("cat concatenate cat\ncat", caret(0, 1)), SelectAllOccurrences)

    ranges(after) shouldBe List((0, 0, 0, 3), (0, 16, 0, 19), (1, 0, 1, 3))
    buffer(after).editing.cursors.head.position shouldBe CursorPosition(0, 3)
  }

  it should "select every substring match of a selection the user made" in {
    val after = reduced(stateWith("cat concatenate cat", selecting(0, 0, 0, 3)), SelectAllOccurrences)

    ranges(after) shouldBe List((0, 0, 0, 3), (0, 7, 0, 10), (0, 16, 0, 19))
  }

  it should "replace cursors that were selecting something else" in {
    val after =
      reduced(stateWith("cat dog cat dog", selecting(0, 0, 0, 3), selecting(0, 4, 0, 7)), SelectAllOccurrences)

    ranges(after) shouldBe List((0, 0, 0, 3), (0, 8, 0, 11))
  }

  it should "do nothing when the caret is in no word" in {
    val before = stateWith("cat  dog", caret(0, 4))

    buffer(reduced(before, SelectAllOccurrences)).editing shouldBe buffer(before).editing
  }

  it should "stop at the cursor limit" in {
    val text  = Vector.fill(EditorOccurrenceEventReducer.CursorLimit + 5)("a").mkString(" ")
    val after = reduced(stateWith(text, caret(0, 0)), SelectAllOccurrences)

    buffer(after).editing.cursors.size shouldBe EditorOccurrenceEventReducer.CursorLimit
  }

  "Typing after selecting occurrences" should "replace every selected match" in {
    val selected = pressed(stateWith("cat dog cat bird cat", caret(0, 1)), SelectNextOccurrence, SelectNextOccurrence)

    typed(selected, "fox") shouldBe "fox dog fox bird cat"
  }

  it should "replace every match after selecting all of them" in {
    val selected = reduced(stateWith("cat dog\ncat concatenate cat", caret(0, 1)), SelectAllOccurrences)

    typed(selected, "fox") shouldBe "fox dog\nfox concatenate fox"
  }

  it should "leave one cursor after each replacement" in {
    val selected = reduced(stateWith("cat cat", caret(0, 1)), SelectAllOccurrences)
    val after    = pressed(selected, InsertChar('x'))

    buffer(after).editing.cursorPositions shouldBe List(CursorPosition(0, 1), CursorPosition(0, 3))
    buffer(after).allSelections shouldBe Nil
  }
