package com.serenity.state.reducers

import com.serenity.keystroke.events.*
import com.serenity.rope.{Balance, Rope}
import com.serenity.state.models.*
import com.serenity.state.undo.BufferSnapshot
import com.serenity.testkit.EditingStateFixtures
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Every edit reducer records the change it made on the document it returns (#1838), exactly: applying it to the text
  * before gives the text after, so no consumer has to compare the two.
  */
class EditChangeSetProducerSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val paneId   = PaneId(0)
  private val bufferId = BufferId(0)

  private def stateOf(text: String, editing: EditingState): AppState =
    val base = AppState.initial.persisted.buffers(bufferId)
    AppState.initial.copy(persisted =
      AppState.initial.persisted.copy(buffers =
        AppState.initial.persisted.buffers
          .updated(bufferId, base.copy(document = base.document.copy(content = Rope(text)), editing = editing))
      )
    )

  private def caretsAt(text: String, carets: CursorPosition*): AppState =
    stateOf(text, EditingStateFixtures(cursors = carets.toList))

  private def selecting(text: String, selections: Selection*): AppState =
    stateOf(text, EditingStateFixtures(selections = selections.toList))

  /** The text the recorded change makes of the old text, and the text the reducer actually produced. */
  private def changedAndActual(start: AppState, event: TextEntryEvent): (String, String) =
    val before  = start.persisted.buffers(bufferId).document
    val after   = EditorEventReducer.reduce(event, paneId, start).state.persisted.buffers(bufferId).document
    val change  = after.changeFrom(before).getOrElse(fail(s"$event recorded no change"))
    val applied = change.applyTo(before.content).getOrElse(fail(s"$event recorded a change for other text"))
    (applied.collect(), after.content.collect())

  private val text = "alpha beta\ngamma delta\nepsilon zeta"

  private val scenarios: List[(String, AppState, TextEntryEvent)] = List(
    ("typing a character", caretsAt(text, CursorPosition(1, 3)), InsertChar('x')),
    (
      "typing over a selection",
      selecting(text, Selection(CursorPosition(0, 2), CursorPosition(1, 4))),
      InsertChar('x')
    ),
    ("a newline", caretsAt(text, CursorPosition(0, 5)), NewLine),
    ("backspace", caretsAt(text, CursorPosition(1, 3)), DeleteBackward),
    ("forward delete", caretsAt(text, CursorPosition(1, 3)), DeleteForward),
    ("deleting a word backward", caretsAt(text, CursorPosition(2, 7)), DeleteWordBackward),
    ("deleting a word forward", caretsAt(text, CursorPosition(0, 0)), DeleteWordForward),
    ("deleting to the line end", caretsAt(text, CursorPosition(1, 2)), DeleteToLineEnd),
    (
      "typing at several carets",
      caretsAt(text, CursorPosition(0, 1), CursorPosition(1, 3), CursorPosition(2, 0)),
      InsertChar('q')
    ),
    ("backspace at several carets", caretsAt(text, CursorPosition(0, 1), CursorPosition(1, 3)), DeleteBackward),
    (
      "typing over several selections",
      selecting(
        text,
        Selection(CursorPosition(0, 0), CursorPosition(0, 5)),
        Selection(CursorPosition(2, 0), CursorPosition(2, 7))
      ),
      InsertChar('z')
    ),
    ("indenting selected lines", selecting(text, Selection(CursorPosition(0, 2), CursorPosition(2, 2))), TabKey),
    ("a tab at several carets", caretsAt(text, CursorPosition(0, 1), CursorPosition(2, 1)), TabKey)
  )

  scenarios.foreach { (name, start, event) =>
    s"A reducer applying $name" should "record the change that takes the old text to the new" in {
      val (byChange, actual) = changedAndActual(start, event)
      byChange shouldBe actual
      actual should not be text
    }
  }

  "A reducer unindenting lines" should "record the change to the lines it dedented" in {
    val indented           = "\talpha\n\tbeta\ngamma"
    val start              = selecting(indented, Selection(CursorPosition(0, 1), CursorPosition(1, 2)))
    val (byChange, actual) = changedAndActual(start, ReverseTabKey)
    byChange shouldBe actual
    actual shouldBe "alpha\nbeta\ngamma"
  }

  "Restoring an undo snapshot" should "record the change from the current text to the restored one" in {
    val earlier  = caretsAt("one two three", CursorPosition(0, 0)).persisted.buffers(bufferId)
    val current  = caretsAt("one 2 three!", CursorPosition(0, 0)).persisted.buffers(bufferId)
    val restored = BufferSnapshot.fromBuffer(earlier).restoreInto(current)

    restored.document.content.collect() shouldBe "one two three"
    val change = restored.document.changeFrom(current.document).getOrElse(fail("restore recorded no change"))
    change.applyTo(current.document.content).map(_.collect()) shouldBe Some("one two three")
  }
