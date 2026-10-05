package com.serenity.state.manager

import scala.concurrent.duration.DurationInt

import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.state.undo.{BufferSnapshot, CaretMarks, EditGrouping, EditKind, HistoryEntry, UndoState}
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The pure undo bookkeeping behind Ctrl+Z (#1930): which history an undo acts on, and how recorded steps are kept. */
class UndoRecordingSpec extends AnyFlatSpec with Matchers with OptionValues:

  given Balance = Balance.default

  private val shown  = BufferId(0)
  private val hidden = BufferId(1)
  private val paneId = PaneId(0)

  /** `shown` in the focused pane, `hidden` open but in no pane. */
  private val state: AppState =
    AppState.initial.copy(persisted =
      AppState.initial.persisted
        .copy(buffers = Map(shown -> Buffer.fromString(shown, "shown"), hidden -> Buffer.fromString(hidden, "hidden")))
    )

  private def carets(line: Int, column: Int): CaretMarks =
    CaretMarks.of(EditingState(List(CursorPosition(line, column))))

  private val atStart = carets(0, 0)

  extension (undo: UndoState)

    private def standalone(entry: HistoryEntry): UndoState =
      undo.recorded(entry, EditGrouping.Standalone, atStart, paused = false)

    private def typed(entry: HistoryEntry): UndoState =
      undo.recorded(entry, EditGrouping.Coalescing(EditKind.Typing, afterWord = false), atStart, paused = false)

  /** An edit's pre-edit snapshot, taken mid-edit so the buffer was already unsaved, with the cursor at `cursor`. */
  private def edited(
    bufferId: BufferId,
    text: String,
    cursor: CursorPosition = CursorPosition(0, 0)
  ): HistoryEntry.BufferEdit =
    val buffer = Buffer.fromString(bufferId, text)
    HistoryEntry.BufferEdit(
      bufferId,
      paneId,
      BufferSnapshot.fromBuffer(
        buffer.copy(document = buffer.document.copy(isDirty = true), editing = EditingState(List(cursor)))
      )
    )

  private val panelToggle: HistoryEntry = HistoryEntry.PanelChange.capture(state)

  private def text(model: Model, bufferId: BufferId): String =
    model.app.persisted.buffers(bufferId).document.content.collect()

  "Undo" should "never restore an edit to a buffer other than the one being worked in" in {
    val model = Model(state, UndoState().standalone(edited(hidden, "before")))

    UndoRecording.undone(model) shouldBe None
  }

  it should "restore the working buffer's newest edit even when another buffer was edited after it" in {
    val undo  = UndoState().standalone(edited(shown, "before")).standalone(edited(hidden, "x"))
    val after = UndoRecording.undone(Model(state, undo)).value

    text(after, shown) shouldBe "before"
    text(after, hidden) shouldBe "hidden"
    after.undo.nextUndo(after.app) shouldBe None
  }

  it should "take a layout change recorded after the buffer's newest edit first" in {
    val undo  = UndoState().standalone(edited(shown, "before")).standalone(panelToggle)
    val first = UndoRecording.undone(Model(state, undo)).value

    text(first, shown) shouldBe "shown"
    text(UndoRecording.undone(first).value, shown) shouldBe "before"
  }

  it should "redo into the working buffer only" in {
    val undo   = UndoState().standalone(edited(shown, "before"))
    val undone = UndoRecording.undone(Model(state, undo)).value

    text(UndoRecording.redone(undone).value, shown) shouldBe "shown"
  }

  "Recording" should "bound each history on its own, so layout changes never push out a buffer's edits" in {
    val undo = (1 to 5).foldLeft(UndoState(maxUndoDepth = 2).standalone(edited(shown, "before")))((current, _) =>
      current.standalone(panelToggle)
    )

    undo.layout.undo should have size 2
    undo.buffers.get(shown).map(_.stacks.undo.size) shouldBe Some(1)
  }

  it should "end a typing run once any other history records a step" in {
    val undo = UndoState()
      .typed(edited(shown, "a"))
      .standalone(edited(hidden, "b"))
      .typed(edited(shown, "ab"))

    undo.buffers.get(shown).map(_.stacks.undo.size) shouldBe Some(2)
  }

  it should "drop the history of a buffer that is no longer open" in {
    val undo = UndoState().standalone(edited(shown, "a")).standalone(edited(hidden, "b"))

    undo.forOpenBuffers(state.persisted.buffers - hidden).buffers.keySet shouldBe Set(shown)
  }

  private def isDirty(model: Model): Boolean = model.app.persisted.buffers(shown).document.isDirty

  private def withShown(model: Model)(update: Document => Document): Model =
    val buffer = model.app.persisted.buffers(shown)
    model.copy(app =
      model.app.copy(persisted =
        model.app.persisted
          .copy(buffers = model.app.persisted.buffers.updated(shown, buffer.copy(document = update(buffer.document))))
      )
    )

  /** `shown` edited from clean, its pre-edit text recorded as the undo step. */
  private val editedFromClean: Model =
    val clean = state.persisted.buffers(shown)
    val entry = HistoryEntry.BufferEdit(shown, paneId, BufferSnapshot.fromBuffer(clean))
    withShown(Model(state, UndoState().typed(entry)))(_.withContent(com.serenity.rope.Rope("x")))

  "Undo to the saved text" should "leave the buffer clean, and redo past it dirty again" in {
    val undone = UndoRecording.undone(editedFromClean).value

    isDirty(undone) shouldBe false
    isDirty(UndoRecording.redone(undone).value) shouldBe true
  }

  it should "leave the buffer dirty once it has been saved since the step was recorded" in {
    val savedSince = withShown(editedFromClean)(_.markedSaved.withContent(com.serenity.rope.Rope("y")))

    isDirty(UndoRecording.undone(savedSince).value) shouldBe true
  }

  it should "leave the buffer dirty after a change undo does not record, such as a comment edit" in {
    val commented = withShown(editedFromClean)(_.withUnrecordedChange)

    isDirty(UndoRecording.undone(commented).value) shouldBe true
  }

  "Recording" should "start a new step at a clean buffer even inside a typing run" in {
    val run   = UndoState().typed(edited(shown, "a"))
    val clean = HistoryEntry.BufferEdit(shown, paneId, BufferSnapshot.fromBuffer(Buffer.fromString(shown, "ab")))

    run.typed(clean).buffers.get(shown).map(_.stacks.undo.size) shouldBe Some(2)
  }

  private val typing    = EditGrouping.Coalescing(EditKind.Typing, afterWord = false)
  private val backspace = EditGrouping.Coalescing(EditKind.DeletingBackward, afterWord = false)

  private def undoSteps(undo: UndoState): Int = undo.buffers.get(shown).map(_.stacks.undo.size).getOrElse(0)

  /** `grouping` recorded for an edit that began with the cursor at `column` and left it at `leftAt`. */
  private def edit(
    undo: UndoState,
    grouping: EditGrouping,
    column: Int,
    leftAt: Int,
    paused: Boolean = false
  ): UndoState =
    undo.recorded(edited(shown, "text", CursorPosition(0, column)), grouping, carets(0, leftAt), paused)

  "A typing run" should "keep consecutive keystrokes in one step" in {
    val undo = edit(edit(edit(UndoState(), typing, 0, 1), typing, 1, 2), typing, 2, 3)

    undoSteps(undo) shouldBe 1
  }

  it should "end when the cursor was moved before the next keystroke" in {
    undoSteps(edit(edit(UndoState(), typing, 0, 1), typing, 7, 8)) shouldBe 2
  }

  it should "end when a selection was made before the next keystroke" in {
    val selected = EditingState.fromCursors(List(Cursor(CursorPosition(0, 1), Some(CursorPosition(0, 0)))))
    val entry =
      HistoryEntry.BufferEdit(
        shown,
        paneId,
        BufferSnapshot.fromBuffer(Buffer.fromString(shown, "t").copy(editing = selected))
      )
    val run = edit(UndoState(), typing, 0, 1).recorded(entry, typing, carets(0, 2), paused = false)

    undoSteps(run) shouldBe 2
  }

  it should "end before whitespace typed straight after a word, which opens the next step" in {
    val separator = EditGrouping.Coalescing(EditKind.Typing, afterWord = true)
    val undo      = edit(edit(edit(UndoState(), typing, 0, 1), separator, 1, 2), typing, 2, 3)

    undoSteps(undo) shouldBe 2
  }

  it should "end when the buffer was left alone for over a second before the next keystroke" in {
    val undo = edit(UndoState(), typing, 0, 1)

    undoSteps(edit(undo, typing, 1, 2)) shouldBe 1
    undoSteps(edit(undo, typing, 1, 2, paused = true)) shouldBe 2
  }

  it should "end when the kind of edit changes, and run on through consecutive deletions" in {
    val deleting = edit(edit(edit(UndoState(), typing, 0, 5), backspace, 5, 4), backspace, 4, 3)

    undoSteps(deleting) shouldBe 2
    undoSteps(edit(deleting, typing, 3, 4)) shouldBe 3
  }

  it should "never take in a standalone edit, nor continue past one" in {
    val undo = edit(UndoState(), typing, 0, 1).recorded(
      edited(shown, "text", CursorPosition(0, 1)),
      EditGrouping.Standalone,
      carets(0, 2),
      paused = false
    )

    undoSteps(undo) shouldBe 2
    undoSteps(edit(undo, typing, 2, 3)) shouldBe 3
  }

  private def clockedAt(model: Model, nanos: Long): Model =
    val runtime = model.app.runtime
    model.copy(app = model.app.copy(runtime = runtime.copy(editClock = runtime.editClock.observed(nanos))))

  "Typing recorded against the model's edit clock" should "end a run left alone for over a second" in {
    val first = UndoRecording.recorded(clockedAt(Model(state, UndoState()), 0L), edited(shown, "a"), typing)

    val soon = UndoRecording.recorded(clockedAt(first, 900.millis.toNanos), edited(shown, "ab"), typing)
    val late = UndoRecording.recorded(clockedAt(first, 1100.millis.toNanos), edited(shown, "ab"), typing)

    (undoSteps(soon.undo), undoSteps(late.undo)) shouldBe (1, 2)
  }

  it should "measure the pause from the previous keystroke, not from where the run began" in {
    val gap = 900.millis.toNanos
    val run = (0 until 5).foldLeft(Model(state, UndoState())) { (model, index) =>
      UndoRecording.recorded(clockedAt(model, index * gap), edited(shown, "text"), typing)
    }

    undoSteps(run.undo) shouldBe 1
  }
