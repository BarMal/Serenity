package com.serenity.state.manager

import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.state.undo.{BufferSnapshot, HistoryEntry, UndoState}
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
      AppState.initial.persisted.copy(buffers =
        Map(shown -> Buffer.fromString(shown, "shown"), hidden -> Buffer.fromString(hidden, "hidden"))
      )
    )

  private def edited(bufferId: BufferId, text: String): HistoryEntry.BufferEdit =
    HistoryEntry.BufferEdit(bufferId, paneId, BufferSnapshot.fromBuffer(Buffer.fromString(bufferId, text)))

  private val panelToggle: HistoryEntry = HistoryEntry.PanelChange.capture(state)

  private def text(model: Model, bufferId: BufferId): String =
    model.app.persisted.buffers(bufferId).document.content.collect()

  "Undo" should "never restore an edit to a buffer other than the one being worked in" in {
    val model = Model(state, UndoState().recorded(edited(hidden, "before"), groupable = false))

    UndoRecording.undone(model) shouldBe None
  }

  it should "restore the working buffer's newest edit even when another buffer was edited after it" in {
    val undo  = UndoState().recorded(edited(shown, "before"), groupable = false).recorded(edited(hidden, "x"), false)
    val after = UndoRecording.undone(Model(state, undo)).value

    text(after, shown) shouldBe "before"
    text(after, hidden) shouldBe "hidden"
    after.undo.nextUndo(after.app) shouldBe None
  }

  it should "take a layout change recorded after the buffer's newest edit first" in {
    val undo  = UndoState().recorded(edited(shown, "before"), groupable = false).recorded(panelToggle, false)
    val first = UndoRecording.undone(Model(state, undo)).value

    text(first, shown) shouldBe "shown"
    text(UndoRecording.undone(first).value, shown) shouldBe "before"
  }

  it should "redo into the working buffer only" in {
    val undo   = UndoState().recorded(edited(shown, "before"), groupable = false)
    val undone = UndoRecording.undone(Model(state, undo)).value

    text(UndoRecording.redone(undone).value, shown) shouldBe "shown"
  }

  "Recording" should "bound each history on its own, so layout changes never push out a buffer's edits" in {
    val undo = (1 to 5).foldLeft(UndoState(maxUndoDepth = 2).recorded(edited(shown, "before"), groupable = false))(
      (current, _) => current.recorded(panelToggle, groupable = false)
    )

    undo.layout.undo should have size 2
    undo.buffers.get(shown).map(_.stacks.undo.size) shouldBe Some(1)
  }

  it should "end a typing run once any other history records a step" in {
    val undo = UndoState()
      .recorded(edited(shown, "a"), groupable = true)
      .recorded(edited(hidden, "b"), groupable = false)
      .recorded(edited(shown, "ab"), groupable = true)

    undo.buffers.get(shown).map(_.stacks.undo.size) shouldBe Some(2)
  }

  it should "drop the history of a buffer that is no longer open" in {
    val undo = UndoState().recorded(edited(shown, "a"), groupable = false).recorded(edited(hidden, "b"), false)

    undo.forOpenBuffers(state.persisted.buffers - hidden).buffers.keySet shouldBe Set(shown)
  }

  private def isDirty(model: Model): Boolean = model.app.persisted.buffers(shown).document.isDirty

  private def withShown(model: Model)(update: Document => Document): Model =
    val buffer = model.app.persisted.buffers(shown)
    model.copy(app =
      model.app.copy(persisted =
        model.app.persisted.copy(buffers =
          model.app.persisted.buffers.updated(shown, buffer.copy(document = update(buffer.document)))
        )
      )
    )

  /** `shown` edited from clean, its pre-edit text recorded as the undo step. */
  private val editedFromClean: Model =
    val clean = state.persisted.buffers(shown)
    val entry = HistoryEntry.BufferEdit(shown, paneId, BufferSnapshot.fromBuffer(clean))
    withShown(Model(state, UndoState().recorded(entry, groupable = true)))(_.withContent(com.serenity.rope.Rope("x")))

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
    val run   = UndoState().recorded(edited(shown, "a"), groupable = true)
    val clean = HistoryEntry.BufferEdit(shown, paneId, BufferSnapshot.fromBuffer(Buffer.fromString(shown, "ab")))

    run.recorded(clean, groupable = true).buffers.get(shown).map(_.stacks.undo.size) shouldBe Some(2)
  }
