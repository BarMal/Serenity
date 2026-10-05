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
