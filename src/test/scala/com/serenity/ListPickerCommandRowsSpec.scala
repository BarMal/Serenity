package com.serenity

import com.serenity.command.{Command, CommandIntent, SessionIntent}
import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.session.SessionId
import com.serenity.state.models.*
import com.serenity.state.reducers.ModalEventReducer
import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #2577 P1b: a picker choice can carry a key hint and a reason it can't run, so the command picker can show what the
  * palette shows today -- the key on the right, a muted description, and a greyed row that says why it is refused.
  */
class ListPickerCommandRowsSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val frame = LayoutRect(0, 0, 60, 14)

  private def command(name: String): Command =
    Command.typed(name, name, CommandIntent.Session(SessionIntent.OpenNamedSession(SessionId(name))))

  private val bound = ListChoice("Save", Some("Save the current file."), command("save"), hint = Some("ctrl+s"))

  private val disabled =
    ListChoice(
      "Cut to Darlings",
      Some("Cut the selection."),
      command("cut"),
      disabledReason = Some("Needs a selection.")
    )

  private val plain = ListChoice("Draft", Some("today"), command("draft"))

  private def planFor(picker: ListPicker): ResolvedSurfaceComposition =
    ModalSurfaceComposition
      .forModal(Modal.ListPicker(picker), frame, 1)
      .getOrElse(fail("expected a composition"))

  private def choiceBox(picker: ListPicker, label: String): SurfacePaintBox =
    planFor(picker).paintBoxes
      .find(_.segments.headOption.exists(_.text == label))
      .orElse(planFor(picker).paintBoxes.find(_.text.exists(_.startsWith(label))))
      .getOrElse(fail(s"no row for $label"))

  private def stateWith(picker: ListPicker): AppState =
    AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(focus = Focus.Surface(SurfaceId("picker"))),
      runtime = AppState.initial.runtime.copy(
        uiSurfaces = List(
          UiSurface(
            SurfaceId("picker"),
            SurfaceContent.ModalWorkflow(Modal.ListPicker(picker)),
            SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
          )
        )
      )
    )

  private def shown(state: AppState): Option[ListPicker] =
    state.modalSurface.map(_.content).collect {
      case SurfaceContent.ModalWorkflow(Modal.ListPicker(picker)) =>
        picker
    }

  "A choice with a key hint" should "paint its label, a muted detail and the key, in columns" in {
    val box = choiceBox(ListPicker.of("Commands", List(bound), "None"), "Save")

    box.layout shouldBe SurfacePaintLayout.Columns
    box.segments.map(segment => segment.text -> segment.tone) shouldBe List(
      "Save"                   -> OverlayTone.Normal,
      "Save the current file." -> OverlayTone.Muted,
      "ctrl+s"                 -> OverlayTone.Normal
    )
  }

  "A disabled choice" should "paint a muted label and its reason in place of its detail" in {
    val box = choiceBox(ListPicker.of("Commands", List(disabled), "None"), "Cut to Darlings")

    box.segments.map(segment => segment.text -> segment.tone) shouldBe List(
      "Cut to Darlings"    -> OverlayTone.Muted,
      "Needs a selection." -> OverlayTone.Muted
    )
  }

  "A plain choice" should "keep its one-line row" in {
    planFor(ListPicker.of("Open session", List(plain), "None")).paintBoxes.flatMap(_.text) shouldBe
      List("Open session", "Draft  today")
  }

  "Enter on a disabled choice" should "keep the picker open, run nothing and say why" in {
    val result = ModalEventReducer.reduce(
      ModalType.ListPicker,
      Enter,
      stateWith(ListPicker.of("Commands", List(disabled), "None"))
    )

    result.effects shouldBe Nil
    shown(result.state).flatMap(_.message) shouldBe Some("Needs a selection.")
  }

  it should "show the reason as a muted row under the choices, and count it in the frame height" in {
    val refused = ListPicker.of("Commands", List(disabled, bound), "None").copy(message = Some("Needs a selection."))
    val boxes   = planFor(refused).paintBoxes

    boxes.lastOption.flatMap(_.text) shouldBe Some("Needs a selection.")
    boxes.lastOption.map(_.tone) shouldBe Some(OverlayTone.Muted)
    ListPickerComposition.frameHeight(refused, ModalWrap.ofContent(58, TextMeasure.Cells)) shouldBe
      ListPickerComposition.frameHeight(refused.copy(message = None), ModalWrap.ofContent(58, TextMeasure.Cells)) + 1
  }

  "Moving off a refused choice" should "clear the reason" in {
    val refused = ModalEventReducer
      .reduce(ModalType.ListPicker, Enter, stateWith(ListPicker.of("Commands", List(disabled, bound), "None")))
      .state

    val moved = ModalEventReducer.reduce(ModalType.ListPicker, MoveDown, refused)

    shown(moved.state).flatMap(_.message) shouldBe None
  }

  "Enter on an enabled choice" should "still close the picker and run it" in {
    val result =
      ModalEventReducer.reduce(ModalType.ListPicker, Enter, stateWith(ListPicker.of("Commands", List(bound), "None")))

    result.state.modalSurface shouldBe None
    result.effects.size shouldBe 1
  }

  "A picker with key hints" should "paint them as the last, muted row, under any refusal reason" in {
    val hinted = ListPicker
      .of("Commands", List(disabled, bound), "None")
      .copy(message = Some("Needs a selection."), footer = Some("↑↓ move • Enter run • Esc close"))
    val texts = planFor(hinted).paintBoxes.flatMap(_.text)

    texts.takeRight(2) shouldBe List("Needs a selection.", "↑↓ move • Enter run • Esc close")
    planFor(hinted).paintBoxes.lastOption.map(_.tone) shouldBe Some(OverlayTone.Muted)
    ListPickerComposition.frameHeight(hinted, ModalWrap.ofContent(58, TextMeasure.Cells)) shouldBe
      ListPickerComposition.frameHeight(hinted.copy(footer = None), ModalWrap.ofContent(58, TextMeasure.Cells)) + 1
  }

  it should "drop them when nothing matches, since there is no row to move to or run" in {
    val empty = ListPicker.of("Commands", Nil, "No matching commands").copy(footer = Some("↑↓ move • Enter run"))

    planFor(empty).paintBoxes.flatMap(_.text) should not contain "↑↓ move • Enter run"
    ListPickerComposition.frameHeight(empty, ModalWrap.ofContent(58, TextMeasure.Cells)) shouldBe
      ListPickerComposition.frameHeight(empty.copy(footer = None), ModalWrap.ofContent(58, TextMeasure.Cells))
  }
