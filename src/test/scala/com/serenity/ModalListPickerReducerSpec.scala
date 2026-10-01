package com.serenity

import com.serenity.command.{Command, CommandIntent, SessionIntent}
import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.session.SessionId
import com.serenity.state.models.*
import com.serenity.state.reducers.{AppEffect, ModalEventReducer}
import com.serenity.ui.layout.ListPickerComposition
import com.serenity.ui.widget.Loadable
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A [[ListPicker]]: arrows move, Enter runs the highlighted choice's command, and a choice that waits on slow work
  * keeps the picker open, showing it, until that work lands or Escape abandons it.
  */
class ModalListPickerReducerSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private def command(name: String): Command =
    Command.typed(name, name, CommandIntent.Session(SessionIntent.OpenNamedSession(SessionId(name))))

  private val quick = ListChoice("Quick", None, command("quick"))
  private val slow  = ListChoice("Slow", Some("takes a while"), command("slow"), waitingLabel = Some("Opening Slow…"))

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
    state.modalSurface
      .map(_.content)
      .collect { case SurfaceContent.ModalWorkflow(Modal.ListPicker(picker)) => picker }

  private val ready = ListPicker.of("Pick one", List(quick, slow), emptyMessage = "Nothing to pick")

  "A list picker" should "close and run a choice that doesn't wait" in {
    val result = ModalEventReducer.reduce(ModalType.ListPicker, Enter, stateWith(ready))

    result.state.modalSurface shouldBe None
    result.effects shouldBe List(AppEffect.ExecuteCommand(quick.action))
  }

  it should "stay open on a choice that waits, showing it as pending, and ignore Enter until it lands" in {
    val onSlow = ModalEventReducer.reduce(ModalType.ListPicker, MoveDown, stateWith(ready)).state
    val picked = ModalEventReducer.reduce(ModalType.ListPicker, Enter, onSlow)

    shown(picked.state).flatMap(_.pending) shouldBe Some(slow)
    picked.effects shouldBe List(AppEffect.ExecuteCommand(slow.action))
    ModalEventReducer.reduce(ModalType.ListPicker, Enter, picked.state).effects shouldBe Nil
  }

  it should "wrap its highlight and take a clicked row" in {
    shown(ModalEventReducer.reduce(ModalType.ListPicker, MoveUp, stateWith(ready)).state)
      .flatMap(_.selectedChoice) shouldBe Some(slow)
    val click = ModalClick(
      ListPickerComposition.choiceActionId(1).value,
      Some(ListPickerComposition.choiceActionId(1).value)
    )
    shown(ModalEventReducer.reduce(ModalType.ListPicker, click, stateWith(ready)).state)
      .flatMap(_.selectedChoice) shouldBe Some(slow)
  }

  it should "pick nothing while its choices are still loading, and close on Escape" in {
    val loading = stateWith(ListPicker.loading("Pick one"))

    ModalEventReducer.reduce(ModalType.ListPicker, Enter, loading).effects shouldBe Nil
    ModalEventReducer.reduce(ModalType.ListPicker, Escape, loading).state.modalSurface shouldBe None
  }

  it should "say so when there is nothing to pick" in {
    ListPicker.of("Pick one", Nil, emptyMessage = "Nothing to pick").items shouldBe Loadable.Empty("Nothing to pick")
  }
