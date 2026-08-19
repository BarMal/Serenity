package com.serenity

import com.serenity.keystroke.events.{Escape, Event, MoveUp}
import com.serenity.rope.Balance
import com.serenity.state.components.{ComponentResult, TypedFocusedComponent}
import com.serenity.state.models.{AppState, Focus, PaneId}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TypedFocusedComponentSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  // `Event` is a union of the event families, so it cannot be extended and this spec cannot mint throwaway events of
  // its own. Two real events stand in: the machinery under test only cares that one decodes and the other does not.
  private val TypedEvent = MoveUp
  private val OtherEvent = Escape

  private class TestComponent extends TypedFocusedComponent[MoveUp.type]:
    protected def decodeEvent(event: Event): Option[MoveUp.type] =
      Option.when(event == TypedEvent)(MoveUp)

    protected def processTypedEvent(event: MoveUp.type, currentState: AppState): ComponentResult =
      ComponentResult.dismiss

    override protected def processFallbackEvent(event: Event, currentState: AppState): ComponentResult =
      ComponentResult.transferFocus(Focus.EditorPane(PaneId(7)))

  "TypedFocusedComponent" should "delegate decoded events to the typed handler" in {
    val component = TestComponent()

    component.processEvent(TypedEvent, AppState.initial).shouldBe(ComponentResult.Dismiss)
  }

  it should "fall back when an event does not decode" in {
    val component = TestComponent()

    component
      .processEvent(OtherEvent, AppState.initial)
      .shouldBe(
        ComponentResult.FocusTransfer(Focus.EditorPane(PaneId(7)))
      )
  }
