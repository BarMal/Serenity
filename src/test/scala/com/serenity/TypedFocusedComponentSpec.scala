package com.serenity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import com.serenity.keystroke.events.Event
import com.serenity.rope.Balance
import com.serenity.state.components.{ComponentResult, TypedFocusedComponent}
import com.serenity.state.models.{AppState, Focus, PaneId}

class TypedFocusedComponentSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private case object TypedEvent extends Event
  private case object OtherEvent extends Event

  private class TestComponent extends TypedFocusedComponent[TypedEvent.type]:
    protected def decodeEvent(event: Event): Option[TypedEvent.type] =
      Option.when(event == TypedEvent)(TypedEvent)

    protected def processTypedEvent(event: TypedEvent.type, currentState: AppState): ComponentResult =
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
