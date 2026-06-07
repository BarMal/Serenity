package com.serenity

import com.serenity.keystroke.events.{Direction, PeekInputEvent}
import com.serenity.rope.Balance
import com.serenity.state.components.{ComponentResult, PeekOverlayComponent}
import com.serenity.state.models.*
import com.serenity.ui.layout.Layout
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PeekOverlayComponentSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private def baseState: AppState =
    val paneId   = PaneId(0)
    val bufferId = BufferId(1)
    val buffer   = Buffer.fromString(bufferId, "hello")
    AppState.initial.copy(
      buffers = Map(bufferId -> buffer),
      bufferOrder = List(bufferId),
      layout = Layout(
        editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
        activeEditorPaneId = Some(paneId)
      ),
      uiSurfaces = List(
        UiSurface(
          SurfaceId("peek"),
          SurfaceContent.QuickInfo("hint"),
          SurfacePresentation.Floating(None, SurfacePlacement.AboveCursor)
        )
      ),
      focus = Focus.Surface(SurfaceId("peek"))
    )

  "PeekOverlayComponent" should "dismiss on typed navigation events" in {
    val component = PeekOverlayComponent()

    component.processEvent(PeekInputEvent.Navigate(Direction.Up), baseState).shouldBe(ComponentResult.Dismiss)
  }

  it should "dismiss on other local input events" in {
    val component = PeekOverlayComponent()

    component.processEvent(PeekInputEvent.OtherInput, baseState).shouldBe(ComponentResult.Dismiss)
    component.processEvent(PeekInputEvent.Accept, baseState).shouldBe(ComponentResult.Dismiss)
    component.processEvent(PeekInputEvent.Dismiss, baseState).shouldBe(ComponentResult.Dismiss)
  }
