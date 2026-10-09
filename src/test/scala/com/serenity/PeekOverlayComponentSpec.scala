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
      persisted = AppState.initial.persisted.copy(
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = Layout(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(TestWorkspaceTrees.linear(paneId))
        ),
        focus = Focus.Surface(SurfaceId("peek"))
      ),
      runtime = AppState.initial.runtime.copy(
        uiSurfaces = List(
          UiSurface(
            SurfaceId("peek"),
            SurfaceContent.QuickInfo("hint"),
            SurfacePresentation.Floating(None, SurfacePlacement.AboveCursor)
          )
        )
      )
    )

  private val dismissedAndPassedOn = ComponentResult.composite(ComponentResult.dismiss, ComponentResult.unhandled)

  "PeekOverlayComponent" should "dismiss on typed navigation events and pass them on to the editor" in {
    val component = PeekOverlayComponent()

    component.processEvent(PeekInputEvent.Navigate(Direction.Up), baseState).shouldBe(dismissedAndPassedOn)
  }

  it should "dismiss on other local input events and pass them on to the editor" in {
    val component = PeekOverlayComponent()

    component.processEvent(PeekInputEvent.OtherInput, baseState).shouldBe(dismissedAndPassedOn)
    component.processEvent(PeekInputEvent.Accept, baseState).shouldBe(dismissedAndPassedOn)
  }

  it should "only dismiss on Dismiss" in
    PeekOverlayComponent().processEvent(PeekInputEvent.Dismiss, baseState).shouldBe(ComponentResult.Dismiss)
