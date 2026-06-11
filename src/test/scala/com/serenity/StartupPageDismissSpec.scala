package com.serenity

import com.serenity.keystroke.events.Escape
import com.serenity.rope.Balance
import com.serenity.state.components.{ComponentResult, StartupPageComponent}
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class StartupPageDismissSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  behavior of "StartupPage Dismiss Behavior"

  it should "return ComponentResult.Dismiss when Escape is pressed" in {
    val component = StartupPageComponent()
    val startPage = StartupPage(
      title = "Test",
      options = List("Option 1", "Option 2"),
      selectedIndex = 0
    )
    val surfaceId = SurfaceId("test-surface")
    val state = AppState.empty.copy(
      focus = Focus.Surface(surfaceId),
      uiSurfaces = List(
        UiSurface(
          id = surfaceId,
          content = SurfaceContent.StartPage(startPage),
          presentation = SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
        )
      )
    )

    val result = component.processEvent(Escape, state)

    result shouldBe ComponentResult.Dismiss
  }
