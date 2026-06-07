package com.serenity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.components.{ComponentResult, StartupPageComponent}
import com.serenity.state.models.*

class StartupPageComponentSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private def createStartupPageState(selectedIndex: Int = 0): AppState =
    val startPage = StartupPage(
      title = "What would you like to do?",
      options = List(
        "1. Start a new session",
        "2. Restore an existing session",
        "3. Open an existing file or directory"
      ),
      selectedIndex = selectedIndex
    )
    val surfaceId = SurfaceId("surface-0")
    AppState.empty.copy(
      focus = Focus.Surface(surfaceId),
      uiSurfaces = List(
        UiSurface(
          id = surfaceId,
          content = SurfaceContent.StartPage(startPage),
          presentation = SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
        )
      )
    )

  behavior of "StartupPageComponent"

  it should "handle down arrow navigation to move selection down" in {
    val component    = StartupPageComponent()
    val initialState = createStartupPageState(selectedIndex = 0)

    val result = component.processEvent(MoveDown, initialState)

    result match
      case ComponentResult.StateChange(updateFn) =>
        val updatedState     = updateFn(initialState)
        val startPageSurface = updatedState.startPageSurface.get
        val startPage        = startPageSurface.content.asInstanceOf[SurfaceContent.StartPage].page
        startPage.selectedIndex shouldBe 1
      case _ => fail(s"Expected StateChange, got $result")
  }

  it should "handle up arrow navigation to move selection up" in {
    val component    = StartupPageComponent()
    val initialState = createStartupPageState(selectedIndex = 1)

    val result = component.processEvent(MoveUp, initialState)

    result match
      case ComponentResult.StateChange(updateFn) =>
        val updatedState     = updateFn(initialState)
        val startPageSurface = updatedState.startPageSurface.get
        val startPage        = startPageSurface.content.asInstanceOf[SurfaceContent.StartPage].page
        startPage.selectedIndex shouldBe 0
      case _ => fail(s"Expected StateChange, got $result")
  }

  it should "wrap around to last option when moving up from first option" in {
    val component    = StartupPageComponent()
    val initialState = createStartupPageState(selectedIndex = 0)

    val result = component.processEvent(MoveUp, initialState)

    result match
      case ComponentResult.StateChange(updateFn) =>
        val updatedState     = updateFn(initialState)
        val startPageSurface = updatedState.startPageSurface.get
        val startPage        = startPageSurface.content.asInstanceOf[SurfaceContent.StartPage].page
        startPage.selectedIndex shouldBe 2 // Should wrap to last option
      case _ => fail(s"Expected StateChange, got $result")
  }

  it should "wrap around to first option when moving down from last option" in {
    val component    = StartupPageComponent()
    val initialState = createStartupPageState(selectedIndex = 2)

    val result = component.processEvent(MoveDown, initialState)

    result match
      case ComponentResult.StateChange(updateFn) =>
        val updatedState     = updateFn(initialState)
        val startPageSurface = updatedState.startPageSurface.get
        val startPage        = startPageSurface.content.asInstanceOf[SurfaceContent.StartPage].page
        startPage.selectedIndex shouldBe 0 // Should wrap to first option
      case _ => fail(s"Expected StateChange, got $result")
  }

  it should "execute new session command when Enter is pressed on first option" in {
    val component    = StartupPageComponent()
    val initialState = createStartupPageState(selectedIndex = 0)

    val result = component.processEvent(Enter, initialState)

    result match
      case ComponentResult.ExecuteCommand(command) =>
        command.name shouldBe "startup.new-session"
      case _ => fail(s"Expected ExecuteCommand, got $result")
  }

  it should "execute restore session command when Enter is pressed on second option" in {
    val component    = StartupPageComponent()
    val initialState = createStartupPageState(selectedIndex = 1)

    val result = component.processEvent(Enter, initialState)

    result match
      case ComponentResult.ExecuteCommand(command) =>
        command.name shouldBe "startup.restore-session"
      case _ => fail(s"Expected ExecuteCommand, got $result")
  }

  it should "execute open file command when Enter is pressed on third option" in {
    val component    = StartupPageComponent()
    val initialState = createStartupPageState(selectedIndex = 2)

    val result = component.processEvent(Enter, initialState)

    result match
      case ComponentResult.ExecuteCommand(command) =>
        command.name shouldBe "startup.open-file"
      case _ => fail(s"Expected ExecuteCommand, got $result")
  }

  it should "dismiss startup page when Escape is pressed" in {
    val component    = StartupPageComponent()
    val initialState = createStartupPageState(selectedIndex = 1)

    val result = component.processEvent(Escape, initialState)

    result shouldBe ComponentResult.Dismiss
  }

  it should "ignore unhandled events" in {
    val component    = StartupPageComponent()
    val initialState = createStartupPageState(selectedIndex = 0)

    val result = component.processEvent(MoveLeft, initialState)

    result shouldBe ComponentResult.NoChange
  }

  it should "ignore text input events" in {
    val component    = StartupPageComponent()
    val initialState = createStartupPageState(selectedIndex = 0)

    val result = component.processEvent(InsertChar('a'), initialState)

    result shouldBe ComponentResult.NoChange
  }
