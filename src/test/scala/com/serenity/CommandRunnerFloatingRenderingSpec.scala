package com.serenity

import cats.effect.IO
import com.serenity.command.{Command, CommandCategory, CommandRegistry, CommandRunner}
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.{CursorLayout, Layout, LayoutEngine, ViewportSize}
import com.serenity.ui.renderer.Renderer
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CommandRunnerFloatingRenderingSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val paneId   = PaneId(0)
  private val bufferId = BufferId(1)

  private def stateWithRunner(theme: Theme, searchTerm: String, commands: List[Command]): AppState =
    val registry = CommandRegistry(commands)
    val runner = CommandRunner.empty
      .activate(registry)
      .updateSearchTerm(searchTerm)(using registry)
    val buffer = Buffer.fromString(bufferId, "alpha\nbeta\ngamma").copy(
      cursors = List(CursorPosition(1, 2))
    )
    val pane = EditorPane.withBuffer(paneId, bufferId)

    AppState.initial.copy(
      buffers = Map(bufferId -> buffer),
      bufferOrder = List(bufferId),
      layout = Layout(
        editorPanes = Map(paneId -> pane),
        activeEditorPaneId = Some(paneId)
      ),
      focus = Focus.Surface(SurfaceId("command-runner")),
      theme = theme,
      uiSurfaces = List(
        UiSurface(
          SurfaceId("command-runner"),
          SurfaceContent.CommandPalette(runner),
          SurfacePresentation.Floating(Some(CursorPosition(1, 2)), SurfacePlacement.BelowCursor)
        )
      )
    )

  "Renderer.render" should "paint a themed command runner with descriptions, selection highlight, and visible search cursor" in {
    val commands = List(
      Command("open", "Open file", _ => IO.unit),
      Command("close", "Close current file", _ => IO.unit)
    )
    val state   = stateWithRunner(Theme.light, "op", commands)
    val surface = new MockRenderSurface(100, 30)
    val layout  = LayoutEngine.calculateLayout(state, ViewportSize(100, 30))
    val overlay = layout.belowCursorOverlayRect.getOrElse(fail("Expected below-cursor overlay rect"))
    val paneRect = LayoutEngine
      .calculatePaneLayouts(state, layout)
      .getOrElse(paneId, fail("Expected pane layout"))
    val contentRect = CursorLayout.contentRectForPane(paneRect)

    Renderer.render(state, cursorVisible = true, surface, ViewportSize(100, 30))

    val searchLine =
      (overlay.x + 1 until overlay.right - 1).map(x => surface.getChar(x, overlay.y + 1)).mkString.trim
    val commandLine =
      (overlay.x + 1 until overlay.right - 1).map(x => surface.getChar(x, overlay.y + 2)).mkString.trim

    searchLine should include("search: op")
    commandLine should include("open")
    commandLine should include("Open file")
    overlay.width shouldBe contentRect.width
    overlay.x shouldBe contentRect.x

    surface.getBg(0, 0) shouldBe state.theme.background
    surface.getBg(overlay.x + 1, overlay.y + 1) shouldBe state.theme.panel.background
    surface.getBg(overlay.x + 1, overlay.y + 2) shouldBe state.theme.highlighted.background

    val cursorX = overlay.x + 1 + "search: op".length
    surface.getBg(cursorX, overlay.y + 1) shouldBe state.theme.cursorColor
  }

  it should "render category tabs in browse mode and show inline animation options in settings" in {
    val commands = List(
      Command.typed("open", "Open file", com.serenity.command.CommandIntent.OpenFile),
      Command.typed("toggle-theme", "Switch between light and dark theme", com.serenity.command.CommandIntent.ToggleTheme)
    )
    val registry = CommandRegistry(commands)
    given CommandRegistry = registry
    val runner = CommandRunner.empty
      .activate(registry)
      .withActiveCategory(CommandCategory.Settings)
    val buffer = Buffer.fromString(bufferId, "alpha\nbeta\ngamma").copy(
      cursors = List(CursorPosition(1, 2))
    )
    val pane = EditorPane.withBuffer(paneId, bufferId)
    val state = AppState.initial.copy(
      buffers = Map(bufferId -> buffer),
      bufferOrder = List(bufferId),
      layout = Layout(
        editorPanes = Map(paneId -> pane),
        activeEditorPaneId = Some(paneId)
      ),
      focus = Focus.Surface(SurfaceId("command-runner")),
      theme = Theme.light,
      uiSurfaces = List(
        UiSurface(
          SurfaceId("command-runner"),
          SurfaceContent.CommandPalette(runner),
          SurfacePresentation.Floating(Some(CursorPosition(1, 2)), SurfacePlacement.BelowCursor)
        )
      )
    )
    val surface = new MockRenderSurface(100, 30)
    val layout  = LayoutEngine.calculateLayout(state, ViewportSize(100, 30))
    val overlay = layout.belowCursorOverlayRect.getOrElse(fail("Expected below-cursor overlay rect"))

    Renderer.render(state, cursorVisible = true, surface, ViewportSize(100, 30))

    val tabLine =
      (overlay.x + 1 until overlay.right - 1).map(x => surface.getChar(x, overlay.y + 1)).mkString.trim
    val optionLine =
      (overlay.x + 1 until overlay.right - 1).map(x => surface.getChar(x, overlay.y + 2)).mkString.trim

    tabLine should include("All")
    tabLine should include("File")
    tabLine should include("View")
    tabLine should include("Edit")
    tabLine should include("Settings")
    tabLine should not include "["
    tabLine.indexOf("Settings") should be > tabLine.length / 2
    optionLine should include("Animation")
    optionLine should include("Mode")
    optionLine should include("Full")
    optionLine should not include "["

    val settingsBackgrounds =
      (overlay.x + 1 until overlay.right - 1)
        .map(x => surface.getBg(x, overlay.y + 1))
        .distinct
    settingsBackgrounds.size should be > 1
  }
end CommandRunnerFloatingRenderingSpec
