package com.serenity

import java.awt.Font

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.command.*
import com.serenity.config.AppConfig
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.layout.*
import com.serenity.ui.presets.UiPreset
import com.serenity.ui.renderer.RendererEntryPoints
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

class CommandRunnerPaletteContentRenderingSpec extends AnyFlatSpec with Matchers:

  given Balance    = Balance.default
  given Logger[IO] = Slf4jLogger.getLogger[IO]

  private val paneId   = PaneId(0)
  private val bufferId = BufferId(1)

  private val codeFont = FontLoader
    .loadCodeFont(FontLoader.FontConfig(codeFontFamily = FontLoader.BundledCodeFontFamily, enableLigatures = true))
    .unsafeRunSync()

  private val cellMetrics = CellMetrics.fromFont(codeFont)

  private def singlePaneLayout: Layout =
    Layout(
      editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
      activeEditorPaneId = Some(paneId),
      workspaceTree = Some(TestWorkspaceTrees.linear(paneId))
    )

  private def stateWithRunner(
    theme: Theme,
    searchTerm: String,
    commands: List[Command],
    cursors: List[CursorPosition] = List(CursorPosition(1, 2))
  ): AppState =
    val registry = CommandRegistry(commands)
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default)
      .updateSearchTerm(searchTerm)(using registry)
    val buffer = Buffer
      .fromString(bufferId, "alpha\nbeta\ngamma")
      .copy(
        editing = EditingState(cursors = cursors)
      )

    val initialState = AppState.initial
    initialState.copy(
      persisted = initialState.persisted.copy(
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = singlePaneLayout,
        focus = Focus.Surface(SurfaceId("command-runner")),
        theme = theme
      ),
      runtime = initialState.runtime.copy(
        uiSurfaces = List(
          UiSurface(
            SurfaceId("command-runner"),
            SurfaceContent.CommandPalette(runner),
            SurfacePresentation.Floating(Some(CursorPosition(1, 2)), SurfacePlacement.BelowCursor)
          )
        )
      )
    )
  "RendererEntryPoints.render" should "paint a themed command runner with descriptions, selection highlight, and visible search cursor" in {
    val commands = List(
      Command.typed("open", "Open file", CommandIntent.File(FileIntent.OpenFile)),
      Command.typed("close", "Close current file", CommandIntent.File(FileIntent.CloseCurrentFile))
    )
    val state   = stateWithRunner(Theme.light, "op", commands)
    val surface = new MockRenderSurface(100, 30)
    val layout  = LayoutEngine.calculateLayout(state, ViewportSize(100, 30))
    val overlay = layout.belowCursorOverlayRect.getOrElse(fail("Expected below-cursor overlay rect"))
    val paneRect = LayoutEngine
      .calculatePaneLayouts(state, layout)
      .getOrElse(paneId, fail("Expected pane layout"))
    val paneContentRect = CursorLayout.contentRectForPane(paneRect)
    val commandContentRect = SurfaceFrameLayout
      .forContent(overlay, state.runtime.uiSurfaces.head.content)
      .contentRect

    RendererEntryPoints.render(
      state,
      cursorVisible = true,
      surface,
      ViewportSize(100, 30),
      codeFont,
      Font(Font.SANS_SERIF, Font.PLAIN, 12),
      cellMetrics,
      None
    )

    val commandLine =
      (commandContentRect.x until commandContentRect.right)
        .map(x => surface.getChar(x, commandContentRect.y + 1))
        .mkString
        .trim

    commandLine should include("Open")
    commandLine should include("Open file")
    overlay.width shouldBe 72
    // Horizontally centered on screen, not cursor-anchored (bug fix): the palette's width and content bear no
    // relationship to the cursor's column.
    overlay.x shouldBe paneContentRect.x + (paneContentRect.width - overlay.width) / 2

    surface.getBg(0, 0) shouldBe state.persisted.theme.highlighted.background
    surface.getBg(overlay.x, overlay.y) shouldBe state.persisted.theme.panel.background
    surface.getBg(commandContentRect.x, commandContentRect.y + 1) shouldBe state.persisted.theme.highlighted.background

    val uiFont     = Font(Font.SANS_SERIF, Font.PLAIN, codeFont.getSize).deriveFont(codeFont.getSize2D)
    val searchText = "search: op"
    val searchRun  = surface.drawRunPxCalls.find(_.s == searchText).getOrElse(fail("Expected measured search text"))
    searchRun.xPx shouldBe cellMetrics.toPixelX(commandContentRect.x).toFloat
    searchRun.yPx shouldBe cellMetrics.toPixelY(commandContentRect.y)

    val searchCursorXPx = cellMetrics.toPixelX(commandContentRect.x) +
      math.round(TextLayoutSnapshot.caretXsForText(searchText, uiFont, surface.fontRenderContext.get).last)
    val searchCursorYPx = cellMetrics.toPixelY(commandContentRect.y)
    surface.fillPixelRectCalls.exists(call =>
      call.xPx == searchCursorXPx &&
        call.yPx == searchCursorYPx &&
        call.color == state.persisted.theme.cursor
    ) shouldBe true
  }

  it should "keep written document text and the command runner visible with Writing's text insets" in {
    val commands  = List(Command.typed("open", "Open file", CommandIntent.File(FileIntent.OpenFile)))
    val writing   = UiPreset.builtIn("Writing").getOrElse(fail("Expected Writing preset"))
    val baseState = stateWithRunner(Theme.light, "op", commands)
    val state     = baseState.copy(persisted = baseState.persisted.copy(config = writing.config))
    val surface   = new MockRenderSurface(100, 30)
    val layout    = LayoutEngine.calculateLayout(state, ViewportSize(100, 30))
    val paneLayout = LayoutEngine
      .calculateEditorPaneLayouts(state, layout)
      .getOrElse(paneId, fail("Expected pane layout"))
    val overlay = layout.belowCursorOverlayRect.getOrElse(fail("Expected command runner overlay"))

    RendererEntryPoints.render(state, cursorVisible = true, surface, ViewportSize(100, 30))

    overlay.x shouldBe paneLayout.contentRect.x
    overlay.width shouldBe paneLayout.contentRect.width
    surface.drawRunPxCalls.map(_.s) should contain("beta")
    surface.drawRunPxCalls.map(_.s) should contain("search: op")
  }

  it should "place the command runner below the editor cursor when there is room" in {
    val commands = List(Command.typed("open", "Open file", CommandIntent.File(FileIntent.OpenFile)))
    val registry = CommandRegistry(commands)
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default)
      .updateSearchTerm("open")(using registry)
    val content = (1 to 60).map(i => s"line $i").mkString("\n")

    def overlayFor(cursor: CursorPosition): (LayoutRect, LayoutRect) =
      val buffer = Buffer
        .fromString(bufferId, content)
        .copy(editing = EditingState(cursors = List(cursor)))
      val initialState = AppState.initial
      val state = initialState.copy(
        persisted = initialState.persisted.copy(
          buffers = Map(bufferId -> buffer),
          bufferOrder = List(bufferId),
          layout = singlePaneLayout,
          focus = Focus.Surface(SurfaceId("command-runner")),
          theme = Theme.light
        ),
        runtime = initialState.runtime.copy(
          uiSurfaces = List(
            UiSurface(
              SurfaceId("command-runner"),
              SurfaceContent.CommandPalette(runner),
              SurfacePresentation.Floating(Some(cursor), SurfacePlacement.BelowCursor)
            )
          )
        )
      )
      val layout = LayoutEngine.calculateLayout(state, ViewportSize(100, 40))
      val paneRect = LayoutEngine
        .calculatePaneLayouts(state, layout)
        .getOrElse(paneId, fail("Expected pane layout"))

      layout.belowCursorOverlayRect.getOrElse(fail("Expected command runner overlay")) ->
        CursorLayout.contentRectForPane(paneRect)

    val (topOverlay, topContentRect)     = overlayFor(CursorPosition(0, 0))
    val (lowerOverlay, lowerContentRect) = overlayFor(CursorPosition(20, 0))

    topOverlay.y shouldBe topContentRect.y + 2
    lowerOverlay.y shouldBe lowerContentRect.y + 22
    lowerOverlay.y should be > topOverlay.y
    // Horizontally centered on screen, not cursor-anchored (bug fix): both cursors sit at column 0, but the palette's
    // x position depends only on the content rect's width, not the cursor's row.
    lowerOverlay.x shouldBe lowerContentRect.x + (lowerContentRect.width - lowerOverlay.width) / 2
    lowerOverlay.x shouldBe topOverlay.x
  }

  // issue #931: category tabs are retired -- this used to also assert on a distributed tab row above the settings
  // rows (`tabLine`) and on background variation across it (tab-highlighting). Neither exists any more; browsing
  // settings groups with no search now only happens via `.openSettings`, whose header is a plain breadcrumb, not a
  // tab row.
  it should "show grouped settings rows in browse mode" in {
    val commands = List(
      Command.typed("open", "Open file", com.serenity.command.CommandIntent.File(FileIntent.OpenFile)),
      Command.typed(
        "toggle-theme",
        "Switch between light and dark theme",
        com.serenity.command.CommandIntent.Theme(ThemeIntent.ToggleTheme)
      )
    )
    val registry          = CommandRegistry(commands)
    given CommandRegistry = registry
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default.withShowAllSettingsRegardlessOfMode(true))
      .openSettings
    val buffer = Buffer
      .fromString(bufferId, "alpha\nbeta\ngamma")
      .copy(
        editing = EditingState(cursors = List(CursorPosition(1, 2)))
      )
    val initialState = AppState.initial
    val state = initialState.copy(
      persisted = initialState.persisted.copy(
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = singlePaneLayout,
        focus = Focus.Surface(SurfaceId("command-runner")),
        theme = Theme.light
      ),
      runtime = initialState.runtime.copy(
        uiSurfaces = List(
          UiSurface(
            SurfaceId("command-runner"),
            SurfaceContent.CommandPalette(runner),
            SurfacePresentation.Floating(Some(CursorPosition(1, 2)), SurfacePlacement.BelowCursor)
          )
        )
      )
    )
    val surface = new MockRenderSurface(100, 30)
    val layout  = LayoutEngine.calculateLayout(state, ViewportSize(100, 30))
    val overlay = layout.belowCursorOverlayRect.getOrElse(fail("Expected below-cursor overlay rect"))
    val commandContentRect = SurfaceFrameLayout
      .forContent(overlay, state.runtime.uiSurfaces.head.content)
      .contentRect

    RendererEntryPoints.render(state, cursorVisible = true, surface, ViewportSize(100, 30))

    val headerLine =
      (commandContentRect.x until commandContentRect.right)
        .map(x => surface.getChar(x, commandContentRect.y))
        .mkString
        .trim
    val optionLine =
      (commandContentRect.x until commandContentRect.right)
        .map(x => surface.getChar(x, commandContentRect.y + 1))
        .mkString
        .trim

    headerLine should include("Settings")
    optionLine should include("App Mode")
    optionLine should not include "["

    surface.fillPixelRectCalls.filter(_.color == state.persisted.theme.cursor) should have size 1
  }

  it should "retain nested settings breadcrumbs in a compact command palette row" in {
    val state   = stateWithRunner(Theme.light, "default document", Nil)
    val surface = new MockRenderSurface(55, 30)
    val layout  = LayoutEngine.calculateLayout(state, ViewportSize(55, 30))
    val overlay = layout.belowCursorOverlayRect.getOrElse(fail("Expected below-cursor overlay rect"))
    val contentRect = SurfaceFrameLayout
      .forContent(overlay, state.runtime.uiSurfaces.head.content)
      .contentRect

    RendererEntryPoints.render(state, cursorVisible = false, surface, ViewportSize(55, 30))

    val resultLine =
      (contentRect.x until contentRect.right)
        .map(x => surface.getChar(x, contentRect.y + 1))
        .mkString

    resultLine should include("Default Document")
    resultLine should include("Settings")
  }

  // issue #1059: a settings group drilled into from either entry point now renders on the one command-runner
  // surface -- previously this exercised focus sitting on a second floating submenu surface (dimming the root and
  // keeping editor cursors steady relative to it); with that second surface gone, focus and content both live on
  // the one surface, so there is nothing left to dim relative to, but the "editor cursors stay steady regardless of
  // command-runner navigation" coverage still applies here.
  // issue #931: category tabs are retired -- browsing settings groups with no search now only happens via the
  // dedicated Settings surface (`.openSettings`), not by switching the palette's category.
  it should "keep every editor cursor visible but steady while browsing a settings group" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default)
      .openSettings
      .enterSelectedGroup
    val buffer = Buffer
      .fromString(bufferId, "alpha\nbeta\ngamma")
      .copy(
        editing = EditingState(cursors = List(CursorPosition(1, 1), CursorPosition(1, 2), CursorPosition(1, 3)))
      )
    val initialState = AppState.initial
    val state = initialState.copy(
      persisted = initialState.persisted.copy(
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = singlePaneLayout,
        focus = Focus.Surface(SurfaceId("command-runner")),
        theme = Theme.light
      ),
      runtime = initialState.runtime.copy(
        uiSurfaces = List(
          UiSurface(
            SurfaceId("command-runner"),
            SurfaceContent.CommandPalette(runner),
            SurfacePresentation.Floating(Some(CursorPosition(1, 2)), SurfacePlacement.BelowCursor)
          )
        )
      )
    )
    val visibleSurface = new MockRenderSurface(100, 30)
    val hiddenSurface  = new MockRenderSurface(100, 30)

    RendererEntryPoints.render(state, cursorVisible = true, visibleSurface, ViewportSize(100, 30))
    RendererEntryPoints.render(state, cursorVisible = false, hiddenSurface, ViewportSize(100, 30))

    val visibleCursors = visibleSurface.fillPixelRectCalls.filter(_.color == state.persisted.theme.cursor)
    val hiddenCursors  = hiddenSurface.fillPixelRectCalls.filter(_.color == state.persisted.theme.cursor)

    visibleCursors should have size 3
    hiddenCursors should have size 3
    hiddenCursors.map(_.xPx) shouldBe visibleCursors.map(_.xPx)
  }

  // issue #1059: a settings leaf reached via search now renders its drilled-in navigation on the one command-runner
  // surface (no second floating submenu surface, and no live "search: ..." echo left showing once entered).
  // issue #1057: previously used a "lang-markdown" settings-tree search to reach the (now-removed) "Current Buffer
  // Language" group; retargeted to "UI Outline Thickness", a still-present settings leaf inside "Interface Layout".
  it should "render a direct settings leaf while keeping editor cursors steady" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default)
      .updateSearchTerm("UI Outline Thickness")
      .enterSelectedGroup
    val buffer = Buffer
      .fromString(bufferId, "alpha\nbeta\ngamma")
      .copy(
        editing = EditingState(cursors = List(CursorPosition(1, 1), CursorPosition(1, 2), CursorPosition(1, 3)))
      )
    val initialState = AppState.initial
    val state = initialState.copy(
      persisted = initialState.persisted.copy(
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = singlePaneLayout,
        focus = Focus.Surface(SurfaceId("command-runner")),
        theme = Theme.light
      ),
      runtime = initialState.runtime.copy(
        uiSurfaces = List(
          UiSurface(
            SurfaceId("command-runner"),
            SurfaceContent.CommandPalette(runner),
            SurfacePresentation.Floating(Some(CursorPosition(1, 2)), SurfacePlacement.BelowCursor)
          )
        )
      )
    )
    val visibleSurface = new MockRenderSurface(100, 30)
    val hiddenSurface  = new MockRenderSurface(100, 30)

    RendererEntryPoints.render(state, cursorVisible = true, visibleSurface, ViewportSize(100, 30))
    RendererEntryPoints.render(state, cursorVisible = false, hiddenSurface, ViewportSize(100, 30))

    val layout = LayoutEngine.calculateLayout(state, ViewportSize(100, 30))
    val submenuRect = layout.belowCursorOverlayStack
      .collectFirst { case (SurfaceId("command-runner"), rect) => rect }
      .getOrElse(fail("Expected command-runner overlay"))
    val submenuText = (submenuRect.y until submenuRect.bottom).map(visibleSurface.getRow).mkString("\n")

    val visibleCursors = visibleSurface.fillPixelRectCalls.filter(_.color == state.persisted.theme.cursor)
    val hiddenCursors  = hiddenSurface.fillPixelRectCalls.filter(_.color == state.persisted.theme.cursor)

    visibleCursors.map(_.xPx) shouldBe hiddenCursors.map(_.xPx)
    submenuText should include("Interface Layout")
    submenuText should not include "search: UI Outline Thickness"
    submenuText should include("UI Outline Thickness")
  }

end CommandRunnerPaletteContentRenderingSpec
