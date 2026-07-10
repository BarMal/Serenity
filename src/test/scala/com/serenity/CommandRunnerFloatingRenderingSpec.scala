package com.serenity

import java.awt.Font

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.animation.{AnimatedCell, AnimationState, CharacterKey}
import com.serenity.command.*
import com.serenity.config.{AppConfig, BackgroundStyle}
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.layout.*
import com.serenity.ui.renderer.{Renderer, SurfaceMaterials}
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

class CommandRunnerFloatingRenderingSpec extends AnyFlatSpec with Matchers:

  given Balance    = Balance.default
  given Logger[IO] = Slf4jLogger.getLogger[IO]

  private val paneId   = PaneId(0)
  private val bufferId = BufferId(1)

  private val codeFont = FontLoader
    .loadCodeFont(FontLoader.FontConfig(codeFontFamily = FontLoader.BundledCodeFontFamily, enableLigatures = true))
    .unsafeRunSync()

  private val cellMetrics = CellMetrics.fromFont(codeFont)

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
        cursors = cursors
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
      Command.typed("open", "Open file", CommandIntent.OpenFile),
      Command.typed("close", "Close current file", CommandIntent.CloseCurrentFile)
    )
    val state   = stateWithRunner(Theme.light, "op", commands)
    val surface = new MockRenderSurface(100, 30)
    val layout  = LayoutEngine.calculateLayout(state, ViewportSize(100, 30))
    val overlay = layout.belowCursorOverlayRect.getOrElse(fail("Expected below-cursor overlay rect"))
    val paneRect = LayoutEngine
      .calculatePaneLayouts(state, layout)
      .getOrElse(paneId, fail("Expected pane layout"))
    val contentRect = CursorLayout.contentRectForPane(paneRect)

    Renderer.render(
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
      (overlay.x until overlay.right).map(x => surface.getChar(x, overlay.y + 1)).mkString.trim

    commandLine should include("Open")
    commandLine should include("Open file")
    overlay.width shouldBe contentRect.width
    overlay.x shouldBe contentRect.x

    surface.getBg(0, 0) shouldBe state.theme.highlighted.background
    surface.getBg(overlay.x, overlay.y) shouldBe state.theme.panel.background
    surface.getBg(overlay.x, overlay.y + 1) shouldBe state.theme.highlighted.background

    val uiFont     = Font(Font.SANS_SERIF, Font.PLAIN, codeFont.getSize).deriveFont(codeFont.getSize2D)
    val searchText = "search: op"
    val searchRun  = surface.drawRunPxCalls.find(_.s == searchText).getOrElse(fail("Expected measured search text"))
    searchRun.xPx shouldBe cellMetrics.toPixelX(overlay.x).toFloat
    searchRun.yPx shouldBe cellMetrics.toPixelY(overlay.y)

    val searchCursorXPx = cellMetrics.toPixelX(overlay.x) +
      math.round(TextLayoutSnapshot.caretXsForText(searchText, uiFont, surface.fontRenderContext.get).last)
    val searchCursorYPx = cellMetrics.toPixelY(overlay.y)
    surface.fillPixelRectCalls.exists(call =>
      call.xPx == searchCursorXPx &&
        call.yPx == searchCursorYPx &&
        call.color == state.theme.cursor
    ) shouldBe true
  }

  it should "place the command runner below the editor cursor when there is room" in {
    val commands = List(Command.typed("open", "Open file", CommandIntent.OpenFile))
    val registry = CommandRegistry(commands)
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default)
      .updateSearchTerm("open")(using registry)
    val content = (1 to 60).map(i => s"line $i").mkString("\n")

    def overlayFor(cursor: CursorPosition): (LayoutRect, LayoutRect) =
      val buffer = Buffer
        .fromString(bufferId, content)
        .copy(cursors = List(cursor))
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
            SurfacePresentation.Floating(Some(cursor), SurfacePlacement.BelowCursor)
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
    lowerOverlay.x shouldBe lowerContentRect.x
  }

  it should "render category tabs in browse mode and show grouped settings rows" in {
    val commands = List(
      Command.typed("open", "Open file", com.serenity.command.CommandIntent.OpenFile),
      Command.typed(
        "toggle-theme",
        "Switch between light and dark theme",
        com.serenity.command.CommandIntent.ToggleTheme
      )
    )
    val registry          = CommandRegistry(commands)
    given CommandRegistry = registry
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default)
      .withActiveCategory(CommandCategory.Settings)
    val buffer = Buffer
      .fromString(bufferId, "alpha\nbeta\ngamma")
      .copy(
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
      (overlay.x until overlay.right).map(x => surface.getChar(x, overlay.y)).mkString.trim
    val optionLine =
      (overlay.x until overlay.right).map(x => surface.getChar(x, overlay.y + 1)).mkString.trim

    tabLine should include("All")
    tabLine should include("File")
    tabLine should include("View")
    tabLine should include("Edit")
    tabLine should include("Settings")
    tabLine should not include "["
    tabLine.indexOf("Settings") should be > tabLine.length / 2
    optionLine should include("Panels & Workspace")
    optionLine should not include "["

    val settingsBackgrounds =
      (overlay.x until overlay.right)
        .map(x => surface.getBg(x, overlay.y))
        .distinct
    settingsBackgrounds.size should be > 1
    surface.fillPixelRectCalls.filter(_.color == state.theme.cursor) should have size 1
  }

  it should "keep every editor cursor visible but steady while browsing a focused submenu" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default)
      .withActiveCategory(CommandCategory.Settings)
      .enterSelectedGroup
    val buffer = Buffer
      .fromString(bufferId, "alpha\nbeta\ngamma")
      .copy(
        cursors = List(CursorPosition(1, 1), CursorPosition(1, 2), CursorPosition(1, 3))
      )
    val pane = EditorPane.withBuffer(paneId, bufferId)
    val state = AppState.initial.copy(
      buffers = Map(bufferId -> buffer),
      bufferOrder = List(bufferId),
      layout = Layout(
        editorPanes = Map(paneId -> pane),
        activeEditorPaneId = Some(paneId)
      ),
      focus = Focus.Surface(SurfaceId("command-runner-submenu")),
      theme = Theme.light,
      uiSurfaces = List(
        UiSurface(
          SurfaceId("command-runner"),
          SurfaceContent.CommandPalette(runner),
          SurfacePresentation.Floating(Some(CursorPosition(1, 2)), SurfacePlacement.BelowCursor)
        ),
        UiSurface(
          SurfaceId("command-runner-submenu"),
          SurfaceContent.CommandPaletteSubmenu(runner, "settings-animation", previewOnly = false),
          SurfacePresentation.Floating(Some(CursorPosition(1, 2)), SurfacePlacement.BelowCursor)
        )
      )
    )
    val visibleSurface = new MockRenderSurface(100, 30)
    val hiddenSurface  = new MockRenderSurface(100, 30)

    Renderer.render(state, cursorVisible = true, visibleSurface, ViewportSize(100, 30))
    Renderer.render(state, cursorVisible = false, hiddenSurface, ViewportSize(100, 30))

    val visibleCursors = visibleSurface.fillPixelRectCalls.filter(_.color == state.theme.cursor)
    val hiddenCursors  = hiddenSurface.fillPixelRectCalls.filter(_.color == state.theme.cursor)

    visibleCursors should have size 3
    hiddenCursors should have size 3
    hiddenCursors.map(_.xPx) shouldBe visibleCursors.map(_.xPx)
  }

  it should "dim the inactive root command runner while a submenu has focus" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default)
      .withActiveCategory(CommandCategory.Settings)
      .enterSelectedGroup
    val buffer = Buffer.fromString(bufferId, "alpha\nbeta\ngamma").copy(cursors = List(CursorPosition(1, 2)))
    val pane   = EditorPane.withBuffer(paneId, bufferId)
    val state = AppState.initial.copy(
      buffers = Map(bufferId -> buffer),
      bufferOrder = List(bufferId),
      layout = Layout(
        editorPanes = Map(paneId -> pane),
        activeEditorPaneId = Some(paneId)
      ),
      focus = Focus.Surface(SurfaceId("command-runner-submenu")),
      theme = Theme.light,
      uiSurfaces = List(
        UiSurface(
          SurfaceId("command-runner"),
          SurfaceContent.CommandPalette(runner),
          SurfacePresentation.Floating(Some(CursorPosition(1, 2)), SurfacePlacement.BelowCursor)
        ),
        UiSurface(
          SurfaceId("command-runner-submenu"),
          SurfaceContent.CommandPaletteSubmenu(runner, "settings-animation", previewOnly = false),
          SurfacePresentation.Floating(Some(CursorPosition(1, 2)), SurfacePlacement.BelowCursor)
        )
      )
    )
    val surface = new MockRenderSurface(100, 30)

    Renderer.render(state, cursorVisible = true, surface, ViewportSize(100, 30))

    val baseAlpha = SurfaceMaterials.panelAlpha(state.config, state.theme)
    surface.alphaCalls should contain(baseAlpha)
    surface.alphaCalls.filter(_ < baseAlpha) should not be empty
  }

  it should "render carried submenu search while keeping editor cursors steady" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default)
      .updateSearchTerm("lang-markdown")
      .enterSelectedGroup
    val buffer = Buffer
      .fromString(bufferId, "alpha\nbeta\ngamma")
      .copy(
        cursors = List(CursorPosition(1, 1), CursorPosition(1, 2), CursorPosition(1, 3))
      )
    val pane = EditorPane.withBuffer(paneId, bufferId)
    val state = AppState.initial.copy(
      buffers = Map(bufferId -> buffer),
      bufferOrder = List(bufferId),
      layout = Layout(
        editorPanes = Map(paneId -> pane),
        activeEditorPaneId = Some(paneId)
      ),
      focus = Focus.Surface(SurfaceId("command-runner-submenu")),
      theme = Theme.light,
      uiSurfaces = List(
        UiSurface(
          SurfaceId("command-runner"),
          SurfaceContent.CommandPalette(runner),
          SurfacePresentation.Floating(Some(CursorPosition(1, 2)), SurfacePlacement.BelowCursor)
        ),
        UiSurface(
          SurfaceId("command-runner-submenu"),
          SurfaceContent.CommandPaletteSubmenu(runner, "settings-language", previewOnly = false),
          SurfacePresentation.Floating(Some(CursorPosition(1, 2)), SurfacePlacement.BelowCursor)
        )
      )
    )
    val visibleSurface = new MockRenderSurface(100, 30)
    val hiddenSurface  = new MockRenderSurface(100, 30)

    Renderer.render(state, cursorVisible = true, visibleSurface, ViewportSize(100, 30))
    Renderer.render(state, cursorVisible = false, hiddenSurface, ViewportSize(100, 30))

    val layout = LayoutEngine.calculateLayout(state, ViewportSize(100, 30))
    val submenuRect = layout.belowCursorOverlayStack
      .collectFirst { case (SurfaceId("command-runner-submenu"), rect) => rect }
      .getOrElse(fail("Expected command-runner submenu overlay"))
    val codeFont         = Font(Font.MONOSPACED, Font.PLAIN, 12)
    val defaultMetrics   = CellMetrics.fromFont(codeFont)
    val searchText       = "Current Buffer Language search: lang-markdown"
    val submenuText      = (submenuRect.y until submenuRect.bottom).map(visibleSurface.getRow).mkString("\n")
    val submenuCursorYPx = defaultMetrics.toPixelY(submenuRect.y)

    val visibleCursors = visibleSurface.fillPixelRectCalls.filter(_.color == state.theme.cursor)
    val hiddenCursors  = hiddenSurface.fillPixelRectCalls.filter(_.color == state.theme.cursor)

    visibleCursors should have size 5
    hiddenCursors should have size 3
    hiddenCursors.map(_.xPx) shouldBe visibleCursors.take(3).map(_.xPx)
    submenuText should include(searchText)
    submenuText should include("Markdown")
    visibleCursors.exists(_.yPx == submenuCursorYPx) shouldBe true
    hiddenCursors.exists(_.yPx == submenuCursorYPx) shouldBe false
  }

  it should "fade the selected command highlight with the overlay row animation" in {
    val commands = List(
      Command.typed("open", "Open file", CommandIntent.OpenFile),
      Command.typed("close", "Close current file", CommandIntent.CloseCurrentFile)
    )
    val baseState = stateWithRunner(Theme.light, "op", commands)
    val surfaceId = SurfaceId("command-runner")
    val transparentPanelForeground = new java.awt.Color(
      baseState.theme.panel.foreground.getRed,
      baseState.theme.panel.foreground.getGreen,
      baseState.theme.panel.foreground.getBlue,
      0
    )
    val transparentPanelBackground = new java.awt.Color(
      baseState.theme.panel.background.getRed,
      baseState.theme.panel.background.getGreen,
      baseState.theme.panel.background.getBlue,
      0
    )
    val animationState = AnimationState.empty.mergeAnimations(
      Map(
        CharacterKey(0, 1) -> AnimatedCell.fromThemeTransition(
          transparentPanelForeground,
          baseState.theme.panel.foreground,
          transparentPanelBackground,
          baseState.theme.panel.background,
          steps = 2
        )
      )
    )
    val state = baseState.copy(
      surfaceAnimations = Map(surfaceId -> SurfaceAnimationState(animationState = animationState))
    )

    val surface = new MockRenderSurface(100, 30)
    val layout  = LayoutEngine.calculateLayout(state, ViewportSize(100, 30))
    val overlay = layout.belowCursorOverlayRect.getOrElse(fail("Expected below-cursor overlay rect"))

    Renderer.render(state, cursorVisible = true, surface, ViewportSize(100, 30))

    val selectedBackground = surface.getBg(overlay.x, overlay.y + 1)
    val selectedForeground = surface.getFg(overlay.x, overlay.y + 1)
    selectedBackground.getRGB & 0x00ffffff shouldBe state.theme.highlighted.background.getRGB & 0x00ffffff
    selectedForeground.getRGB & 0x00ffffff shouldBe state.theme.highlighted.foreground.getRGB & 0x00ffffff
    selectedBackground.getAlpha shouldBe 0
    selectedForeground.getAlpha shouldBe 0
  }

  it should "draw the floating border with the rounded stroke even while animating" in {
    val commands = List(Command.typed("open", "Open file", CommandIntent.OpenFile))
    val baseState = stateWithRunner(Theme.light, "op", commands).copy(
      config = AppConfig.default.withUiCornerRadiusPx(12)
    )
    val surfaceId = SurfaceId("command-runner")
    val animationState = AnimationState.empty.mergeAnimations(
      Map(
        CharacterKey(0, 0) -> AnimatedCell.fromThemeTransition(
          baseState.theme.panel.foreground,
          baseState.theme.panel.foreground,
          baseState.theme.panel.background,
          baseState.theme.panel.background,
          steps = 2
        )
      )
    )
    val state = baseState.copy(
      surfaceAnimations = Map(surfaceId -> SurfaceAnimationState(animationState = animationState))
    )

    val surface = new MockRenderSurface(100, 30)
    val layout  = LayoutEngine.calculateLayout(state, ViewportSize(100, 30))
    layout.belowCursorOverlayRect.getOrElse(fail("Expected below-cursor overlay rect"))

    Renderer.render(state, cursorVisible = true, surface, ViewportSize(100, 30))

    surface.strokeRoundRectCalls should not be empty
    surface.strokeRoundRectCalls.headOption.map(_.arcPx) shouldBe Some(12)
  }

  it should "draw the floating border with the configured outline thickness" in {
    val commands = List(Command.typed("open", "Open file", CommandIntent.OpenFile))
    val state = stateWithRunner(Theme.light, "op", commands).copy(
      config = AppConfig.default.withUiOutlineThicknessPx(4)
    )

    val surface = new MockRenderSurface(100, 30)

    Renderer.render(state, cursorVisible = true, surface, ViewportSize(100, 30))

    surface.strokeRoundRectCalls should not be empty
    surface.strokeRoundRectCalls.headOption.map(_.strokeWidth) shouldBe Some(4.0f)
  }

  it should "request backdrop blur for the floating overlay using the configured blur radius" in {
    val commands = List(Command.typed("open", "Open file", CommandIntent.OpenFile))
    val state = stateWithRunner(Theme.light, "op", commands).copy(
      config = AppConfig.default.withBlurRadius(0.6f)
    )
    val surface = new MockRenderSurface(100, 30)
    val layout  = LayoutEngine.calculateLayout(state, ViewportSize(100, 30))
    val overlay = layout.belowCursorOverlayRect.getOrElse(fail("Expected below-cursor overlay rect"))

    Renderer.render(state, cursorVisible = true, surface, ViewportSize(100, 30))

    surface.blurRegionCalls should contain(
      surface.BlurRegionCall(overlay.x, overlay.y, overlay.width, overlay.height, 0.6f)
    )
  }

  it should "skip backdrop blur for a solid overlay background style" in {
    val commands = List(Command.typed("open", "Open file", CommandIntent.OpenFile))
    val state = stateWithRunner(Theme.light, "op", commands).copy(
      config = AppConfig.default
        .withBlurRadius(0.6f)
        .withBackgroundStyle(BackgroundStyle.Solid)
    )
    val surface = new MockRenderSurface(100, 30)

    Renderer.render(state, cursorVisible = true, surface, ViewportSize(100, 30))

    surface.blurRegionCalls shouldBe empty
  }

  it should "use a stronger blur radius for the glass-like overlay style" in {
    val commands = List(Command.typed("open", "Open file", CommandIntent.OpenFile))
    val state = stateWithRunner(Theme.light, "op", commands).copy(
      config = AppConfig.default
        .withBlurRadius(0.2f)
        .withBackgroundStyle(BackgroundStyle.GlassLike)
    )
    val surface = new MockRenderSurface(100, 30)
    val layout  = LayoutEngine.calculateLayout(state, ViewportSize(100, 30))
    val overlay = layout.belowCursorOverlayRect.getOrElse(fail("Expected below-cursor overlay rect"))

    Renderer.render(state, cursorVisible = true, surface, ViewportSize(100, 30))

    surface.blurRegionCalls should contain(
      surface.BlurRegionCall(overlay.x, overlay.y, overlay.width, overlay.height, 0.6f)
    )
  }

  it should "render a ghost submenu preview beneath the main command runner with reduced alpha" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default)
      .withActiveCategory(CommandCategory.Settings)
    val buffer = Buffer
      .fromString(bufferId, "alpha\nbeta\ngamma")
      .copy(
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
        ),
        UiSurface(
          SurfaceId("command-runner-submenu"),
          SurfaceContent.CommandPaletteSubmenu(runner, "settings-animation", previewOnly = true),
          SurfacePresentation.Floating(Some(CursorPosition(1, 2)), SurfacePlacement.BelowCursor)
        )
      )
    )
    val surface = new MockRenderSurface(100, 30)

    Renderer.render(state, cursorVisible = true, surface, ViewportSize(100, 30))

    surface.strokeRoundRectCalls should have size 2
    surface.alphaCalls.exists(_ < SurfaceMaterials.panelAlpha(state.config, state.theme)) shouldBe true
  }
end CommandRunnerFloatingRenderingSpec
