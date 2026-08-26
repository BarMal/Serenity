package com.serenity

import java.awt.image.BufferedImage
import java.awt.{Color, Font}

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.animation.{AnimatedCell, AnimationState, CharacterKey}
import com.serenity.command.*
import com.serenity.config.{AppConfig, BackgroundStyle}
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.layout.*
import com.serenity.ui.presets.UiPreset
import com.serenity.ui.renderer.{Java2DRenderSurface, Renderer, SurfaceMaterials}
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
        editing = EditingState(cursors = cursors)
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
    val paneContentRect = CursorLayout.contentRectForPane(paneRect)
    val commandContentRect = SurfaceFrameLayout
      .forContent(overlay, state.uiSurfaces.head.content)
      .contentRect

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
      (commandContentRect.x until commandContentRect.right)
        .map(x => surface.getChar(x, commandContentRect.y + 1))
        .mkString
        .trim

    commandLine should include("Open")
    commandLine should include("Open file")
    overlay.width shouldBe 72
    overlay.x shouldBe paneContentRect.x

    surface.getBg(0, 0) shouldBe state.theme.highlighted.background
    surface.getBg(overlay.x, overlay.y) shouldBe state.theme.panel.background
    surface.getBg(commandContentRect.x, commandContentRect.y + 1) shouldBe state.theme.highlighted.background

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
        call.color == state.theme.cursor
    ) shouldBe true
  }

  it should "keep written document text and the command runner visible with Writing's text insets" in {
    val commands = List(Command.typed("open", "Open file", CommandIntent.OpenFile))
    val writing  = UiPreset.builtIn("Writing").getOrElse(fail("Expected Writing preset"))
    val state    = stateWithRunner(Theme.light, "op", commands).copy(config = writing.config)
    val surface  = new MockRenderSurface(100, 30)
    val layout   = LayoutEngine.calculateLayout(state, ViewportSize(100, 30))
    val paneLayout = LayoutEngine
      .calculateEditorPaneLayouts(state, layout)
      .getOrElse(paneId, fail("Expected pane layout"))
    val overlay = layout.belowCursorOverlayRect.getOrElse(fail("Expected command runner overlay"))

    Renderer.render(state, cursorVisible = true, surface, ViewportSize(100, 30))

    overlay.x shouldBe paneLayout.contentRect.x
    overlay.width shouldBe paneLayout.contentRect.width
    surface.drawRunPxCalls.map(_.s) should contain("beta")
    surface.drawRunPxCalls.map(_.s) should contain("search: op")
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
        .copy(editing = EditingState(cursors = List(cursor)))
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
        editing = EditingState(cursors = List(CursorPosition(1, 2)))
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
    val commandContentRect = SurfaceFrameLayout
      .forContent(overlay, state.uiSurfaces.head.content)
      .contentRect

    Renderer.render(state, cursorVisible = true, surface, ViewportSize(100, 30))

    val tabLine =
      (commandContentRect.x until commandContentRect.right)
        .map(x => surface.getChar(x, commandContentRect.y))
        .mkString
        .trim
    val optionLine =
      (commandContentRect.x until commandContentRect.right)
        .map(x => surface.getChar(x, commandContentRect.y + 1))
        .mkString
        .trim

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
      (commandContentRect.x until commandContentRect.right)
        .map(x => surface.getBg(x, commandContentRect.y))
        .distinct
    settingsBackgrounds.size should be > 1
    surface.fillPixelRectCalls.filter(_.color == state.theme.cursor) should have size 1
  }

  it should "retain nested settings breadcrumbs in a compact command palette row" in {
    val state   = stateWithRunner(Theme.light, "default document", Nil)
    val surface = new MockRenderSurface(55, 30)
    val layout  = LayoutEngine.calculateLayout(state, ViewportSize(55, 30))
    val overlay = layout.belowCursorOverlayRect.getOrElse(fail("Expected below-cursor overlay rect"))
    val contentRect = SurfaceFrameLayout
      .forContent(overlay, state.uiSurfaces.head.content)
      .contentRect

    Renderer.render(state, cursorVisible = false, surface, ViewportSize(55, 30))

    val resultLine =
      (contentRect.x until contentRect.right)
        .map(x => surface.getChar(x, contentRect.y + 1))
        .mkString

    resultLine should include("Default Document")
    resultLine should include("Settings")
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
        editing = EditingState(cursors = List(CursorPosition(1, 1), CursorPosition(1, 2), CursorPosition(1, 3)))
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
    val buffer = Buffer
      .fromString(bufferId, "alpha\nbeta\ngamma")
      .copy(editing = EditingState(cursors = List(CursorPosition(1, 2))))
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
    val surface = new MockRenderSurface(100, 30)

    Renderer.render(state, cursorVisible = true, surface, ViewportSize(100, 30))

    val baseAlpha = SurfaceMaterials.panelAlpha(state.config, state.theme)
    surface.alphaCalls should contain(baseAlpha)
    surface.alphaCalls.filter(_ < baseAlpha) should not be empty
  }

  it should "render a direct settings leaf while keeping editor cursors steady" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default)
      .updateSearchTerm("lang-markdown")
      .enterSelectedGroup
    val buffer = Buffer
      .fromString(bufferId, "alpha\nbeta\ngamma")
      .copy(
        editing = EditingState(cursors = List(CursorPosition(1, 1), CursorPosition(1, 2), CursorPosition(1, 3)))
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
    val submenuText = (submenuRect.y until submenuRect.bottom).map(visibleSurface.getRow).mkString("\n")

    val visibleCursors = visibleSurface.fillPixelRectCalls.filter(_.color == state.theme.cursor)
    val hiddenCursors  = hiddenSurface.fillPixelRectCalls.filter(_.color == state.theme.cursor)

    visibleCursors should have size 4
    hiddenCursors should have size 3
    hiddenCursors.map(_.xPx) shouldBe visibleCursors.take(3).map(_.xPx)
    submenuText should include("Current Buffer Language")
    submenuText should not include "search: lang-markdown"
    submenuText should include("Markdown")
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
        CharacterKey(0, 2) -> AnimatedCell.fromThemeTransition(
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
    val commandContentRect = SurfaceFrameLayout
      .forContent(overlay, state.uiSurfaces.head.content)
      .contentRect

    Renderer.render(state, cursorVisible = true, surface, ViewportSize(100, 30))

    val selectedBackground = surface.getBg(commandContentRect.x, commandContentRect.y + 1)
    val selectedForeground = surface.getFg(commandContentRect.x, commandContentRect.y + 1)
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

  it should "draw a shadow behind the command runner only when UI shadows are enabled" in {
    val commands     = List(Command.typed("open", "Open file", CommandIntent.OpenFile))
    val enabledState = stateWithRunner(Theme.light, "op", commands)
    val disabledState = enabledState.copy(
      config = AppConfig.default.withUiShadowsEnabled(false)
    )
    val enabledSurface  = new MockRenderSurface(100, 30)
    val disabledSurface = new MockRenderSurface(100, 30)

    Renderer.render(enabledState, cursorVisible = true, enabledSurface, ViewportSize(100, 30))
    Renderer.render(disabledState, cursorVisible = true, disabledSurface, ViewportSize(100, 30))

    enabledSurface.roundRectShadowCalls should not be empty
    disabledSurface.roundRectShadowCalls shouldBe empty
  }

  it should "preserve the rounded command runner after its animation has materialised" in {
    val commands = List(Command.typed("open", "Open file", CommandIntent.OpenFile))
    val state = stateWithRunner(Theme.light, "op", commands).copy(
      config = AppConfig.default.withUiCornerRadiusPx(12)
    )
    val surface = new MockRenderSurface(100, 30)

    Renderer.render(state, cursorVisible = true, surface, ViewportSize(100, 30))

    surface.strokeRoundRectCalls.headOption.map(_.arcPx) shouldBe Some(12)
    surface.putStringCalls.map(_.s) should not contain "."
  }

  it should "leave the rounded command runner's fully materialised corner unpainted" in {
    val commands = List(Command.typed("open", "Open file", CommandIntent.OpenFile))
    val state = stateWithRunner(Theme.light, "op", commands).copy(
      config = AppConfig.default
        .withBackgroundStyle(BackgroundStyle.Solid)
        .withUiCornerRadiusPx(12)
    )
    val viewport = ViewportSize(100, 30)
    val layout   = LayoutEngine.calculateLayout(state, viewport)
    val overlay  = layout.belowCursorOverlayRect.getOrElse(fail("Expected below-cursor overlay rect"))
    val widthPx  = viewport.width * cellMetrics.charWidth
    val heightPx = viewport.height * cellMetrics.lineHeight

    def renderedImage(renderState: AppState): BufferedImage =
      val image   = new BufferedImage(widthPx, heightPx, BufferedImage.TYPE_INT_ARGB)
      val surface = new Java2DRenderSurface(image, cellMetrics, codeFont, _ => ())
      Renderer.render(
        renderState,
        cursorVisible = true,
        surface,
        viewport,
        codeFont,
        Font(Font.SANS_SERIF, Font.PLAIN, 12),
        cellMetrics,
        None
      )
      image

    val withoutRunner = renderedImage(state.copy(uiSurfaces = Nil, focus = Focus.EditorPane(paneId)))
    val withRunner    = renderedImage(state)
    val cornerX       = cellMetrics.toPixelX(overlay.x)
    val cornerY       = cellMetrics.toPixelY(overlay.y)

    new Color(withRunner.getRGB(cornerX, cornerY), true) shouldBe new Color(
      withoutRunner.getRGB(cornerX, cornerY),
      true
    )
  }

  it should "leave the rounded command runner's frosted corner unblurred" in {
    val commands = List(Command.typed("open", "Open file", CommandIntent.OpenFile))
    val state = stateWithRunner(Theme.light, "op", commands).copy(
      config = AppConfig.default
        .withBackgroundStyle(BackgroundStyle.GlassLike)
        .withBlurRadius(0.6f)
        .withUiCornerRadiusPx(12)
    )
    val viewport = ViewportSize(100, 30)
    val layout   = LayoutEngine.calculateLayout(state, viewport)
    val overlay  = layout.belowCursorOverlayRect.getOrElse(fail("Expected below-cursor overlay rect"))
    val widthPx  = viewport.width * cellMetrics.charWidth
    val heightPx = viewport.height * cellMetrics.lineHeight

    def renderedImage(renderState: AppState): BufferedImage =
      val image   = new BufferedImage(widthPx, heightPx, BufferedImage.TYPE_INT_ARGB)
      val surface = new Java2DRenderSurface(image, cellMetrics, codeFont, _ => ())
      Renderer.render(
        renderState,
        cursorVisible = true,
        surface,
        viewport,
        codeFont,
        Font(Font.SANS_SERIF, Font.PLAIN, 12),
        cellMetrics,
        None
      )
      image

    val withoutRunner = renderedImage(state.copy(uiSurfaces = Nil, focus = Focus.EditorPane(paneId)))
    val withRunner    = renderedImage(state)
    val cornerX       = cellMetrics.toPixelX(overlay.x)
    val cornerY       = cellMetrics.toPixelY(overlay.y)

    new Color(withRunner.getRGB(cornerX, cornerY), true) shouldBe new Color(
      withoutRunner.getRGB(cornerX, cornerY),
      true
    )
  }

  it should "preserve rounded context menus after their animation has materialised" in {
    val copyCommand = Command.typed("copy", "Copy", CommandIntent.Copy, label = "Copy")
    val menu = ContextMenu(
      title = "editor",
      targetFocus = Focus.EditorPane(paneId),
      items = List(ContextMenuItem("copy", "Copy", copyCommand))
    )
    val state = stateWithRunner(Theme.light, "", Nil).copy(
      config = AppConfig.default.withUiCornerRadiusPx(12),
      focus = Focus.Surface(SurfaceId("context-menu")),
      uiSurfaces = List(
        UiSurface(
          SurfaceId("context-menu"),
          SurfaceContent.ContextMenu(menu),
          SurfacePresentation.Floating(Some(CursorPosition(1, 2)), SurfacePlacement.BelowCursor)
        )
      )
    )
    val surface = new MockRenderSurface(100, 30)

    Renderer.render(state, cursorVisible = true, surface, ViewportSize(100, 30))

    surface.strokeRoundRectCalls.headOption.map(_.arcPx) shouldBe Some(12)
    surface.putStringCalls.map(_.s) should not contain "."
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

  it should "translate backdrop blur with a fractional floating offset" in {
    val commands = List(Command.typed("open", "Open file", CommandIntent.OpenFile))
    val state = stateWithRunner(Theme.light, "op", commands).copy(
      config = AppConfig.default
        .withBlurRadius(0.6f)
        .withCommandRunnerCursorGapRows(Some(0.5))
    )
    val surface = new MockRenderSurface(100, 30)
    val layout  = LayoutEngine.calculateLayout(state, ViewportSize(100, 30))
    val offsetPx = FloatingSurfaceGeometry.signedRowOffsetPixels(
      layout.floatingOverlayOffsetRows.getOrElse(SurfaceId("command-runner"), 0.0),
      cellMetrics
    )

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

    surface.blurRegionTranslations should contain(surface.PixelTranslationCall(0.0, offsetPx))
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
      surface.BlurRegionCall(overlay.x, overlay.y, overlay.width, overlay.height, 0.42f)
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
        editing = EditingState(cursors = List(CursorPosition(1, 2)))
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
