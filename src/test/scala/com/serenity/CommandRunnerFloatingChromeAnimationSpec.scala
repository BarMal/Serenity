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
import com.serenity.ui.renderer.{Java2DRenderSurface, RendererEntryPoints}
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

class CommandRunnerFloatingChromeAnimationSpec extends AnyFlatSpec with Matchers:

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
  "RendererEntryPoints.render" should "fade the selected command highlight with the overlay row animation" in {
    val commands = List(
      Command.typed("open", "Open file", CommandIntent.File(FileIntent.OpenFile)),
      Command.typed("close", "Close current file", CommandIntent.File(FileIntent.CloseCurrentFile))
    )
    val baseState = stateWithRunner(Theme.light, "op", commands)
    val surfaceId = SurfaceId("command-runner")
    val transparentPanelForeground = new java.awt.Color(
      baseState.persisted.theme.panel.foreground.getRed,
      baseState.persisted.theme.panel.foreground.getGreen,
      baseState.persisted.theme.panel.foreground.getBlue,
      0
    )
    val transparentPanelBackground = new java.awt.Color(
      baseState.persisted.theme.panel.background.getRed,
      baseState.persisted.theme.panel.background.getGreen,
      baseState.persisted.theme.panel.background.getBlue,
      0
    )
    val animationState = AnimationState.empty.mergeAnimations(
      Map(
        CharacterKey(0, 2) -> AnimatedCell.fromThemeTransition(
          transparentPanelForeground,
          baseState.persisted.theme.panel.foreground,
          transparentPanelBackground,
          baseState.persisted.theme.panel.background,
          steps = 2
        )
      )
    )
    val state = baseState.copy(
      runtime = baseState.runtime.copy(surfaceAnimations =
        Map(surfaceId -> SurfaceAnimationState(animationState = animationState))
      )
    )

    val surface = new MockRenderSurface(100, 30)
    val layout  = LayoutEngine.calculateLayout(state, ViewportSize(100, 30))
    val overlay = layout.belowCursorOverlayRect.getOrElse(fail("Expected below-cursor overlay rect"))
    val commandContentRect = SurfaceFrameLayout
      .forContent(overlay, state.runtime.uiSurfaces.head.content)
      .contentRect

    RendererEntryPoints.render(state, cursorVisible = true, surface, ViewportSize(100, 30))

    val selectedBackground = surface.getBg(commandContentRect.x, commandContentRect.y + 1)
    val selectedForeground = surface.getFg(commandContentRect.x, commandContentRect.y + 1)
    selectedBackground.getRGB & 0x00ffffff shouldBe state.persisted.theme.highlighted.background.getRGB & 0x00ffffff
    selectedForeground.getRGB & 0x00ffffff shouldBe state.persisted.theme.highlighted.foreground.getRGB & 0x00ffffff
    selectedBackground.getAlpha shouldBe 0
    selectedForeground.getAlpha shouldBe 0
  }

  it should "draw the floating border with the rounded stroke even while animating" in {
    val commands       = List(Command.typed("open", "Open file", CommandIntent.File(FileIntent.OpenFile)))
    val preConfigState = stateWithRunner(Theme.light, "op", commands)
    val baseState = preConfigState.copy(
      persisted = preConfigState.persisted.copy(config = AppConfig.default.withUiCornerRadiusPx(12))
    )
    val surfaceId = SurfaceId("command-runner")
    val animationState = AnimationState.empty.mergeAnimations(
      Map(
        CharacterKey(0, 0) -> AnimatedCell.fromThemeTransition(
          baseState.persisted.theme.panel.foreground,
          baseState.persisted.theme.panel.foreground,
          baseState.persisted.theme.panel.background,
          baseState.persisted.theme.panel.background,
          steps = 2
        )
      )
    )
    val state = baseState.copy(
      runtime = baseState.runtime.copy(surfaceAnimations =
        Map(surfaceId -> SurfaceAnimationState(animationState = animationState))
      )
    )

    val surface = new MockRenderSurface(100, 30)
    val layout  = LayoutEngine.calculateLayout(state, ViewportSize(100, 30))
    layout.belowCursorOverlayRect.getOrElse(fail("Expected below-cursor overlay rect"))

    RendererEntryPoints.render(state, cursorVisible = true, surface, ViewportSize(100, 30))

    surface.strokeRoundRectCalls should not be empty
    surface.strokeRoundRectCalls.headOption.map(_.arcPx) shouldBe Some(12)
  }

  it should "draw the floating border with the configured outline thickness" in {
    val commands       = List(Command.typed("open", "Open file", CommandIntent.File(FileIntent.OpenFile)))
    val preConfigState = stateWithRunner(Theme.light, "op", commands)
    val state = preConfigState.copy(
      persisted = preConfigState.persisted.copy(config = AppConfig.default.withUiOutlineThicknessPx(4))
    )

    val surface = new MockRenderSurface(100, 30)

    RendererEntryPoints.render(state, cursorVisible = true, surface, ViewportSize(100, 30))

    surface.strokeRoundRectCalls should not be empty
    surface.strokeRoundRectCalls.headOption.map(_.strokeWidth) shouldBe Some(4.0f)
  }

  it should "draw a shadow behind the command runner only when UI shadows are enabled" in {
    val commands     = List(Command.typed("open", "Open file", CommandIntent.File(FileIntent.OpenFile)))
    val enabledState = stateWithRunner(Theme.light, "op", commands)
    val disabledState = enabledState.copy(
      persisted = enabledState.persisted.copy(config = AppConfig.default.withUiShadowsEnabled(false))
    )
    val enabledSurface  = new MockRenderSurface(100, 30)
    val disabledSurface = new MockRenderSurface(100, 30)

    RendererEntryPoints.render(enabledState, cursorVisible = true, enabledSurface, ViewportSize(100, 30))
    RendererEntryPoints.render(disabledState, cursorVisible = true, disabledSurface, ViewportSize(100, 30))

    enabledSurface.roundRectShadowCalls should not be empty
    disabledSurface.roundRectShadowCalls shouldBe empty
  }

  it should "preserve the rounded command runner after its animation has materialised" in {
    val commands       = List(Command.typed("open", "Open file", CommandIntent.File(FileIntent.OpenFile)))
    val preConfigState = stateWithRunner(Theme.light, "op", commands)
    val state = preConfigState.copy(
      persisted = preConfigState.persisted.copy(config = AppConfig.default.withUiCornerRadiusPx(12))
    )
    val surface = new MockRenderSurface(100, 30)

    RendererEntryPoints.render(state, cursorVisible = true, surface, ViewportSize(100, 30))

    surface.strokeRoundRectCalls.headOption.map(_.arcPx) shouldBe Some(12)
    surface.putStringCalls.map(_.s) should not contain "."
  }

  it should "leave the rounded command runner's fully materialised corner unpainted" in {
    val commands       = List(Command.typed("open", "Open file", CommandIntent.File(FileIntent.OpenFile)))
    val preConfigState = stateWithRunner(Theme.light, "op", commands)
    val state = preConfigState.copy(
      persisted = preConfigState.persisted.copy(
        config = AppConfig.default
          .withBackgroundStyle(BackgroundStyle.Solid)
          .withUiCornerRadiusPx(12)
      )
    )
    val viewport = ViewportSize(100, 30)
    val layout   = LayoutEngine.calculateLayout(state, viewport)
    val overlay  = layout.belowCursorOverlayRect.getOrElse(fail("Expected below-cursor overlay rect"))
    val widthPx  = viewport.width * cellMetrics.charWidth
    val heightPx = viewport.height * cellMetrics.lineHeight

    def renderedImage(renderState: AppState): BufferedImage =
      val image   = new BufferedImage(widthPx, heightPx, BufferedImage.TYPE_INT_ARGB)
      val surface = new Java2DRenderSurface(image, cellMetrics, codeFont, _ => ())
      RendererEntryPoints.render(
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

    val withoutRunner = renderedImage(
      state.copy(
        persisted = state.persisted.copy(focus = Focus.EditorPane(paneId)),
        runtime = state.runtime.copy(uiSurfaces = Nil)
      )
    )
    val withRunner = renderedImage(state)
    val cornerX    = cellMetrics.toPixelX(overlay.x)
    val cornerY    = cellMetrics.toPixelY(overlay.y)

    new Color(withRunner.getRGB(cornerX, cornerY), true) shouldBe new Color(
      withoutRunner.getRGB(cornerX, cornerY),
      true
    )
  }

  it should "leave the rounded command runner's frosted corner unblurred" in {
    val commands       = List(Command.typed("open", "Open file", CommandIntent.File(FileIntent.OpenFile)))
    val preConfigState = stateWithRunner(Theme.light, "op", commands)
    val state = preConfigState.copy(
      persisted = preConfigState.persisted.copy(
        config = AppConfig.default
          .withBackgroundStyle(BackgroundStyle.GlassLike)
          .withBlurRadius(0.6f)
          .withUiCornerRadiusPx(12)
      )
    )
    val viewport = ViewportSize(100, 30)
    val layout   = LayoutEngine.calculateLayout(state, viewport)
    val overlay  = layout.belowCursorOverlayRect.getOrElse(fail("Expected below-cursor overlay rect"))
    val widthPx  = viewport.width * cellMetrics.charWidth
    val heightPx = viewport.height * cellMetrics.lineHeight

    def renderedImage(renderState: AppState): BufferedImage =
      val image   = new BufferedImage(widthPx, heightPx, BufferedImage.TYPE_INT_ARGB)
      val surface = new Java2DRenderSurface(image, cellMetrics, codeFont, _ => ())
      RendererEntryPoints.render(
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

    val withoutRunner = renderedImage(
      state.copy(
        persisted = state.persisted.copy(focus = Focus.EditorPane(paneId)),
        runtime = state.runtime.copy(uiSurfaces = Nil)
      )
    )
    val withRunner = renderedImage(state)
    val cornerX    = cellMetrics.toPixelX(overlay.x)
    val cornerY    = cellMetrics.toPixelY(overlay.y)

    new Color(withRunner.getRGB(cornerX, cornerY), true) shouldBe new Color(
      withoutRunner.getRGB(cornerX, cornerY),
      true
    )
  }

  it should "preserve rounded context menus after their animation has materialised" in {
    val copyCommand = Command.typed("copy", "Copy", CommandIntent.Edit(EditIntent.Copy), label = "Copy")
    val menu = ContextMenu(
      title = "editor",
      targetFocus = Focus.EditorPane(paneId),
      items = List(ContextMenuItem("copy", "Copy", copyCommand))
    )
    val preMenuState = stateWithRunner(Theme.light, "", Nil)
    val state = preMenuState.copy(
      persisted = preMenuState.persisted.copy(
        config = AppConfig.default.withUiCornerRadiusPx(12),
        focus = Focus.Surface(SurfaceId("context-menu"))
      ),
      runtime = preMenuState.runtime.copy(
        uiSurfaces = List(
          UiSurface(
            SurfaceId("context-menu"),
            SurfaceContent.ContextMenu(menu),
            SurfacePresentation.Floating(Some(CursorPosition(1, 2)), SurfacePlacement.BelowCursor)
          )
        )
      )
    )
    val surface = new MockRenderSurface(100, 30)

    RendererEntryPoints.render(state, cursorVisible = true, surface, ViewportSize(100, 30))

    surface.strokeRoundRectCalls.headOption.map(_.arcPx) shouldBe Some(12)
    surface.putStringCalls.map(_.s) should not contain "."
  }

end CommandRunnerFloatingChromeAnimationSpec
