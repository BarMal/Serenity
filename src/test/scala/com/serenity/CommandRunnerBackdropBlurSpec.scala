package com.serenity

import java.awt.Font

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.command.*
import com.serenity.config.AppConfigMotionOps.*
import com.serenity.config.{AppConfig, BackgroundStyle}
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.layout.*
import com.serenity.ui.renderer.RendererEntryPoints
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

class CommandRunnerBackdropBlurSpec extends AnyFlatSpec with Matchers:

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

  "RendererEntryPoints.render" should "request backdrop blur for the floating overlay using the configured blur radius" in {
    val commands       = List(Command.typed("open", "Open file", CommandIntent.File(FileIntent.OpenFile)))
    val preConfigState = stateWithRunner(Theme.light, "op", commands)
    val state = preConfigState.copy(
      persisted = preConfigState.persisted.copy(config = AppConfig.default.withBlurRadius(0.6f))
    )
    val surface = new MockRenderSurface(100, 30)
    val layout  = LayoutEngine.calculateLayout(state, ViewportSize(100, 30))
    val overlay = layout.belowCursorOverlayRect.getOrElse(fail("Expected below-cursor overlay rect"))

    RendererEntryPoints.render(state, cursorVisible = true, surface, ViewportSize(100, 30))

    surface.blurRegionCalls should contain(
      surface.BlurRegionCall(overlay.x, overlay.y, overlay.width, overlay.height, 0.6f)
    )
  }

  it should "translate backdrop blur with a fractional floating offset" in {
    val commands       = List(Command.typed("open", "Open file", CommandIntent.File(FileIntent.OpenFile)))
    val preConfigState = stateWithRunner(Theme.light, "op", commands)
    val state = preConfigState.copy(
      persisted = preConfigState.persisted.copy(
        config = AppConfig.default
          .withBlurRadius(0.6f)
          .withCommandRunnerCursorGapRows(Some(0.5))
      )
    )
    val surface = new MockRenderSurface(100, 30)
    val layout  = LayoutEngine.calculateLayout(state, ViewportSize(100, 30))
    val offsetPx = FloatingSurfaceGeometry.signedRowOffsetPixels(
      layout.floatingOverlayOffsetRows.getOrElse(SurfaceId("command-runner"), 0.0),
      cellMetrics
    )

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

    surface.blurRegionTranslations should contain(surface.PixelTranslationCall(0.0, offsetPx))
  }

  it should "skip backdrop blur for a solid overlay background style" in {
    val commands       = List(Command.typed("open", "Open file", CommandIntent.File(FileIntent.OpenFile)))
    val preConfigState = stateWithRunner(Theme.light, "op", commands)
    val state = preConfigState.copy(
      persisted = preConfigState.persisted.copy(
        config = AppConfig.default
          .withBlurRadius(0.6f)
          .withBackgroundStyle(BackgroundStyle.Solid)
      )
    )
    val surface = new MockRenderSurface(100, 30)

    RendererEntryPoints.render(state, cursorVisible = true, surface, ViewportSize(100, 30))

    surface.blurRegionCalls shouldBe empty
  }

  it should "use a stronger blur radius for the glass-like overlay style" in {
    val commands       = List(Command.typed("open", "Open file", CommandIntent.File(FileIntent.OpenFile)))
    val preConfigState = stateWithRunner(Theme.light, "op", commands)
    val state = preConfigState.copy(
      persisted = preConfigState.persisted.copy(
        config = AppConfig.default
          .withBlurRadius(0.2f)
          .withBackgroundStyle(BackgroundStyle.GlassLike)
      )
    )
    val surface = new MockRenderSurface(100, 30)
    val layout  = LayoutEngine.calculateLayout(state, ViewportSize(100, 30))
    val overlay = layout.belowCursorOverlayRect.getOrElse(fail("Expected below-cursor overlay rect"))

    RendererEntryPoints.render(state, cursorVisible = true, surface, ViewportSize(100, 30))

    surface.blurRegionCalls should contain(
      surface.BlurRegionCall(overlay.x, overlay.y, overlay.width, overlay.height, 0.42f)
    )
  }

end CommandRunnerBackdropBlurSpec
