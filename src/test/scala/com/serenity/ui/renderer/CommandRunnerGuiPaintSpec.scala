package com.serenity.ui.renderer

import java.awt.Font
import java.awt.image.BufferedImage
import java.util.concurrent.atomic.AtomicReference

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.TestWorkspaceTrees
import com.serenity.command.{CommandRegistry, CommandRunner}
import com.serenity.config.AppConfig
import com.serenity.rope.Balance
import com.serenity.state.manager.RenderCaches
import com.serenity.state.models.*
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.layout.*
import com.serenity.ui.theme.{DefaultThemes, Theme}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

/** The GUI command runner, painted through a real [[Java2DRenderSurface]] at device scale 2, must show its text: the
  * region it covers holds more than one colour and some pixels near the panel's text colour, whichever theme is active.
  */
class CommandRunnerGuiPaintSpec extends AnyFlatSpec with Matchers:

  given Balance    = Balance.default
  given Logger[IO] = Slf4jLogger.getLogger[IO]

  private val paneId   = PaneId(0)
  private val bufferId = BufferId(1)
  private val runnerId = SurfaceId("command-runner")

  private val codeFont =
    FontLoader
      .loadCodeFont(FontLoader.FontConfig(codeFontFamily = FontLoader.BundledCodeFontFamily, enableLigatures = true))
      .unsafeRunSync()

  private val cellMetrics = CellMetrics.fromFont(codeFont)
  private val uiFont      = Font(Font.SANS_SERIF, Font.PLAIN, codeFont.getSize).deriveFont(codeFont.getSize2D)
  private val uiMetrics   = CellMetrics.fromFont(uiFont)

  private val viewport      = ViewportSize(90, 30)
  private val deviceScale   = 2.0
  private val logicalWidth  = viewport.width * cellMetrics.charWidth
  private val logicalHeight = viewport.height * cellMetrics.lineHeight

  private val registry = CommandRegistry.default

  private val runner = CommandRunner.empty
    .activate(registry, AppConfig.default)
    .updateSearchTerm("")(using registry)

  private def stateWith(theme: Theme, focus: Focus, extra: List[UiSurface] = Nil): AppState =
    val buffer = Buffer.fromString(bufferId, (1 to 20).map(i => s"line number $i with some content").mkString("\n"))
    val pane   = EditorPane.withBuffer(paneId, bufferId)
    val base   = AppState.initial
    base.copy(
      persisted = base.persisted.copy(
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = Layout(
          editorPanes = Map(paneId -> pane),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(TestWorkspaceTrees.linear(paneId))
        ),
        focus = focus,
        theme = theme
      ),
      runtime = base.runtime.copy(
        uiSurfaces = UiSurface(
          runnerId,
          SurfaceContent.CommandPalette(runner),
          SurfacePresentation.Floating(Some(CursorPosition(1, 2)), SurfacePlacement.BelowCursor)
        ) :: extra
      )
    )

  private def render(state: AppState, damage: Damage, caches: RenderCaches, into: Option[BufferedImage] = None) =
    val captured = new AtomicReference[BufferedImage](null)
    val image = into.getOrElse(
      new BufferedImage(
        math.ceil(logicalWidth * deviceScale).toInt,
        math.ceil(logicalHeight * deviceScale).toInt,
        BufferedImage.TYPE_INT_RGB
      )
    )
    val surface = new Java2DRenderSurface(
      image,
      cellMetrics,
      codeFont,
      frame => captured.set(frame),
      logicalWidthPx = logicalWidth,
      logicalHeightPx = logicalHeight,
      deviceScaleX = deviceScale,
      deviceScaleY = deviceScale,
      contentPersists = true
    )
    RendererEntryPoints.render(
      state,
      cursorVisible = false,
      surface,
      viewport,
      FontSpec.fromAwt(codeFont),
      FontSpec.fromAwt(codeFont),
      FontSpec.fromAwt(uiFont),
      cellMetrics,
      uiMetrics,
      None,
      damage,
      caches
    )
    Option(captured.get()).getOrElse(image)

  private def runnerPixels(state: AppState, image: BufferedImage): Vector[Int] =
    val rect = LayoutEngine
      .calculateLayout(state, viewport)
      .belowCursorOverlayRect
      .getOrElse(fail("expected a below-cursor overlay rect"))
    val left   = math.round(rect.x * cellMetrics.charWidth * deviceScale).toInt
    val top    = math.round(rect.y * cellMetrics.lineHeight * deviceScale).toInt
    val right  = math.round(rect.right * cellMetrics.charWidth * deviceScale).toInt.min(image.getWidth)
    val bottom = math.round(rect.bottom * cellMetrics.lineHeight * deviceScale).toInt.min(image.getHeight)
    (for
      y <- top until bottom
      x <- left until right
    yield image.getRGB(x, y) & 0xffffff).toVector

  private def closeTo(rgb: Int, color: com.serenity.ui.color.RenderColor): Boolean =
    math.abs(((rgb >> 16) & 0xff) - color.red) + math.abs(((rgb >> 8) & 0xff) - color.green) +
      math.abs((rgb & 0xff) - color.blue) < 60

  private def assertShowsText(
    theme: Theme,
    focus: Focus = Focus.Surface(runnerId),
    extra: List[UiSurface] = Nil,
    exactTextColour: Boolean = true
  ) =
    val state  = stateWith(theme, focus, extra)
    val caches = RenderCaches.create()
    val first  = runnerPixels(state, render(state, Damage.Everything, caches))
    val second = runnerPixels(state, render(state, Damage.Surface(runnerId), caches))
    List(first, second).foreach { pixels =>
      pixels.distinct.size should be > 2
      if exactTextColour then pixels.exists(closeTo(_, theme.panel.foreground)) shouldBe true
    }

  private val transparent: Theme => Theme = theme =>
    theme.copy(
      background = theme.background.withAlpha(0),
      panel = theme.panel.copy(background = theme.panel.background.withAlpha(0))
    )

  DefaultThemes.allInternal.foreach { (name, theme) =>
    s"The command runner under the $name theme" should "paint its text at scale 2" in assertShowsText(theme)

    it should "paint its text when the theme's backgrounds are fully transparent" in
      assertShowsText(transparent(theme))

    it should "paint its text while another surface holds focus" in {
      val other = UiSurface(
        SurfaceId("peek"),
        SurfaceContent.QuickInfo("x"),
        SurfacePresentation.Floating(Some(CursorPosition(0, 2)), SurfacePlacement.AboveCursor)
      )
      assertShowsText(theme, Focus.Surface(other.id), List(other), exactTextColour = false)
    }
  }

  private def translucentPanel(theme: Theme, alpha: Int): Theme =
    theme.copy(panel = theme.panel.copy(background = theme.panel.background.withAlpha(alpha)))

  private def runnerRender(damage: Damage, caches: RenderCaches, state: AppState) =
    runnerPixels(state, render(state, damage, caches))

  "A command runner whose theme panel background has alpha" should "let the editor behind it show through" in {
    val opaqueState = stateWith(Theme.dark, Focus.Surface(runnerId))
    val frostState  = stateWith(translucentPanel(Theme.dark, 110), Focus.Surface(runnerId))
    val solidPixels = runnerRender(Damage.Everything, RenderCaches.create(), opaqueState)
    val frosted     = runnerRender(Damage.Everything, RenderCaches.create(), frostState)

    frosted should not be solidPixels
    frosted.distinct.size should be > solidPixels.distinct.size
  }

  it should "still paint its text" in {
    val theme = translucentPanel(Theme.dark, 110)
    assertShowsText(theme)
  }

  it should "paint the same pixels when only the runner is repainted as on a full repaint" in {
    val theme  = translucentPanel(Theme.dark, 110)
    val state  = stateWith(theme, Focus.Surface(runnerId))
    val caches = RenderCaches.create()
    val image = new BufferedImage(
      math.ceil(logicalWidth * deviceScale).toInt,
      math.ceil(logicalHeight * deviceScale).toInt,
      BufferedImage.TYPE_INT_RGB
    )
    val full = runnerPixels(state, render(state, Damage.Everything, caches, Some(image)))
    val again =
      (1 to 3).map(_ => runnerPixels(state, render(state, Damage.Surface(runnerId), caches, Some(image)))).toList

    again.zipWithIndex.foreach { (pixels, repeat) =>
      val differing = pixels.indices.filter(i => pixels(i) != full(i))
      withClue(
        s"repaint ${repeat + 1}: ${differing.size} of ${full.size} pixels differ; first at ${differing.headOption} " +
          s"full=${differing.headOption.map(i => f"${full(i)}%06x")} repaint=${differing.headOption.map(i => f"${pixels(i)}%06x")}: "
      )(differing shouldBe empty)
    }
  }
