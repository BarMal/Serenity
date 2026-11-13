package com.serenity

import java.awt.Font
import java.util.concurrent.atomic.AtomicInteger

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.command.*
import com.serenity.config.AppConfig
import com.serenity.perf.FrameImages
import com.serenity.rope.Balance
import com.serenity.state.manager.DamageProducer
import com.serenity.state.models.*
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.layout.*
import com.serenity.ui.renderer.{FontSpec, RendererEntryPoints}
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

/** A kept-pixels guard: navigating the floating command runner over an editor pane must not disturb the editor pane's
  * pixels. Only the runner surface's own damage is reported per keystroke, so the editor pane is preserved, and a pane
  * that drifted a little more with every navigation would "snap back" only on a full clean repaint.
  *
  * [[CommandRunnerEditorPaneAccumulationSpec]] was written for the Java2D seeded-layer round trip, which a Skia surface
  * does not have (it paints panels straight onto the frame). The invariant is still one to hold: this drives a Skia
  * surface persisting into the two-image pool a window uses, at a fractional device scale and a size that is not a
  * whole number of cells, across several navigation frames, and asserts the editor pane region is byte-identical.
  */
class SkiaCommandRunnerEditorPaneAccumulationSpec extends AnyFlatSpec with Matchers:

  given Balance    = Balance.default
  given Logger[IO] = Slf4jLogger.getLogger[IO]

  private val paneId   = PaneId(0)
  private val bufferId = BufferId(1)

  private val codeFont = FontLoader
    .loadCodeFont(FontLoader.FontConfig(codeFontFamily = FontLoader.BundledCodeFontFamily, enableLigatures = true))
    .unsafeRunSync()

  private val cellMetrics = CellMetrics.fromFont(codeFont)
  private val uiFont      = Font(Font.SANS_SERIF, Font.PLAIN, codeFont.getSize).deriveFont(codeFont.getSize2D)
  private val uiMetrics   = CellMetrics.fromFont(uiFont)

  // A deliberately non-grid-aligned logical size (not a whole multiple of the cell size) plus a fractional device
  // scale -- exactly the conditions a real window presents, and the ones under which a non-pixel-exact seed/redraw
  // round-trip compounds a shrink into the editor pane frame after frame.
  private val viewport      = ViewportSize(90, 30)
  private val deviceScaleX  = 1.25
  private val deviceScaleY  = 1.25
  private val logicalWidth  = viewport.width * cellMetrics.charWidth + 7
  private val logicalHeight = viewport.height * cellMetrics.lineHeight + 5

  private val baseBuffer = Buffer
    .fromString(bufferId, (1 to 20).map(i => s"line number $i with some content").mkString("\n"))
    .copy(editing = EditingState(List(CursorPosition(1, 2))))

  // A single base state shared across navigation frames -- only the command-runner surface's content changes between
  // frames, so DamageProducer scopes the transition to that one surface, exactly as a real Up/Down keystroke does.
  private val baseState: AppState =
    val pane         = EditorPane.withBuffer(paneId, bufferId)
    val initialState = AppState.initial
    initialState.copy(
      persisted = initialState.persisted.copy(
        buffers = Map(bufferId -> baseBuffer),
        bufferOrder = List(bufferId),
        layout = Layout(
          editorPanes = Map(paneId -> pane),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(TestWorkspaceTrees.linear(paneId))
        ),
        focus = Focus.Surface(SurfaceId("command-runner")),
        theme = Theme.light
      )
    )

  private def stateWithRunner(runner: CommandRunner): AppState =
    baseState.copy(
      runtime = baseState.runtime.copy(
        uiSurfaces = List(
          UiSurface(
            SurfaceId("command-runner"),
            SurfaceContent.CommandPalette(runner),
            SurfacePresentation.Floating(Some(CursorPosition(1, 2)), SurfacePlacement.BelowCursor)
          )
        )
      )
    )

  private val deviceWidth  = math.ceil(logicalWidth * deviceScaleX).toInt
  private val deviceHeight = math.ceil(logicalHeight * deviceScaleY).toInt

  /** The two images a window alternates between, so painted pixels alternate between two backing buffers. */
  final private class Pool:
    private val images = FrameImages(deviceWidth, deviceHeight, 2).drawn
    private val next   = AtomicInteger(0)

    def acquire(): FramePixels = images(next.getAndIncrement() % images.length)

  private def renderFrame(pool: Pool, state: AppState, damage: Damage): FramePixels =
    val image  = pool.acquire()
    val caches = com.serenity.state.manager.RenderCaches.create()
    val surface = SkiaFrames.surfaceOver(
      image,
      cellMetrics,
      codeFont,
      logicalWidth,
      logicalHeight,
      deviceScaleX,
      persists = true,
      caches
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
    image

  private val registry = CommandRegistry.default

  private val runner0 = CommandRunner.empty
    .activate(registry, AppConfig.default)
    .updateSearchTerm("")(using registry)

  // The command runner floats below the cursor (row 1), so its rect starts a couple of rows down. The band this test
  // guards is the editor rows strictly above that rect -- pure editor content that must not move while the runner
  // navigates.
  private val runnerRect: LayoutRect =
    LayoutEngine
      .calculateLayout(stateWithRunner(runner0), viewport)
      .belowCursorOverlayRect
      .getOrElse(throw new AssertionError("expected a below-cursor overlay rect"))

  private def editorPaneRegionPixels(image: FramePixels): Vector[Int] =
    val bottomPx =
      math.round((runnerRect.y - 1) * cellMetrics.lineHeight * deviceScaleY).toInt.max(1).min(image.heightPx)
    val pixels = image.rgbPixels
    (for
      y <- 0 until bottomPx
      x <- 0 until image.widthPx
    yield pixels(y * image.widthPx + x)).toVector

  "Navigating the command runner" should "not disturb the editor pane pixels behind it" in {
    val pool   = new Pool()
    val state0 = stateWithRunner(runner0)

    // Two full paints so both of the pool's alternating backing buffers hold a correct editor pane before navigation.
    val _ = renderFrame(pool, state0, Damage.Everything)
    val _ = renderFrame(pool, state0, Damage.Everything)

    // The settled editor pane: this is what every subsequent navigation frame must reproduce byte-for-byte.
    val baseline = editorPaneRegionPixels(renderFrame(pool, state0, Damage.Everything))

    // Navigate the runner repeatedly. Each step changes only the command-runner surface, so DamageProducer scopes the
    // transition to that surface and the editor pane behind it is preserved -- it must not drift or shrink.
    (1 to 6).foldLeft((state0, runner0)) {
      case ((current, runner), _) =>
        val nextRunner = runner.moveSelection(1)
        val nextState  = stateWithRunner(nextRunner)
        val damage     = DamageProducer.forTransition(current, nextState)
        damage shouldBe Damage.Surface(SurfaceId("command-runner"))
        editorPaneRegionPixels(renderFrame(pool, nextState, damage)) shouldBe baseline
        (nextState, nextRunner)
    }
  }

end SkiaCommandRunnerEditorPaneAccumulationSpec
