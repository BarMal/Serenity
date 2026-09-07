package com.serenity

import java.awt.image.BufferedImage
import java.awt.Font
import java.util.concurrent.atomic.AtomicReference

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.command.*
import com.serenity.config.AppConfig
import com.serenity.rope.Balance
import com.serenity.state.manager.DamageProducer
import com.serenity.state.models.*
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.layout.*
import com.serenity.ui.renderer.{Java2DRenderSurface, RendererEntryPoints}
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

/** Regression cover for the command-runner render-accumulation bug: navigating the floating command runner over an
  * editor pane must not disturb the editor pane's pixels. Only the runner surface's own damage is reported per
  * keystroke, so the editor pane is preserved -- but the floating-panel layer path seeds a full-frame layer buffer from
  * the live surface and composites it back, and if that round-trip is not pixel-exact the editor pane drifts a little
  * more with every navigation, "snapping back" only on a full clean repaint.
  *
  * This drives a real, pooled [[Java2DRenderSurface]] (the two-image pool `SwingWindow` uses in production, which no
  * other renderer spec exercises: `MockRenderSurface` reports `layerBuffers = None`, so the seeded-layer path never
  * runs there) across several navigation frames and asserts the editor pane region is byte-identical across them.
  */
class CommandRunnerEditorPaneAccumulationSpec extends AnyFlatSpec with Matchers:

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
    .copy(editing = EditingState(cursors = List(CursorPosition(1, 2))))

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
          activeEditorPaneId = Some(paneId)
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

  /** The two-image frame pool `SwingWindow.ReusableImagePool` uses: `acquire` hands back the spare (the frame from two
    * frames ago) so painted pixels alternate between two backing buffers, exactly as in production.
    */
  private final class Pool:
    private val published = new AtomicReference[Option[BufferedImage]](None)
    private val spare     = new AtomicReference[Option[BufferedImage]](None)

    def acquire(width: Int, height: Int, imageType: Int): BufferedImage =
      spare
        .getAndSet(None)
        .filter(i => i.getWidth == width && i.getHeight == height && i.getType == imageType)
        .getOrElse(new BufferedImage(width, height, imageType))

    def publish(image: BufferedImage): Unit =
      val previous = published.getAndSet(Some(image))
      previous.filterNot(_ eq image).foreach(p => spare.set(Some(p)))

  private def renderFrame(pool: Pool, state: AppState, damage: Damage): BufferedImage =
    val capturedRef = new AtomicReference[BufferedImage](null)
    val deviceWidth  = math.ceil(logicalWidth * deviceScaleX).toInt
    val deviceHeight = math.ceil(logicalHeight * deviceScaleY).toInt
    val image        = pool.acquire(deviceWidth, deviceHeight, BufferedImage.TYPE_INT_ARGB)
    val surface = new Java2DRenderSurface(
      image,
      cellMetrics,
      codeFont,
      captured => capturedRef.set(captured),
      logicalWidthPx = logicalWidth,
      logicalHeightPx = logicalHeight,
      deviceScaleX = deviceScaleX,
      deviceScaleY = deviceScaleY,
      contentPersists = true
    )
    RendererEntryPoints.render(
      state,
      cursorVisible = false,
      surface,
      viewport,
      codeFont,
      codeFont,
      uiFont,
      cellMetrics,
      uiMetrics,
      None,
      damage
    )
    val out = capturedRef.get()
    pool.publish(out)
    out

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

  private def editorPaneRegionPixels(image: BufferedImage): Vector[Int] =
    val bottomPx = math.round((runnerRect.y - 1) * cellMetrics.lineHeight * deviceScaleY).toInt.max(1).min(image.getHeight)
    (for
      y <- 0 until bottomPx
      x <- 0 until image.getWidth
    yield image.getRGB(x, y)).toVector

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

end CommandRunnerEditorPaneAccumulationSpec
