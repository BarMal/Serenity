package com.serenity.perf

import java.awt.Font
import java.awt.image.BufferedImage
import java.nio.file.Files
import javax.swing.SwingUtilities

import cats.effect.unsafe.IORuntime
import cats.effect.{IO, Resource}
import com.serenity.config.{AppConfig, PreferredWindowSize}
import com.serenity.keystroke.events.{DeleteBackward, Event, InsertChar, MoveDown, MoveUp}
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManagerTestFacade.{createBuffer, updateState}
import com.serenity.state.manager.{RenderCaches, StateManager}
import com.serenity.state.models.*
import com.serenity.ui.layout.{CellMetrics, PanelPosition, ViewportSize}
import com.serenity.ui.renderer.{Java2DRenderSurface, RendererEntryPoints}
import com.serenity.ui.terminal.SwingWindow
import com.serenity.{DockedPanelFixtures, setBufferForPane, setCursorPosition}
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.noop.{NoOpFactory, NoOpLogger}

/** The editor at the size issue #1798's laptop runs it: a 1500x1000 logical window at 2x device scale, showing
  * word-wrapped lorem ipsum prose under the app's own default config (Frosted material, so panel blur is on).
  *
  * The other render benchmarks use a 120x40-cell viewport and a no-wrap config, which kept a full frame near a
  * millisecond while a real session at this size spent tens of milliseconds per keystroke. These measure the same
  * stages `ui.render.frame_timing` reports in the running app -- applying input through the `StateManager`, drawing a
  * frame, and Swing painting it -- so CI numbers and laptop logs can be compared stage by stage.
  */
private[perf] object LaptopFrameBenchmarks:

  given Balance = Balance.default

  val LogicalWidthPx  = 1500
  val LogicalHeightPx = 1000
  val DeviceScale     = 2.0

  private val codeFont    = Font(Font.MONOSPACED, Font.PLAIN, 12)
  private val textFont    = Font(Font.SERIF, Font.PLAIN, 14)
  private val cellMetrics = CellMetrics.fromFont(codeFont)
  private val viewport =
    ViewportSize(LogicalWidthPx / cellMetrics.charWidth, LogicalHeightPx / cellMetrics.lineHeight)
  private val bufferId = BufferId(1)

  def proseState: AppState =
    val base   = BenchmarkFixtures.editorState(BenchmarkFixtures.loremIpsumProse(paragraphs = 300), None)
    val buffer = base.persisted.buffers(bufferId)
    val sized = buffer.copy(
      viewport = buffer.viewport.copy(visibleColumns = viewport.width, visibleLines = viewport.height),
      editing = EditingState(List(CursorPosition(150, 40)))
    )
    base.copy(
      persisted = base.persisted.copy(
        config = AppConfig.default,
        buffers = Map(bufferId -> sized),
        focus = Focus.EditorPane(PaneId(0))
      ),
      runtime = base.runtime.copy(viewportSize = Some(viewport))
    )

  def proseWithPinnedPanelState: AppState =
    DockedPanelFixtures.dock(
      proseState,
      SurfaceId("outline"),
      SurfaceContent.Outline(Nil),
      PanelPosition.Right,
      40
    )

  /** A frame buffer at the window's device size; reused across iterations, as the app's own image pool does. */
  def frameImage(): BufferedImage =
    new BufferedImage(
      math.ceil(LogicalWidthPx * DeviceScale).toInt,
      math.ceil(LogicalHeightPx * DeviceScale).toInt,
      BufferedImage.TYPE_INT_ARGB
    )

  def renderedFrame(state: AppState, caches: RenderCaches, image: BufferedImage): BufferedImage =
    val surface = new Java2DRenderSurface(
      image,
      cellMetrics,
      codeFont,
      _ => (),
      logicalWidthPx = LogicalWidthPx,
      logicalHeightPx = LogicalHeightPx,
      deviceScaleX = DeviceScale,
      deviceScaleY = DeviceScale
    )
    RendererEntryPoints.render(
      state,
      cursorVisible = true,
      surface,
      viewport,
      codeFont,
      textFont,
      cellMetrics,
      None,
      caches
    )
    image

  private def hasPixels(image: BufferedImage): Boolean =
    ((image.getRGB(image.getWidth / 2, image.getHeight / 2) >>> 24) & 0xff) > 0

  /** The laptop-sized window [[presentBenchmark]] paints into. */
  def presentWindowResource(metrics: CellMetrics, chromeMetrics: CellMetrics): Resource[IO, SwingWindow] =
    SwingWindow.resource(
      metrics = metrics,
      chromeMetrics = chromeMetrics,
      preferredWindowSize = Some(PreferredWindowSize(LogicalWidthPx, LogicalHeightPx))
    )

  def benchmarks(presentWindow: SwingWindow)(using IORuntime): List[BenchmarkRunner.Benchmark] =
    renderBenchmarks ++ inputBenchmarks ++ List(presentBenchmark(presentWindow))

  private def renderBenchmarks: List[BenchmarkRunner.Benchmark] =
    val caches     = RenderCaches.create()
    val prose      = proseState
    val withPanel  = proseWithPinnedPanelState
    val proseImage = frameImage()
    val panelImage = frameImage()
    List(
      BenchmarkRunner.Benchmark(
        "laptop.render.prose_frame_2x",
        2,
        8,
        () => assert(hasPixels(renderedFrame(prose, caches, proseImage)), "prose frame drew nothing"),
        () => renderedFrame(prose, caches, proseImage)
      ),
      BenchmarkRunner.Benchmark(
        "laptop.render.prose_frame_2x.pinned_panel_frosted",
        2,
        8,
        () => assert(hasPixels(renderedFrame(withPanel, caches, panelImage)), "panel frame drew nothing"),
        () => renderedFrame(withPanel, caches, panelImage)
      )
    )

  /** A live `StateManager` holding the prose, set up the way the StateManager specs do it: a hand-built `AppState`
    * swapped in wholesale can fail validation, and then every applied event is rejected and nothing is measured.
    */
  private def proseStateManager(using IORuntime): (StateManager, BufferId) =
    given LoggerFactory[IO] = NoOpFactory[IO]
    val sessionRoot         = Files.createTempDirectory("serenity-laptop-benchmarks")
    val stateManager = StateManager.apply(NoOpLogger[IO], sessionRootOverride = Some(sessionRoot)).unsafeRunSync()
    val proseBufferId = (for
      _ <- stateManager.updateState(state => state.copy(persisted = state.persisted.copy(config = AppConfig.default)))
      created <- stateManager.createBuffer(BenchmarkFixtures.loremIpsumProse(paragraphs = 300), None)
      state   <- stateManager.getCurrentState
      paneId = state.persisted.layout.editorPanes.keys.head
      _ <- stateManager.setBufferForPane(paneId, created)
      _ <- stateManager.setCursorPosition(paneId, 150, 40)
    yield created).unsafeRunSync()
    (stateManager, proseBufferId)

  private def inputBenchmarks(using IORuntime): List[BenchmarkRunner.Benchmark] =
    val (stateManager, proseBufferId) = proseStateManager
    def buffer: Option[Buffer] = stateManager.getCurrentState.unsafeRunSync().persisted.buffers.get(proseBufferId)
    def cursor: Option[CursorPosition] = buffer.flatMap(_.editing.cursorPositions.headOption)
    def length: Option[Int]            = buffer.map(_.document.content.weight)
    def roundTrip(there: Event, back: Event): Unit =
      (stateManager.applyEvent(there) >> stateManager.applyEvent(back)).unsafeRunSync()
    List(
      BenchmarkRunner.Benchmark(
        "laptop.input.state_manager.move_down_up",
        3,
        20,
        () =>
          val before = cursor
          stateManager.applyEvent(MoveDown).unsafeRunSync()
          val moved = cursor
          stateManager.applyEvent(MoveUp).unsafeRunSync()
          assert(
            moved != before && cursor == before,
            s"cursor did not move down and back: $before -> $moved -> $cursor"
          )
        ,
        () => roundTrip(MoveDown, MoveUp)
      ),
      BenchmarkRunner.Benchmark(
        "laptop.input.state_manager.type_and_delete",
        3,
        20,
        () =>
          val before = length
          roundTrip(InsertChar('x'), DeleteBackward)
          assert(length == before, s"type-and-delete changed the document length: $before -> $length")
        ,
        () => roundTrip(InsertChar('x'), DeleteBackward)
      )
    )

  /** `presentWindow` should come from [[presentWindowResource]]; a canvas-sized frame is painted 1:1. */
  private def presentBenchmark(presentWindow: SwingWindow): BenchmarkRunner.Benchmark =
    val canvas = presentWindow.canvas
    val frame  = new BufferedImage(canvas.getWidth.max(1), canvas.getHeight.max(1), BufferedImage.TYPE_INT_ARGB)
    BenchmarkRunner.Benchmark(
      "laptop.present.swing_paint_window_1500x1000",
      2,
      8,
      () =>
        assert(canvas.isShowing && canvas.getWidth > 0, s"canvas not showing: ${canvas.getWidth}x${canvas.getHeight}"),
      () =>
        SwingUtilities.invokeAndWait { () =>
          presentWindow.onBaseImageReady(frame)
          canvas.paintImmediately(0, 0, canvas.getWidth, canvas.getHeight)
        }
    )
