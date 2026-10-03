package com.serenity.perf

import java.awt.image.BufferedImage
import java.awt.{Color, Font, Rectangle}
import java.nio.file.Files
import java.util.concurrent.atomic.AtomicReference
import javax.swing.SwingUtilities

import cats.effect.unsafe.IORuntime
import cats.effect.{IO, Resource}
import com.serenity.config.{AppConfig, PreferredWindowSize}
import com.serenity.keystroke.events.{
  DeleteBackward,
  Event,
  InsertChar,
  ModalInsertChar,
  ModalSubmit,
  MoveDown,
  MoveUp,
  OpenGotoLine,
  PageDown,
  PageUp,
  ResizeEvent,
  ScrollDown
}
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManagerTestFacade.{createBuffer, updateState}
import com.serenity.state.manager.{CursorViewport, DamageProducer, RenderCaches, StateManager}
import com.serenity.state.models.*
import com.serenity.ui.layout.{CellMetrics, PanelPosition, ViewportSize}
import com.serenity.ui.renderer.{
  FrameOutput,
  Java2DRenderSurface,
  Java2DScratchBuffers,
  RendererEntryPoints,
  RendererFramePlanner,
  ScreenIdentity
}
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

  private lazy val proseThemeAlpha = proseState.persisted.theme.background.getAlpha

  /** A frame buffer at the window's device size; reused across iterations, as the app's own image pool does. Opaque, as
    * `SwingWindow.frameImageType` picks for an opaque theme in an opaque window.
    */
  def frameImage(): BufferedImage =
    new BufferedImage(
      math.ceil(LogicalWidthPx * DeviceScale).toInt,
      math.ceil(LogicalHeightPx * DeviceScale).toInt,
      SwingWindow.frameImageType(translucentWindow = false, proseThemeAlpha)
    )

  /** One window's effect buffers, shared by every frame as `Java2DRenderSurface.forFrame` shares them in the app. */
  private val scratch = Java2DScratchBuffers()

  def renderedFrame(
    state: AppState,
    caches: RenderCaches,
    image: BufferedImage,
    cursorVisible: Boolean = true
  ): BufferedImage =
    val surface = new Java2DRenderSurface(
      image,
      cellMetrics,
      codeFont,
      _ => (),
      logicalWidthPx = LogicalWidthPx,
      logicalHeightPx = LogicalHeightPx,
      deviceScaleX = DeviceScale,
      deviceScaleY = DeviceScale,
      scratch = scratch
    )
    RendererEntryPoints.render(
      state,
      cursorVisible,
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
    val row = image.getHeight / 2
    (0 until image.getWidth).exists(x => image.getRGB(x, row) != image.getRGB(0, row))

  /** The laptop-sized window [[presentBenchmark]] paints into. */
  def presentWindowResource(metrics: CellMetrics, chromeMetrics: CellMetrics): Resource[IO, SwingWindow] =
    SwingWindow.resource(
      metrics = metrics,
      chromeMetrics = chromeMetrics,
      preferredWindowSize = Some(PreferredWindowSize(LogicalWidthPx, LogicalHeightPx))
    )

  def benchmarks(presentWindow: SwingWindow)(using IORuntime): List[BenchmarkRunner.Benchmark] =
    renderBenchmarks ++ interactionBenchmarks ++ inputBenchmarks ++
      List(presentBenchmark(presentWindow), presentCaretBenchmark(presentWindow))

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
    * swapped in wholesale can fail validation, and then every applied event is rejected and nothing is measured. It is
    * resized to the laptop grid (the StateManager otherwise wraps at its 80x24 default) and scrolled so the cursor is
    * on screen, since off-screen editing skips work a real session pays for.
    */
  private def proseStateManager(
    paragraphs: Int = 300,
    config: AppConfig = AppConfig.default,
    cursorLine: Int = 150
  )(using IORuntime): (StateManager, BufferId) =
    given LoggerFactory[IO] = NoOpFactory[IO]
    val sessionRoot         = Files.createTempDirectory("serenity-laptop-benchmarks")
    val stateManager = StateManager.apply(NoOpLogger[IO], sessionRootOverride = Some(sessionRoot)).unsafeRunSync()
    val proseBufferId = (for
      _       <- stateManager.updateState(state => state.copy(persisted = state.persisted.copy(config = config)))
      created <- stateManager.createBuffer(BenchmarkFixtures.loremIpsumProse(paragraphs), None)
      state   <- stateManager.getCurrentState
      paneId = state.persisted.layout.editorPanes.keys.head
      _ <- stateManager.setBufferForPane(paneId, created)
      _ <- stateManager.applyEvent(ResizeEvent(viewport))
      _ <- stateManager.setCursorPosition(paneId, cursorLine, 40)
      _ <- stateManager.updateState(scrolledToCursor(created))
    yield created).unsafeRunSync()
    (stateManager, proseBufferId)

  private def scrolledToCursor(id: BufferId)(state: AppState): AppState =
    state.persisted.buffers.get(id).flatMap(buffer => buffer.editing.cursorPositions.headOption.map(buffer -> _)) match
      case Some((buffer, cursor)) =>
        val placed = buffer.copy(viewport = CursorViewport.adjustForCursor(buffer, state, cursor))
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers.updated(id, placed)))
      case None => state

  private def inputBenchmarks(using IORuntime): List[BenchmarkRunner.Benchmark] =
    val (stateManager, proseBufferId) = proseStateManager()
    def buffer: Option[Buffer] = stateManager.getCurrentState.unsafeRunSync().persisted.buffers.get(proseBufferId)
    def cursor: Option[CursorPosition] = buffer.flatMap(_.editing.cursorPositions.headOption)
    def length: Option[Int]            = buffer.map(_.document.content.weight)
    def roundTrip(there: Event, back: Event): Unit =
      (stateManager.applyEvent(there) >> stateManager.applyEvent(back)).unsafeRunSync()
    def cursorOnScreen: Boolean =
      buffer.zip(cursor).exists((b, c) => c.line >= b.viewport.topLine && b.viewport.visibleLines > 24)
    // A fresh letter each keystroke, so the edited line's text is new every time and never served from a cache.
    val typedLetters = Iterator.continually('a' to 'z').flatten
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
            moved != before && cursor == before && cursorOnScreen,
            s"cursor did not move down and back on screen: $before -> $moved -> $cursor, viewport ${buffer.map(_.viewport)}"
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
      ),
      BenchmarkRunner.Benchmark(
        "laptop.input.state_manager.continuous_typing",
        3,
        20,
        () =>
          val before = length
          stateManager.applyEvent(InsertChar(typedLetters.next())).unsafeRunSync()
          assert(length == before.map(_ + 1) && cursorOnScreen, s"typing did not grow the document: $before -> $length")
        ,
        () => stateManager.applyEvent(InsertChar(typedLetters.next())).unsafeRunSync()
      ),
      BenchmarkRunner.Benchmark(
        "laptop.input.state_manager.page_down_up",
        3,
        20,
        () =>
          val before = cursor.map(_.line)
          stateManager.applyEvent(PageDown).unsafeRunSync()
          val moved = cursor.map(_.line)
          stateManager.applyEvent(PageUp).unsafeRunSync()
          assert(
            moved.exists(m => before.exists(_ < m)) && cursor.map(_.line) == before && cursorOnScreen,
            s"cursor did not page down and back on screen: $before -> $moved -> $cursor"
          )
        ,
        () => roundTrip(PageDown, PageUp)
      )
    ) ++ typewriterBenchmarks ++ goToLineBenchmarks

  private def typewriterBenchmarks(using IORuntime): List[BenchmarkRunner.Benchmark] =
    val typewriter =
      AppConfig.default.copy(surfaceConfig = AppConfig.default.surfaceConfig.copy(typewriterScrollingEnabled = true))
    val (stateManager, proseBufferId) = proseStateManager(config = typewriter)
    def length: Option[Int] =
      stateManager.getCurrentState.unsafeRunSync().persisted.buffers.get(proseBufferId).map(_.document.content.weight)
    def keystroke(): Unit = (stateManager.applyEvent(InsertChar('x')) >> stateManager.applyEvent(DeleteBackward))
      .unsafeRunSync()
    List(
      BenchmarkRunner.Benchmark(
        "laptop.input.state_manager.typewriter_keystroke",
        3,
        20,
        () =>
          val before = length
          keystroke()
          assert(length == before, s"typewriter keystroke changed the document length: $before -> $length")
        ,
        () => keystroke()
      )
    )

  /** Go to Line through its prompt, jumping between the start and the end of a 3000-paragraph document. */
  private def goToLineBenchmarks(using IORuntime): List[BenchmarkRunner.Benchmark] =
    val (stateManager, proseBufferId) = proseStateManager(paragraphs = 3000, cursorLine = 100)
    def cursorLine: Option[Int] = stateManager.getCurrentState
      .unsafeRunSync()
      .persisted
      .buffers
      .get(proseBufferId)
      .flatMap(_.editing.cursorPositions.headOption)
      .map(_.line)
    def goTo(line: Int): Unit =
      val typed = line.toString.toList.map(ModalInsertChar(_))
      (OpenGotoLine :: typed ::: List(ModalSubmit)).foreach(event => stateManager.applyEvent(event).unsafeRunSync())
    def jumpAndBack(): Unit =
      goTo(2900)
      goTo(100)
    List(
      BenchmarkRunner.Benchmark(
        "laptop.input.state_manager.go_to_line_3000_paragraphs",
        2,
        10,
        () =>
          goTo(2900)
          val jumped = cursorLine
          goTo(100)
          assert(jumped.contains(2899) && cursorLine.contains(99), s"go to line landed on $jumped then $cursorLine")
        ,
        () => jumpAndBack()
      )
    )

  private val uiFont    = Font(Font.SANS_SERIF, Font.PLAIN, codeFont.getSize).deriveFont(codeFont.getSize2D)
  private val uiMetrics = CellMetrics.fromFont(uiFont)

  /** A window's frame loop over one persisting image: one screen, and every frame told what the transition into it
    * damaged, as `AppRuntime` reports it, so only damaged rows are redrawn and the rest of the image is kept. Like the
    * app's base frames it leaves the caret out; the window fills carets over the presented frame.
    */
  final private class PersistingFrames:
    private val image  = frameImage()
    private val caches = RenderCaches.create()
    private val screen = ScreenIdentity(this)

    def render(state: AppState, damage: Damage): BufferedImage =
      val effective = RendererEntryPoints.withEffectiveTheme(state)
      val surface = new Java2DRenderSurface(
        image,
        cellMetrics,
        codeFont,
        _ => (),
        logicalWidthPx = LogicalWidthPx,
        logicalHeightPx = LogicalHeightPx,
        deviceScaleX = DeviceScale,
        deviceScaleY = DeviceScale,
        contentPersists = true,
        layerCacheOwnerOverride = Some(screen),
        scratch = scratch
      )
      val _ = RendererFramePlanner.renderFrame(
        effective,
        cursorVisible = false,
        surface,
        viewport,
        caches.authoritativeScene.forState(effective, viewport, codeFont, textFont),
        codeFont,
        textFont,
        uiFont,
        cellMetrics,
        uiMetrics,
        None,
        Some(FrameOutput(screen, new AtomicReference(None))),
        damage,
        caches = caches
      )
      image

  /** One step of a transition and back again, each frame carrying the damage `DamageProducer` reports for its step --
    * computed once up front, so the benchmark times the frame alone.
    */
  final private class AlternatingTransition(before: AppState, after: AppState):
    private val frames   = PersistingFrames()
    private val forward  = DamageProducer.forTransition(before, after)
    private val backward = DamageProducer.forTransition(after, before)
    private val steps    = Iterator.continually(List(after -> forward, before -> backward)).flatten
    val _                = frames.render(before, Damage.Everything)

    def forwardDamage: Damage = forward

    def nextFrame(): BufferedImage =
      val (state, damage) = steps.next()
      frames.render(state, damage)

    /** Both damaged frames must match a frame drawn from scratch for the same state. A few antialiased pixels of
      * line-number ink bleed into the first text column and are not restored on a damaged frame; that predates these
      * benchmarks, so it is tolerated up to [[ToleratedStalePixels]] rather than hidden.
      */
    def verify(name: String): Unit =
      List(after, before).foreach { expected =>
        val drawn = nextFrame()
        val diff =
          differingPixels(drawn, renderedFrame(expected, RenderCaches.create(), frameImage(), cursorVisible = false))
        assert(
          diff.size <= ToleratedStalePixels,
          s"$name: the damaged frame differs from a full frame in ${diff.size} pixels: ${diff.take(8).mkString(" ")}"
        )
      }

  private val ToleratedStalePixels = 32

  /** The device pixels where `a` and `b` differ, as `(x, y)`. */
  private def differingPixels(a: BufferedImage, b: BufferedImage): IndexedSeq[(Int, Int)] =
    val width   = a.getWidth
    val pixelsA = a.getRGB(0, 0, width, a.getHeight, null, 0, width)
    val pixelsB = b.getRGB(0, 0, width, b.getHeight, null, 0, width)
    pixelsA.indices.filter(index => pixelsA(index) != pixelsB(index)).map(index => (index % width, index / width))

  /** Typing and wheel-scrolling as a session at this size meets them. The states come from a live `StateManager` (see
    * [[proseStateManager]]), so the damage is what the app itself reports for the step. Typing alternates one inserted
    * letter with its removal, so the edited row's wrapped layout is served from cache, unlike a real new keystroke.
    */
  private def interactionBenchmarks(using IORuntime): List[BenchmarkRunner.Benchmark] =
    val (stateManager, _) = proseStateManager()
    def current: AppState = stateManager.getCurrentState.unsafeRunSync()
    val beforeTyping      = current
    stateManager.applyEvent(InsertChar('x')).unsafeRunSync()
    val afterTyping = current
    stateManager.applyEvent(ScrollDown(3)).unsafeRunSync()
    val afterScroll = current
    val typing      = AlternatingTransition(beforeTyping, afterTyping)
    val scroll      = AlternatingTransition(afterTyping, afterScroll)
    List(
      BenchmarkRunner.Benchmark(
        "laptop.render.typing_frame_2x",
        2,
        12,
        () =>
          assert(typing.forwardDamage != Damage.Everything, "typing damaged the whole frame")
          typing.verify("typing")
        ,
        () => typing.nextFrame()
      ),
      BenchmarkRunner.Benchmark(
        "laptop.render.scroll_frame_2x",
        2,
        12,
        () =>
          assert(scroll.forwardDamage != Damage.Everything, "scrolling damaged the whole frame")
          scroll.verify("scroll")
        ,
        () => scroll.nextFrame()
      ),
      BenchmarkRunner.Benchmark(
        "laptop.damage.typing_transition",
        3,
        20,
        () =>
          assert(DamageProducer.forTransition(beforeTyping, afterTyping) != Damage.Nothing, "typing damaged nothing"),
        () => DamageProducer.forTransition(beforeTyping, afterTyping)
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

  /** A blink-tick repaint: only the caret's own rectangle is repainted over an unchanged base frame. */
  private def presentCaretBenchmark(presentWindow: SwingWindow): BenchmarkRunner.Benchmark =
    val canvas = presentWindow.canvas
    val frame  = new BufferedImage(canvas.getWidth.max(1), canvas.getHeight.max(1), BufferedImage.TYPE_INT_ARGB)
    val caret  = SwingWindow.CaretPaint(new Rectangle(700, 500, 2, 18), Color.WHITE)
    def publishCaret(): Boolean = presentWindow.onCursorOverlayReady(Some(new Rectangle(0, 0, 0, 0)))(List(caret))
    BenchmarkRunner.Benchmark(
      "laptop.present.swing_paint_caret_1500x1000",
      2,
      8,
      () =>
        SwingUtilities.invokeAndWait { () =>
          presentWindow.onBaseImageReady(frame)
          assert(publishCaret(), "no base frame to fill the caret over")
        },
      () =>
        SwingUtilities.invokeAndWait { () =>
          val _ = publishCaret()
          canvas.paintImmediately(caret.rect)
        }
    )
