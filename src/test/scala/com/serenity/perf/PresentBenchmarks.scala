package com.serenity.perf

import java.awt.geom.AffineTransform
import java.awt.image.{BufferedImage, ColorModel}
import java.awt.{Font, GraphicsConfiguration, GraphicsDevice, GraphicsEnvironment, Rectangle}
import java.nio.file.{Files, Path}
import java.util.concurrent.atomic.AtomicReference
import javax.swing.JPanel

import scala.jdk.CollectionConverters.*

import cats.effect.unsafe.IORuntime
import com.serenity.config.AppConfig
import com.serenity.diagnostics.FrameCounts
import com.serenity.keystroke.events.ScrollDown
import com.serenity.rope.Balance
import com.serenity.state.manager.{DamageProducer, RenderCaches, StateManager}
import com.serenity.state.models.{AppState, Damage}
import com.serenity.ui.layout.{CellMetrics, ViewportSize}
import com.serenity.ui.renderer.{
  FrameOutput,
  FrameSurfaceFactory,
  FrameSurfaceRequest,
  RendererFramePlanner,
  ScreenIdentity
}
import com.serenity.ui.skia.{PresentPhase, SkiaRasterSurfaceFactory}
import com.serenity.ui.terminal.SwingWindow.ReusableImagePool

/** Scratch benchmark for #2380: a window's frame through `FrameSurfaceFactory.Java2D` or `SkiaRasterSurfaceFactory`,
  * end to end from the surface request to the published image (so including the raster copy-out), with the
  * `RendererFramePlanner` and every state, damage and cache identical on both sides.
  *
  * One JVM runs one backend at one window size and scale, so `bench/abba.sh`-style A B B A runs can alternate JVMs. Row
  * names do not mention the backend, so the two sides' CSVs compare. Settings are system properties:
  * `present.backend=java2d|skia`, `present.width`, `present.height` (logical), `present.scale`.
  */
private[perf] object PresentBenchmarks:

  given Balance = Balance.default

  private val codeFont  = LaptopFrameBenchmarks.codeFont
  private val textFont  = Font(Font.SERIF, Font.PLAIN, 14)
  private val uiFont    = Font(Font.SANS_SERIF, Font.PLAIN, 12)
  private val metrics   = CellMetrics.fromFont(codeFont)
  private val uiMetrics = CellMetrics.fromFont(uiFont)

  /** A canvas that reports a device scale without being on screen. */
  final private class ScaledConfiguration(scale: Double) extends GraphicsConfiguration:
    private val underlying =
      GraphicsEnvironment.getLocalGraphicsEnvironment.getDefaultScreenDevice.getDefaultConfiguration
    def getDevice: GraphicsDevice                = underlying.getDevice
    def getColorModel: ColorModel                = underlying.getColorModel
    def getColorModel(t: Int): ColorModel        = underlying.getColorModel(t)
    def getDefaultTransform: AffineTransform     = AffineTransform.getScaleInstance(scale, scale)
    def getNormalizingTransform: AffineTransform = AffineTransform.getScaleInstance(scale, scale)
    def getBounds: Rectangle                     = underlying.getBounds

  /** The fonts a window draws with; the cell metrics follow the code font. */
  final private[perf] case class Fonts(code: Font, text: Font):
    val metrics: CellMetrics = CellMetrics.fromFont(code)

  private[perf] object Fonts:
    val default: Fonts = Fonts(codeFont, textFont)

  final private[perf] class Window(
      factory: FrameSurfaceFactory,
      widthPx: Int,
      heightPx: Int,
      scale: Double,
      grid: ViewportSize,
      fonts: Fonts = Fonts.default
  ):

    private val canvas = new JPanel:
      override def getGraphicsConfiguration: GraphicsConfiguration =
        if scale == 1.0 then super.getGraphicsConfiguration else ScaledConfiguration(scale)

    canvas.setSize(widthPx, heightPx)
    private val pool   = new ReusableImagePool()
    private val caches = RenderCaches.create()
    caches.frameCounters.setEnabled(true)
    private val screen    = ScreenIdentity(canvas)
    private val published = new AtomicReference[BufferedImage]()

    def backendCaches: com.serenity.ui.renderer.BackendCaches = caches.backendCaches

    def lastFrame: FrameCounts = caches.frameCounters.lastFrame

    /** What the Skia raster path spent in each present phase since this was last asked; nothing on Java2D. */
    def drainPhases(): Map[PresentPhase, Long] =
      SkiaRasterSurfaceFactory.drainPhases(caches.backendCaches).getOrElse(Map.empty)

    def lastFrameCounts: String =
      val counts =
        lastFrame.values.toList.sortBy(_._1.toString).map((k, v) => s"$k=$v").mkString(" ")
      val copied = SkiaRasterSurfaceFactory.lastCopiedPixels(caches.backendCaches).fold("")(px => s" copiedPx=$px")
      counts + copied

    def render(state: AppState, damage: Damage): BufferedImage =
      val repaintRegion = new AtomicReference[Option[List[com.serenity.ui.layout.PixelRect]]](None)
      val surface = factory.create(
        FrameSurfaceRequest(
          fonts.metrics,
          fonts.code,
          canvas,
          image =>
            pool.publish(image)
            published.set(image)
          ,
          pool.acquire,
          caches.surfaceText,
          caches.backendCaches,
          () => repaintRegion.get()
        )
      )
      val _ = RendererFramePlanner.renderFrame(
        state,
        cursorVisible = false,
        surface,
        grid,
        caches.authoritativeScene.forState(state, grid, fonts.code, fonts.text),
        fonts.code,
        fonts.text,
        uiFont,
        fonts.metrics,
        uiMetrics,
        None,
        Some(FrameOutput(screen, repaintRegion)),
        damage,
        caches = caches
      )
      published.get

  private def hasPixels(image: BufferedImage): Boolean =
    val row = image.getHeight / 2
    (0 until image.getWidth).exists(x => image.getRGB(x, row) != image.getRGB(0, row))

  /** Shows `states` in turn, forever, each frame damaged by the transition from the state before it. */
  final private[perf] class Cycling(window: Window, states: Vector[AppState]):

    private val transitions = states.indices.toVector.map { i =>
      val following = states((i + 1) % states.size)
      following -> DamageProducer.forTransition(states(i), following)
    }

    private val steps = Iterator.continually(transitions).flatten
    private val _     = window.render(states.head, Damage.Everything)

    def forwardDamage: Damage                  = transitions.head._2
    def lastFrame: FrameCounts                 = window.lastFrame
    def lastFrameCounts: String                = window.lastFrameCounts
    def drainPhases(): Map[PresentPhase, Long] = window.drainPhases()

    def next(): BufferedImage =
      val (state, damage) = steps.next()
      window.render(state, damage)

  /** The prose window's states a benchmark shows: the typing pair, and the same document scrolled three lines at a
    * time. The scroll states are three, not two, because the window's pool hands out two images in turn: two states
    * would give each image the same state every time, and Java2D, which keeps what an image shows, would draw nothing.
    */
  final private[perf] class ProseStates(prose: StateManager)(using IORuntime):
    val (beforeTyping, afterTyping) = TypingTransition.of(prose)

    private def scrolled(): AppState =
      prose.applyEvent(ScrollDown(3)).unsafeRunSync()
      prose.getCurrentState.unsafeRunSync()

    val proseFull: AppState           = scrolled()
    val scrollCycle: Vector[AppState] = Vector(afterTyping, proseFull, scrolled())

  /** The `scroll_frame` row's frames on `factory`, for a spec to count what each one draws. */
  private[perf] def scrollCycle(factory: FrameSurfaceFactory, widthPx: Int, heightPx: Int, scale: Double)(using
    IORuntime
  ): Cycling =
    val grid       = ViewportSize(widthPx / metrics.charWidth, heightPx / metrics.lineHeight)
    val (prose, _) = LaptopFrameBenchmarks.proseStateManager(grid = grid)
    val states     = ProseStates(prose)
    Cycling(Window(factory, widthPx, heightPx, scale, grid), states.scrollCycle)

  private def codeText: String =
    val root  = Path.of("src", "main", "scala", "com", "serenity", "ui", "renderer")
    val files = Files.list(root).iterator().asScala.filter(_.toString.endsWith(".scala")).toList.sortBy(_.toString)
    files.flatMap(file => Files.readAllLines(file).asScala).take(6000).mkString("\n")

  /** The states and windows every row draws, made once for a window size and scale. */
  final private class Scenarios(factory: FrameSurfaceFactory, widthPx: Int, heightPx: Int, scale: Double)(using
      IORuntime
  ):
    val grid = ViewportSize(widthPx / metrics.charWidth, heightPx / metrics.lineHeight)

    private val (prose, _) = LaptopFrameBenchmarks.proseStateManager(grid = grid)
    private val states     = ProseStates(prose)
    import states.{afterTyping, beforeTyping}
    val proseFull: AppState = states.proseFull

    private val (code, _) = LaptopFrameBenchmarks.stateManagerHolding(
      codeText,
      AppConfig.conventional,
      cursorLine = 200,
      cursorColumn = 10,
      grid = grid
    )

    val codeFull: AppState = code.getCurrentState.unsafeRunSync()

    private def window() = Window(factory, widthPx, heightPx, scale, grid)
    val typing           = Cycling(window(), Vector(beforeTyping, afterTyping))
    val scroll           = Cycling(window(), states.scrollCycle)
    val proseWin         = window()
    val codeWin          = window()
    val _                = proseWin.render(proseFull, Damage.Everything)
    val __               = codeWin.render(codeFull, Damage.Everything)

    private val (long, _) = LaptopFrameBenchmarks.proseStateManager(paragraphs = 6000, grid = grid)
    val longWindow        = window()
    val longShown         = new AtomicReference(long.getCurrentState.unsafeRunSync())
    val _____             = longWindow.render(longShown.get, Damage.Everything)

    def scrollDown(): BufferedImage =
      val before = longShown.get
      long.applyEvent(ScrollDown(3)).unsafeRunSync()
      val after = long.getCurrentState.unsafeRunSync()
      longShown.set(after)
      longWindow.render(after, DamageProducer.forTransition(before, after))

    /** What each kind of frame drew, by the renderer's own counters. */
    def printCounters(): Unit =
      (0 until 4).foreach { i =>
        val _ = scrollDown()
        println(s"DIAG scroll_down ${widthPx}x$heightPx@${scale}x frame $i: ${longWindow.lastFrameCounts}")
      }
      List("typing" -> typing, "scroll" -> scroll).foreach { (label, alt) =>
        (0 until 4).foreach { i =>
          val _ = alt.next()
          println(s"DIAG $label ${widthPx}x$heightPx@${scale}x frame $i: ${alt.lastFrameCounts}")
        }
      }
      List("prose" -> proseWin -> proseFull, "code" -> codeWin -> codeFull).foreach {
        case ((label, win), state) =>
          val _ = win.render(state, Damage.Everything)
          println(s"DIAG full_$label: ${win.lastFrameCounts}")
      }

  /** The phase times of a row's most recent frames, which are the ones its samples were taken from. */
  final private[perf] class PhaseLog(drain: () => Map[PresentPhase, Long]):
    private val RecentFrames = 200
    private val recent       = new AtomicReference(Vector.empty[PresentReport.FramePhases])

    def timing(run: () => Any): () => Any = () =>
      val started    = System.nanoTime()
      val result     = run()
      val frameNanos = System.nanoTime() - started
      val phases     = drain()
      val _ = recent.updateAndGet { frames =>
        (frames :+ PresentReport.FramePhases(
          phases.getOrElse(PresentPhase.Plan, 0L),
          phases.getOrElse(PresentPhase.SurfaceMove, 0L),
          phases.getOrElse(PresentPhase.ImageShift, 0L),
          phases.getOrElse(PresentPhase.CopyOut, 0L),
          frameNanos
        )).takeRight(RecentFrames)
      }
      result

    def breakdown: PresentReport.PhaseBreakdown = PresentReport.PhaseBreakdown.median(recent.get())

  /** The rows to run, and the phase breakdown of each once it has run. */
  final private[perf] class Rows(
      val benchmarks: List[BenchmarkRunner.Benchmark],
      logs: Map[String, PhaseLog]
  ):
    def breakdownOf(name: String): Option[PresentReport.PhaseBreakdown] = logs.get(name).map(_.breakdown)

  def benchmarks(using IORuntime): Rows =
    val backend  = sys.props.getOrElse("present.backend", "java2d")
    val widthPx  = sys.props.getOrElse("present.width", "1920").toInt
    val heightPx = sys.props.getOrElse("present.height", "1080").toInt
    val scale    = sys.props.getOrElse("present.scale", "1").toDouble
    val label    = s"${heightPx}p_${scale.toInt}x"
    val factory =
      backend match
        case "java2d" => FrameSurfaceFactory.Java2D
        case "skia"   => SkiaRasterSurfaceFactory
        case other    => throw IllegalArgumentException(s"present.backend must be java2d or skia, not $other")
    val scenarios = Scenarios(factory, widthPx, heightPx, scale)
    import scenarios.{codeFull, codeWin, longShown, longWindow, proseFull, proseWin, scroll, scrollDown, typing}

    def named(name: String): String = s"render.present.$label.$name"

    def row(
      name: String,
      drain: () => Map[PresentPhase, Long],
      verify: () => Unit,
      run: () => Any
    ): (BenchmarkRunner.Benchmark, (String, PhaseLog)) =
      val log = PhaseLog(drain)
      BenchmarkRunner.Benchmark(named(name), 3, 20, verify, log.timing(run), settleJit = true) -> (named(name) -> log)

    if sys.props.contains("present.diag") then scenarios.printCounters()

    val scrollDownRow =
      val log = PhaseLog(longWindow.drainPhases)
      BenchmarkRunner.Benchmark(
        named("scroll_down_frame"),
        30,
        60,
        () =>
          val before = longShown.get
          assert(hasPixels(scrollDown()), "scroll down drew nothing")
          assert(longShown.get != before, "scroll down did not scroll")
        ,
        log.timing(() => scrollDown()),
        fixedSampleCount = true
      ) -> (named("scroll_down_frame") -> log)

    val rows = List(
      row(
        "typing_frame",
        typing.drainPhases,
        () =>
          assert(typing.forwardDamage != Damage.Everything, "typing damaged the whole frame")
          assert(hasPixels(typing.next()), "typing frame drew nothing")
        ,
        () => typing.next()
      ),
      row(
        "scroll_frame",
        scroll.drainPhases,
        () =>
          assert(scroll.forwardDamage != Damage.Everything, "scrolling damaged the whole frame")
          assert(hasPixels(scroll.next()), "scroll frame drew nothing")
        ,
        () => scroll.next()
      ),
      scrollDownRow,
      row(
        "full_prose_frame",
        proseWin.drainPhases,
        () => assert(hasPixels(proseWin.render(proseFull, Damage.Everything)), "prose frame drew nothing"),
        () => proseWin.render(proseFull, Damage.Everything)
      ),
      row(
        "full_code_frame",
        codeWin.drainPhases,
        () => assert(hasPixels(codeWin.render(codeFull, Damage.Everything)), "code frame drew nothing"),
        () => codeWin.render(codeFull, Damage.Everything)
      )
    )
    Rows(rows.map(_._1), rows.map(_._2).toMap)

  def main(args: Array[String]): Unit =
    import cats.effect.unsafe.implicits.global
    val rows    = benchmarks
    val results = BenchmarkRunner.runMatching(args.toList, rows.benchmarks)
    PresentReport.print(results, rows.breakdownOf)
    sys.exit(0)
