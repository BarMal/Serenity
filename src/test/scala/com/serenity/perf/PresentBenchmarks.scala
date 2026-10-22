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
import com.serenity.keystroke.events.ScrollDown
import com.serenity.rope.Balance
import com.serenity.state.manager.{DamageProducer, RenderCaches}
import com.serenity.state.models.{AppState, Damage}
import com.serenity.ui.layout.{CellMetrics, ViewportSize}
import com.serenity.ui.renderer.{
  FrameOutput,
  FrameSurfaceFactory,
  FrameSurfaceRequest,
  RendererFramePlanner,
  ScreenIdentity
}
import com.serenity.ui.skia.SkiaRasterSurfaceFactory
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

  final private[perf] class Window(
      factory: FrameSurfaceFactory,
      widthPx: Int,
      heightPx: Int,
      scale: Double,
      grid: ViewportSize
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

    def lastFrameCounts: String =
      val counts =
        caches.frameCounters.lastFrame.values.toList.sortBy(_._1.toString).map((k, v) => s"$k=$v").mkString(" ")
      val copied = SkiaRasterSurfaceFactory.lastCopiedPixels(caches.backendCaches).fold("")(px => s" copiedPx=$px")
      counts + copied

    def render(state: AppState, damage: Damage): BufferedImage =
      val repaintRegion = new AtomicReference[Option[List[com.serenity.ui.layout.PixelRect]]](None)
      val surface = factory.create(
        FrameSurfaceRequest(
          metrics,
          codeFont,
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
        caches.authoritativeScene.forState(state, grid, codeFont, textFont),
        codeFont,
        textFont,
        uiFont,
        metrics,
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

  final private class Alternating(window: Window, before: AppState, after: AppState):
    private val forward  = DamageProducer.forTransition(before, after)
    private val backward = DamageProducer.forTransition(after, before)
    private val steps    = Iterator.continually(List(after -> forward, before -> backward)).flatten
    private val _        = window.render(before, Damage.Everything)

    def forwardDamage: Damage   = forward
    def lastFrameCounts: String = window.lastFrameCounts

    def next(): BufferedImage =
      val (state, damage) = steps.next()
      window.render(state, damage)

  private def codeText: String =
    val root  = Path.of("src", "main", "scala", "com", "serenity", "ui", "renderer")
    val files = Files.list(root).iterator().asScala.filter(_.toString.endsWith(".scala")).toList.sortBy(_.toString)
    files.flatMap(file => Files.readAllLines(file).asScala).take(6000).mkString("\n")

  /** The states and windows every row draws, made once for a window size and scale. */
  final private class Scenarios(factory: FrameSurfaceFactory, widthPx: Int, heightPx: Int, scale: Double)(using
      IORuntime
  ):
    val grid = ViewportSize(widthPx / metrics.charWidth, heightPx / metrics.lineHeight)

    private val (prose, _)                  = LaptopFrameBenchmarks.proseStateManager(grid = grid)
    private val (beforeTyping, afterTyping) = TypingTransition.of(prose)
    prose.applyEvent(ScrollDown(3)).unsafeRunSync()
    val proseFull: AppState = prose.getCurrentState.unsafeRunSync()

    private val (code, _) = LaptopFrameBenchmarks.stateManagerHolding(
      codeText,
      AppConfig.conventional,
      cursorLine = 200,
      cursorColumn = 10,
      grid = grid
    )

    val codeFull: AppState = code.getCurrentState.unsafeRunSync()

    private def window() = Window(factory, widthPx, heightPx, scale, grid)
    val typing           = Alternating(window(), beforeTyping, afterTyping)
    val scroll           = Alternating(window(), afterTyping, proseFull)
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

  def benchmarks(using IORuntime): List[BenchmarkRunner.Benchmark] =
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
    import scenarios.{codeFull, codeWin, longShown, proseFull, proseWin, scroll, scrollDown, typing}

    def row(name: String, verify: () => Unit, run: () => Any): BenchmarkRunner.Benchmark =
      BenchmarkRunner.Benchmark(s"render.present.$label.$name", 3, 20, verify, run, settleJit = true)

    if sys.props.contains("present.diag") then scenarios.printCounters()

    List(
      row(
        "typing_frame",
        () =>
          assert(typing.forwardDamage != Damage.Everything, "typing damaged the whole frame")
          assert(hasPixels(typing.next()), "typing frame drew nothing")
        ,
        () => typing.next()
      ),
      row(
        "scroll_frame",
        () =>
          assert(scroll.forwardDamage != Damage.Everything, "scrolling damaged the whole frame")
          assert(hasPixels(scroll.next()), "scroll frame drew nothing")
        ,
        () => scroll.next()
      ),
      BenchmarkRunner.Benchmark(
        s"render.present.$label.scroll_down_frame",
        30,
        60,
        () =>
          val before = longShown.get
          assert(hasPixels(scrollDown()), "scroll down drew nothing")
          assert(longShown.get != before, "scroll down did not scroll")
        ,
        () => scrollDown(),
        fixedSampleCount = true
      ),
      row(
        "full_prose_frame",
        () => assert(hasPixels(proseWin.render(proseFull, Damage.Everything)), "prose frame drew nothing"),
        () => proseWin.render(proseFull, Damage.Everything)
      ),
      row(
        "full_code_frame",
        () => assert(hasPixels(codeWin.render(codeFull, Damage.Everything)), "code frame drew nothing"),
        () => codeWin.render(codeFull, Damage.Everything)
      )
    )

  def main(args: Array[String]): Unit =
    import cats.effect.unsafe.implicits.global
    val results = BenchmarkRunner.runMatching(args.toList, benchmarks)
    BenchmarkRunner.printResults(results)
    sys.exit(0)
