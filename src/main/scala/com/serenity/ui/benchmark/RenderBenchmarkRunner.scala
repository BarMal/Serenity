package com.serenity.ui.benchmark

import java.awt.Dimension
import java.util.concurrent.locks.LockSupport
import javax.swing.SwingUtilities

import scala.util.Try

import cats.syntax.either.*
import com.serenity.config.WindowChromeMode
import com.serenity.rope.Balance
import com.serenity.state.manager.RenderCaches
import com.serenity.ui.color.RenderColor
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.layout.CellMetrics
import com.serenity.ui.renderer.{FontSpec, FrameSurfaceFactory, Java2DRenderSurface, RendererCursorOverlay}
import com.serenity.ui.skia.{SkiaLayerHost, SkiaTextCaches, SynchronousSkiaLayerSurfaceFactory, WarmupStats}
import com.serenity.ui.terminal.{LayerDrawStats, SwingWindow}

/** Times the same scripted document on Java2D and on the Skia layer, each in a real window of its own.
  *
  * A frame is timed from the moment the renderer is called to the last time the canvas painted (Java2D) or the layer
  * drew (Skia) for it, and the next frame starts only once nothing has been presented for a moment, so what a frame
  * spread over several paints or draws costs is all counted, and in the same way on both. A frame nothing ever
  * presented is counted as dropped.
  */
object RenderBenchmarkRunner:

  private val QuietNanos     = 15_000_000L
  private val DropAfterNanos = 3_000_000_000L
  private val CaretColor     = Some(RenderColor.fromRgba(255, 255, 255))

  final private case class Fonts(
      code: FontSpec,
      text: FontSpec,
      ui: FontSpec,
      codeMetrics: CellMetrics,
      uiMetrics: CellMetrics
  )

  final private case class Timed(stats: FrameStats, coldPhases: Option[ColdFramePhases])

  final private case class Measured(
      stats: Map[Scenario, FrameStats],
      deviceScale: Double,
      renderApi: Option[String],
      layers: Option[Int],
      draws: Option[LayerDrawStats],
      coldPhases: Option[ColdFramePhases]
  )

  def run(
    options: RenderBenchmarkOptions,
    skiaFactory: FrameSurfaceFactory = SynchronousSkiaLayerSurfaceFactory,
    skiaProbe: Either[String, Unit] = Right(()),
    startup: String = "",
    rasterFactory: Option[FrameSurfaceFactory] = None
  ): BenchmarkReport =
    given Balance = Balance.default
    val fonts     = loadFonts()
    val refresh   = RefreshRate.current
    val java2d    = measure(options, "Java2D", FrameSurfaceFactory.Java2D, fonts, refresh)
    val skia      = skiaProbe.map(_ => measure(options, "Skia layer", skiaFactory, fonts, refresh))
    val raster = rasterFactory.fold("no raster arm was asked for".asLeft[Measured]) { factory =>
      skiaProbe.left
        .map(cause => s"Skia is not available: $cause")
        .flatMap(_ =>
          Try(measure(options, "Skia raster", factory, fonts, refresh)).toEither.left
            .map(failure => s"the raster arm failed: ${Option(failure.getMessage).getOrElse(failure.toString)}")
        )
    }
    val api = skia.flatMap(
      _.renderApi.toRight("the layer could not be made, so the window painted with its fallback surface")
    )
    val measuredSkia = skia.toOption.filter(_ => api.isRight)
    val results = Scenario.values.toVector.map(scenario =>
      ScenarioResult(
        scenario,
        java2d.stats.getOrElse(scenario, FrameStats.of(Vector.empty, dropped = 0, refreshNanos = refresh.nanos)),
        measuredSkia.flatMap(_.stats.get(scenario)),
        raster.toOption.flatMap(_.stats.get(scenario))
      )
    )
    val environment = BenchmarkEnvironment(
      s"${System.getProperty("os.name", "?")} ${System.getProperty("os.version", "")}".trim,
      s"Java ${System.getProperty("java.version", "?")}",
      options.widthPx,
      options.heightPx,
      java2d.deviceScale,
      api,
      toolkit = java.awt.Toolkit.getDefaultToolkit.getClass.getName,
      startup = startup,
      skiaLayers = measuredSkia.flatMap(_.layers),
      skiaDraws = measuredSkia.flatMap(_.draws),
      refresh = refresh,
      rasterNote = raster.left.toOption,
      machine = MachineState.current,
      coldPhases = measuredSkia.flatMap(_.coldPhases)
    )
    BenchmarkReport(environment, results, BenchmarkVerdict.of(results, environment))

  private def loadFonts(): Fonts =
    val config = FontLoader.FontConfig()
    val code   = FontLoader.previewCodeFont(config)
    val text   = FontLoader.previewTextFont(config)
    val ui     = FontLoader.previewUiFont(config)
    Fonts(
      FontSpec.fromAwt(code),
      FontSpec.fromAwt(text),
      FontSpec.fromAwt(ui),
      CellMetrics.fromFont(code),
      CellMetrics.fromFont(ui)
    )

  private def measure(
    options: RenderBenchmarkOptions,
    name: String,
    factory: FrameSurfaceFactory,
    fonts: Fonts,
    refresh: RefreshRate
  )(using Balance): Measured =
    val window = new SwingWindow(
      new Dimension(options.widthPx, options.heightPx),
      fonts.codeMetrics,
      WindowChromeMode.Auto,
      fonts.codeMetrics,
      windowTitle = s"Serenity render benchmark: $name"
    )
    window.start()
    settle()
    try
      val viewportLines = window.viewportSize.height
      val timed = Scenario.values.toList.map { scenario =>
        scenario -> timeScenario(window, fonts, factory, scenario, options, viewportLines, refresh)
      }.toMap
      val stats = timed.view.mapValues(_.stats).toMap
      Measured(
        stats,
        Java2DRenderSurface.deviceScaleFor(window.canvas).x,
        window.layerPresenter.map(_.renderApi),
        window.layerPresenter.map(_ => SkiaLayerHost.layersOn(window.canvas)),
        window.layerPresenter.map(_.drawStats),
        timed.get(Scenario.ColdFullFrame).flatMap(_.coldPhases)
      )
    finally
      window.stop()
      settle()

  private def timeScenario(
    window: SwingWindow,
    fonts: Fonts,
    factory: FrameSurfaceFactory,
    scenario: Scenario,
    options: RenderBenchmarkOptions,
    viewportLines: Int,
    refresh: RefreshRate
  )(using Balance): Timed =
    val steps  = BenchmarkScript.steps(scenario, options.warmup + options.frames, viewportLines)
    val shared = RenderCaches.create()
    def cachesFor(step: BenchmarkScript.Step) =
      if step.frame == BenchmarkScript.FrameKind.ColdFull then RenderCaches.create() else shared
    // A frame for the caret to blink over, and a starting point that is the same on both backends.
    steps.headOption.foreach { first =>
      timed(window)(
        draw(
          window,
          fonts,
          factory,
          first.copy(damage = com.serenity.state.models.Damage.Everything, frame = BenchmarkScript.FrameKind.Full),
          shared
        )
      )
    }
    steps.take(options.warmup).foreach(step => timed(window)(draw(window, fonts, factory, step, cachesFor(step))))
    val measured = steps.drop(options.warmup).map { step =>
      val caches = cachesFor(step)
      val sample = timed(window)(draw(window, fonts, factory, step, caches))
      val warmup = Option.when(step.frame == BenchmarkScript.FrameKind.ColdFull)(
        SkiaTextCaches.warmupOf(caches.backendCaches)
      )
      (sample, warmup.flatten)
    }
    val samples = measured.map(_._1)
    val warmups = measured.flatMap(_._2)
    Timed(
      FrameStats.of(
        samples.flatten.map(_.nanos),
        dropped = samples.count(_.isEmpty),
        endedBeforeReturn = samples.flatten.count(_.endedBeforeReturn),
        refreshNanos = refresh.nanos
      ),
      ColdFramePhases.of(warmups.size, warmups.foldLeft(WarmupStats.Snapshot.empty)(_ + _))
    )

  private def draw(
    window: SwingWindow,
    fonts: Fonts,
    factory: FrameSurfaceFactory,
    step: BenchmarkScript.Step,
    caches: RenderCaches
  ): Unit =
    def full(): Unit =
      val _ = RendererCursorOverlay.renderWithCursorOverlay(
        step.state,
        window,
        fonts.code,
        fonts.text,
        fonts.ui,
        fonts.uiMetrics,
        CaretColor,
        step.damage,
        caches,
        factory
      )
    step.frame match
      case BenchmarkScript.FrameKind.Full | BenchmarkScript.FrameKind.ColdFull => full()
      case BenchmarkScript.FrameKind.CaretOnly(visible) =>
        val painted = RendererCursorOverlay.renderCursorOnly(
          step.state,
          visible,
          window,
          fonts.code,
          fonts.text,
          fonts.ui,
          fonts.uiMetrics,
          CaretColor,
          caches
        )
        if !painted then full()

  /** The nanoseconds from calling `render` to the last presentation of what it published, or `None` if there was none.
    */
  private def timed(window: SwingWindow)(render: => Unit): Option[FrameTimer.Sample] =
    val timer =
      new FrameTimer(() => System.nanoTime(), () => LockSupport.parkNanos(1_000_000L), QuietNanos, DropAfterNanos)
    window.setOnFramePresented(() => timer.presented(window.shownFrames))
    val start = System.nanoTime()
    render
    timer.await(start, window.publishedFrames)

  private def settle(): Unit =
    SwingUtilities.invokeAndWait(() => ())
    SwingUtilities.invokeAndWait(() => ())
