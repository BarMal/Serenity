package com.serenity.perf

import java.awt.Font
import java.util.concurrent.atomic.AtomicReference

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import com.serenity.rope.Balance
import com.serenity.state.manager.{DamageProducer, RenderCaches}
import com.serenity.state.models.{AppState, Damage}
import com.serenity.ui.layout.{CellMetrics, ViewportSize}
import com.serenity.ui.renderer.{FontSpec, FrameOutput, RendererFramePlanner, ScreenIdentity}
import com.serenity.ui.sdl.{Sdl, SdlGlWindow}
import com.serenity.ui.skia.{GlFrameSettings, SkiaGlContext, SkiaGlFrames, SkiaGlTargets}
import com.serenity.{FramePixels, SkiaFrames}

/** What drawing straight onto a GL window's surface costs the thread that does it (#2380): the same typing frame, an
  * idle frame (same state, nothing damaged, so almost all planning and no drawing) and a cold frame, run by the real
  * renderer on the SDL thread over the GL surface, and over Skia's CPU raster surface for comparison.
  *
  * Not part of the perf gate. Run it with
  * `sbt -Dserenity.sdl3.library.path=<directory holding libSDL3.so.0> "Test / runMain com.serenity.perf.SkiaGlFrameBenchmarks"`
  * (optionally followed by benchmark name prefixes); `-Dserenity.sdl3.driver=wayland` runs it on a compositor, and the
  * default is SDL's `offscreen` driver, where GL is Mesa's software rasteriser. The GL numbers then say what the
  * renderer's own work costs the SDL thread, not what a GPU frame costs; the idle frame is the part a GPU does not
  * change.
  */
private[perf] object SkiaGlFrameBenchmarks:

  given Balance = Balance.default

  private val LogicalWidthPx  = LaptopFrameBenchmarks.LogicalWidthPx
  private val LogicalHeightPx = LaptopFrameBenchmarks.LogicalHeightPx
  private val DeviceScale     = LaptopFrameBenchmarks.DeviceScale
  private val codeFont        = LaptopFrameBenchmarks.codeFont
  private val textFont        = Font(Font.SERIF, Font.PLAIN, 14)
  private val uiFont          = Font(Font.SANS_SERIF, Font.PLAIN, 12)

  /** A way to draw one frame of `state` with `damage`, the same renderer call whatever it draws onto. */
  trait Frames:
    def render(state: AppState, damage: Damage): Unit

  final private class Renderer(
      metrics: CellMetrics,
      grid: ViewportSize,
      draw: (com.serenity.ui.renderer.RenderSurface => Unit) => Unit,
      caches: RenderCaches
  ) extends Frames:
    private val screen    = ScreenIdentity(this)
    private val uiMetrics = CellMetrics.fromFont(uiFont)

    def render(state: AppState, damage: Damage): Unit =
      draw { surface =>
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
          Some(FrameOutput(screen, new AtomicReference(None))),
          damage,
          caches = caches
        )
      }

  private def onGl(frames: SkiaGlFrames, settings: GlFrameSettings)(
    render: com.serenity.ui.renderer.RenderSurface => Unit
  ): Unit =
    frames.draw(settings)(render).fold(reason => throw new IllegalStateException(reason), identity)

  private def glFrames(context: SkiaGlContext, metrics: CellMetrics, grid: ViewportSize): Frames =
    val caches = RenderCaches.create()
    val settings = GlFrameSettings(
      metrics,
      FontSpec.fromAwt(codeFont),
      LogicalWidthPx,
      LogicalHeightPx,
      DeviceScale,
      caches.surfaceText,
      caches.backendCaches,
      ScreenIdentity(new Object)
    )
    val targets = SkiaGlTargets(context)
    Renderer(metrics, grid, onGl(SkiaGlFrames(context, targets), settings), caches)

  private def cpuFrames(metrics: CellMetrics, grid: ViewportSize): Frames =
    val caches = RenderCaches.create()
    val frame: FramePixels =
      SkiaFrames.newFrame(
        math.ceil(LogicalWidthPx * DeviceScale).toInt,
        math.ceil(LogicalHeightPx * DeviceScale).toInt
      )
    val surface = SkiaFrames.surfaceOver(
      frame,
      metrics,
      codeFont,
      LogicalWidthPx,
      LogicalHeightPx,
      DeviceScale,
      persists = true,
      caches
    )
    Renderer(metrics, grid, draw => draw(surface), caches)

  private def benchmarksFor(
    label: String,
    frames: Frames,
    before: AppState,
    after: AppState
  ): List[BenchmarkRunner.Benchmark] =
    val forward  = DamageProducer.forTransition(before, after)
    val backward = DamageProducer.forTransition(after, before)
    val steps    = Iterator.continually(List(after -> forward, before -> backward)).flatten
    frames.render(before, Damage.Everything)
    List(
      BenchmarkRunner.Benchmark(
        s"skia_gl.$label.typing_frame_2x",
        2,
        12,
        () => assert(forward != Damage.Everything, "typing damaged the whole frame"),
        () =>
          val (state, damage) = steps.next()
          frames.render(state, damage)
      ),
      BenchmarkRunner.Benchmark(
        s"skia_gl.$label.idle_frame_2x",
        2,
        12,
        () => (),
        () => frames.render(after, Damage.Nothing)
      ),
      BenchmarkRunner.Benchmark(
        s"skia_gl.$label.cold_frame_2x",
        2,
        6,
        () => (),
        () => frames.render(after, Damage.Everything)
      )
    )

  private def benchmarks(context: SkiaGlContext)(using IORuntime): List[BenchmarkRunner.Benchmark] =
    val metrics           = CellMetrics.fromFont(codeFont)
    val grid              = ViewportSize(LogicalWidthPx / metrics.charWidth, LogicalHeightPx / metrics.lineHeight)
    val (stateManager, _) = LaptopFrameBenchmarks.proseStateManager(grid = grid)
    val (before, after)   = TypingTransition.of(stateManager)
    benchmarksFor("cpu_raster", cpuFrames(metrics, grid), before, after) ++
      benchmarksFor("gl", glFrames(context, metrics, grid), before, after)

  def main(args: Array[String]): Unit =
    import cats.effect.unsafe.implicits.global
    val sdl = Sdl.load(sys.props.toMap, java.nio.file.Files.exists(_)) match
      case Right(loaded) => loaded
      case Left(error)   => sys.error(s"libSDL3 did not load: ${error.describe}")
    val driver = sys.props.getOrElse("serenity.sdl3.driver", "offscreen")
    val results = SdlGlWindow
      .open(sdl, SdlGlWindow.Settings("skia gl frame benchmarks", 96, 64, Some(driver)))
      .use {
        case Right(window) =>
          window.onSdlThread { owner =>
            SkiaGlContext.open(owner) match
              case Right(context) =>
                try BenchmarkRunner.runMatching(args.toList, benchmarks(context))
                finally context.close()
              case Left(error) => sys.error(s"no Skia GL context: ${error.describe}")
          }
        case Left(error) => IO.raiseError(new IllegalStateException(s"no GL window: ${error.describe}"))
      }
      .unsafeRunSync()
    BenchmarkRunner.printResults(results)
