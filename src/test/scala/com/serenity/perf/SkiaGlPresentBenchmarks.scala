package com.serenity.perf

import java.lang.foreign.{FunctionDescriptor, Linker}

import scala.concurrent.duration.*

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import cats.syntax.all.*
import com.serenity.rope.Balance
import com.serenity.state.manager.{DamageProducer, RenderCaches}
import com.serenity.state.models.{AppState, Damage}
import com.serenity.testkit.TestMeasure
import com.serenity.ui.layout.{CellMetrics, ViewportSize}
import com.serenity.ui.renderer.{FontSpec, ScreenIdentity}
import com.serenity.ui.sdl.{Sdl, SdlGlWindow}
import com.serenity.ui.skia.GlFrameSettings

/** What a whole SDL3 frame costs on a real compositor (#2380): the presenter's draw, present (copy, carets,
  * `flushAndSubmit`) and swap, one frame at a time, for the typing, idle and cold frames of [[SkiaGlFrameBenchmarks]],
  * with the swap interval at 1 and at 0 and with and without a `glFinish` after the draw and before the swap.
  *
  * It has no input path, no OS input hop and no pacing loop, so `draw_to_rendered` is the render-plus-present-plus-swap
  * floor of a frame, not a key-to-screen time: set it beside the in-app `render` and `publish_to_paint + paint` rows,
  * not `frame_wait`. Without `glFinish` the GPU's work is not in `draw` or `present` (`flushAndSubmit` only submits)
  * and shows up in `swap` or after it; with it the work moves into the row that waited. Where the driver ignores a swap
  * interval of 0 the two interval columns match, and the header says which interval was accepted.
  *
  * Not part of the perf gate. Run it with
  * `sbt "set app / Test / run / javaOptions ++= Seq(\"-Dserenity.sdl3.library.path=<directory holding libSDL3.so.0>\", \"-Dserenity.sdl3.driver=wayland\")" "app/Test/runMain com.serenity.perf.SkiaGlPresentBenchmarks"`
  * (a `-D` on sbt itself does not reach the forked JVM); the default driver is SDL's `offscreen`, where GL is Mesa's
  * software rasteriser and there is no compositor.
  *
  * The window asks for the laptop benchmarks' logical size (or `-Dserenity.sdl3.window=WIDTHxHEIGHT`), but a tiling
  * compositor gives it the tile's size. Its title is the one `bench/hyprland-float.sh` floats, so
  * `bench/hyprland-float.sh on 1500 1000` before the run (and `off` after) keeps the size, and the header says when it
  * did not.
  */
private[perf] object SkiaGlPresentBenchmarks:

  given Balance = Balance.default

  private val codeFont = LaptopFrameBenchmarks.codeFont

  /** Starts with the title `bench/hyprland-float.sh` matches, which is what lets it float the window. */
  val WindowTitle = "Serenity render benchmark: SDL3 present"

  private val SizeProperty = "serenity.sdl3.window"
  private val SizeShape    = raw"(\d+)x(\d+)".r

  /** The logical size to ask for: `-Dserenity.sdl3.window=WIDTHxHEIGHT`, or the laptop benchmarks' own. */
  def requestedSize(properties: Map[String, String]): (Int, Int) =
    properties
      .get(SizeProperty)
      .collect { case SizeShape(width, height) => (width.toInt, height.toInt) }
      .filter((width, height) => width > 0 && height > 0)
      .getOrElse((LaptopFrameBenchmarks.LogicalWidthPx, LaptopFrameBenchmarks.LogicalHeightPx))

  /** Why the numbers are not for the size that was asked for, when the window has other pixels than it. */
  def sizeNote(requestedLogical: (Int, Int), pixels: (Int, Int), scale: Double): Option[String] =
    val wanted = (math.round(requestedLogical._1 * scale).toInt, math.round(requestedLogical._2 * scale).toInt)
    Option.when((pixels._1 - wanted._1).abs > 1 || (pixels._2 - wanted._2).abs > 1)(
      s"size_mismatch: window is ${pixels._1}x${pixels._2}px, ${wanted._1}x${wanted._2}px was asked for; " +
        s"float it with `bench/hyprland-float.sh on ${requestedLogical._1} ${requestedLogical._2}` before the run"
    )

  private val WarmupFrames   = 20
  private val TypedFrames    = 60
  private val ColdFrames     = 30
  private val SettleAttempts = 100
  private val SettlePauseMs  = 20L
  private val SwapIntervals  = List(1, 0)
  private val FinishSettings = List(false, true)

  /** `glFinish`, found through SDL on the SDL thread and called on it. */
  private def glFinish(window: SdlGlWindow): IO[Option[() => Unit]] =
    window.onSdlThread { sdl =>
      sdl.glProcAddress("glFinish").map { address =>
        val handle = Linker.nativeLinker().downcallHandle(address, FunctionDescriptor.ofVoid())
        () => handle.invokeExact(): Unit
      }
    }

  /** The window's pixel size and scale once SDL has told it, as the logical size the renderer lays out for. */
  private def settledLogicalSize(window: SdlGlWindow): IO[(Int, Int, Double)] =
    def settle(attempts: Int): IO[Unit] =
      window.takeResize.flatMap {
        case Some(_) => IO.unit
        case None =>
          if attempts <= 0 then IO.unit
          else IO.sleep(SettlePauseMs.millis) *> settle(attempts - 1)
      }
    for
      _      <- settle(SettleAttempts)
      pixels <- window.pixelSize
      scale  <- window.deviceScale
    yield (math.round(pixels._1 / scale).toInt, math.round(pixels._2 / scale).toInt, scale)

  private def cells(
    frames: SdlPresentedFrames,
    renderer: SkiaGlFrameBenchmarks.Frames,
    before: AppState,
    after: AppState
  ): List[(String, Vector[FrameStamps])] =
    val forward  = DamageProducer.forTransition(before, after)
    val backward = DamageProducer.forTransition(after, before)
    val steps    = Iterator.continually(List(after -> forward, before -> backward)).flatten
    renderer.render(before, Damage.Everything)
    List(
      "typing" -> frames.measure(WarmupFrames, TypedFrames) { () =>
        val (state, damage) = steps.next()
        renderer.render(state, damage)
      },
      "idle" -> frames.measure(WarmupFrames, TypedFrames)(() => renderer.render(after, Damage.Nothing)),
      "cold" -> frames.measure(2, ColdFrames)(() => renderer.render(after, Damage.Everything))
    )

  private def runFinishSetting(
    window: SdlGlWindow,
    finish: Option[() => Unit],
    settings: RenderCaches => GlFrameSettings,
    metrics: CellMetrics,
    grid: ViewportSize,
    before: AppState,
    after: AppState
  ): IO[List[String]] =
    val caches = RenderCaches.create()
    SdlPresentedFrames.open(window, settings(caches), finish).use {
      case Left(error) => IO.raiseError(new IllegalStateException(s"no presenter: ${error.describe}"))
      case Right(frames) =>
        val renderer = SkiaGlFrameBenchmarks.Renderer(metrics, grid, render => frames.frame(render), caches)
        SwapIntervals.flatTraverse { interval =>
          SdlPresentedFrames.setSwapInterval(window, interval).map {
            case Left(error) => List(s"swap interval $interval refused: ${error.describe}")
            case Right(()) =>
              cells(frames, renderer, before, after).map { (transition, stamps) =>
                val label = s"swap_interval=$interval finish=${finish.isDefined} $transition"
                val note  = if stamps.forall(_.isOrdered) then "" else "  [stamps out of order]"
                PresentRows.of(stamps).describe(label) + note
              }
          }
        }
    }

  def main(args: Array[String]): Unit =
    BenchmarkMain.run:
      import cats.effect.unsafe.implicits.global
      val sdl = Sdl.load(sys.props.toMap, java.nio.file.Files.exists(_)) match
        case Right(loaded) => loaded
        case Left(error)   => sys.error(s"libSDL3 did not load: ${error.describe}")
      val driver    = sys.props.getOrElse("serenity.sdl3.driver", "offscreen")
      val requested = requestedSize(sys.props.toMap)
      val lines = SdlGlWindow
        .open(
          sdl,
          SdlGlWindow.Settings(WindowTitle, requested._1, requested._2, Some(driver))
        )
        .use {
          case Left(error)   => IO.raiseError(new IllegalStateException(s"no GL window: ${error.describe}"))
          case Right(window) => report(window, driver, requested)
        }
        .unsafeRunSync()
      lines.foreach(println)

  private def report(window: SdlGlWindow, driver: String, requested: (Int, Int))(using IORuntime): IO[List[String]] =
    for
      size       <- settledLogicalSize(window)
      renderer   <- window.renderer
      pixels     <- window.pixelSize
      finishCall <- glFinish(window)
      (logicalW, logicalH, scale) = size
      metrics                     = TestMeasure.cellMetrics(codeFont)
      grid                        = ViewportSize(logicalW / metrics.charWidth, logicalH / metrics.lineHeight)
      (before, after)             = TypingTransition.of(LaptopFrameBenchmarks.proseStateManager(grid = grid)._1)
      settings = (caches: RenderCaches) =>
        GlFrameSettings(
          metrics,
          FontSpec.fromAwt(codeFont),
          logicalW,
          logicalH,
          scale,
          caches.surfaceText,
          caches.backendCaches,
          ScreenIdentity(new Object)
        )
      header = List(
        s"skia_gl_present: driver=$driver renderer=$renderer window=${pixels._1}x${pixels._2}px scale=$scale " +
          s"vsync_at_open=${window.vsync} glFinish=${finishCall.isDefined}"
      ) ++ sizeNote(requested, pixels, scale).toList
      rows <- FinishSettings.flatTraverse { finish =>
        if finish && finishCall.isEmpty then IO.pure(List("finish=true skipped: glFinish not found"))
        else runFinishSetting(window, Option.when(finish)(finishCall).flatten, settings, metrics, grid, before, after)
      }
    yield header ++ rows
