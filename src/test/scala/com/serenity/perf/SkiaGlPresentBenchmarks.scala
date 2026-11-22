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
  * compositor gives it the tile's size. `-Dserenity.sdl3.window=fullscreen` asks SDL for the whole display instead,
  * which needs no compositor rule (the call is made from the test, so the SDL binding is unchanged); the header then
  * says what the window and the display came to. Fullscreen may take a compositor's direct path to the screen, which a
  * windowed run does not, so set its `swap` rows beside a windowed run's with that in mind. The window's title is the
  * one `bench/hyprland-float.sh` floats, as the other way to keep a size, and the header says when the window has other
  * pixels than were asked for.
  */
private[perf] object SkiaGlPresentBenchmarks:

  given Balance = Balance.default

  private val codeFont = LaptopFrameBenchmarks.codeFont

  private val WarmupFrames       = 20
  private val TypedFrames        = 60
  private val ColdFrames         = 30
  private val SettleAttempts     = 100
  private val SettlePauseMs      = 20L
  private val SettleSteady       = 10
  private val FullscreenAttempts = 250
  private val SwapIntervals      = List(1, 0)
  private val FinishSettings     = List(false, true)

  /** `glFinish`, found through SDL on the SDL thread and called on it. */
  private def glFinish(window: SdlGlWindow): IO[Option[() => Unit]] =
    window.onSdlThread { sdl =>
      sdl.glProcAddress("glFinish").map { address =>
        val handle = Linker.nativeLinker().downcallHandle(address, FunctionDescriptor.ofVoid())
        () => handle.invokeExact(): Unit
      }
    }

  /** The window's pixel size and scale once SDL has told it, as the logical size the renderer lays out for. */
  private[perf] def settledLogicalSize(window: SdlGlWindow): IO[(Int, Int, Double)] =
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

  /** The size the window is expected to come to once it has been asked for, what to say if it did not, and what was
    * found out on the way.
    */
  final private[perf] case class Expectation(logical: (Int, Int), fix: String, notes: List[String])

  private[perf] def arrange(window: SdlGlWindow, request: WindowRequest): IO[Expectation] =
    request match
      case WindowRequest.Logical(width, height) =>
        IO.pure(
          Expectation(
            (width, height),
            s"float it with `bench/hyprland-float.sh on $width $height` before the run, or ask for fullscreen",
            Nil
          )
        )
      case WindowRequest.Fullscreen => fullscreen(window)

  private def fullscreen(window: SdlGlWindow): IO[Expectation] =
    val fallback = (WindowRequest.default.width, WindowRequest.default.height)
    SdlPresentWindow.FullscreenCalls.load(sys.props.toMap) match
      case Left(reason) =>
        IO.pure(Expectation(fallback, "fullscreen was not available", List(s"fullscreen_unavailable: $reason")))
      case Right(calls) =>
        for
          before  <- window.pixelSize
          entered <- window.onSdlThread(_ => calls.enter(window.handle))
          pixels <- SdlPresentWindow.settledPixels(
            window,
            SettleSteady,
            FullscreenAttempts,
            SettlePauseMs.millis,
            awayFrom = Some(before)
          )
          display <- window.onSdlThread(_ => calls.displaySize(window.handle))
          scale   <- window.deviceScale
        yield
          val logical = display.fold(fallback)(shown => (shown.logicalWidth, shown.logicalHeight))
          val refused = Option.unless(entered)("fullscreen_refused: SDL_SetWindowFullscreen returned false")
          val shown =
            s"fullscreen: window ${pixels._1}x${pixels._2}px, display ${logical._1}x${logical._2} logical at scale $scale"
          Expectation(
            logical,
            "fullscreen did not give the whole display (a bar or reserved area, or the compositor kept it windowed)",
            refused.toList :+ shown
          )

  def main(args: Array[String]): Unit =
    BenchmarkMain.run:
      import cats.effect.unsafe.implicits.global
      val sdl = Sdl.load(sys.props.toMap, java.nio.file.Files.exists(_)) match
        case Right(loaded) => loaded
        case Left(error)   => sys.error(s"libSDL3 did not load: ${error.describe}")
      val driver  = sys.props.getOrElse("serenity.sdl3.driver", "offscreen")
      val request = WindowRequest.parse(sys.props.toMap)
      val asked = request match
        case WindowRequest.Logical(width, height) => (width, height)
        case WindowRequest.Fullscreen             => (WindowRequest.default.width, WindowRequest.default.height)
      val lines = SdlGlWindow
        .open(sdl, SdlGlWindow.Settings(SdlPresentWindow.Title, asked._1, asked._2, Some(driver)))
        .use {
          case Left(error)   => IO.raiseError(new IllegalStateException(s"no GL window: ${error.describe}"))
          case Right(window) => report(window, driver, request)
        }
        .unsafeRunSync()
      lines.foreach(println)

  private def report(window: SdlGlWindow, driver: String, request: WindowRequest)(using IORuntime): IO[List[String]] =
    for
      expected   <- arrange(window, request)
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
      ) ++ expected.notes ++
        SdlPresentWindow.sizeNote(expected.logical, pixels, scale, expected.fix).toList
      rows <- FinishSettings.flatTraverse { finish =>
        if finish && finishCall.isEmpty then IO.pure(List("finish=true skipped: glFinish not found"))
        else runFinishSetting(window, Option.when(finish)(finishCall).flatten, settings, metrics, grid, before, after)
      }
    yield header ++ rows
