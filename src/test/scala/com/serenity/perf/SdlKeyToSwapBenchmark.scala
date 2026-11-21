package com.serenity.perf

import scala.concurrent.duration.*

import cats.effect.unsafe.IORuntime
import cats.effect.{IO, Ref, Resource}
import cats.syntax.all.*
import com.serenity.app.{AppRuntime, AppRuntimeRenderLoops}
import com.serenity.config.AppConfig
import com.serenity.diagnostics.{FrameTimings, LatencyWindow, PhaseStats}
import com.serenity.input.{InputRouter, PendingInput, SystemClipboard}
import com.serenity.keystroke.events.Event
import com.serenity.keystroke.translators.TextEntryTranslator
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.models.{AppState, Damage}
import com.serenity.testkit.TestMeasure
import com.serenity.ui.layout.{CellMetrics, ViewportSize}
import com.serenity.ui.renderer.{FontSpec, ScreenIdentity}
import com.serenity.ui.sdl.{Sdl, SdlGlWindow, SdlKeyTranslation}
import com.serenity.ui.skia.GlFrameSettings
import fs2.concurrent.SignallingRef
import fs2.{Chunk, Stream}
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.noop.NoOpLogger

/** What a typed key costs from SDL's event to the swap that shows it, through the application's own input and render
  * loops (#2380, Stage A3). A key SDL reports goes through [[SdlKeyTranslation]] into `inputBatchPhase` over a real
  * `StateManager`, its damage wakes `fastRenderPhase` (the app's pacing loop), the frame is drawn and presented on a
  * real GL window, and the app's own key-latency trace stamps the key at each step.
  *
  * Every reading is put on SDL's clock (`SDL_GetTicksNS`), so `key_to_swap` is the swap's return minus the event's own
  * timestamp and includes the hop from the compositor to the pump and the pump's poll sleep. `event_pickup` is that hop
  * alone. `draw` is the draw's own time, kept apart so the renderer's tail is never credited to or blamed on SDL3. The
  * rest are the trace's stages (`frame_wait`, `pacing_wait`, `render` and so on) over the same keys.
  *
  * With the in-process injector a key is stamped when the push runs on the SDL thread, behind whatever draw or swap
  * that thread is in, so `event_pickup` is then the pump's wait on that thread and not the compositor's hop; only a
  * compositor run measures the hop. The trace's `render_layout`, `render_draw` and `render_flush` are not reported (see
  * [[UnmeasuredSplits]]).
  *
  * The idle phase of the app's loop is reduced to waiting for the fast-mode signal, which is what ends it in the app;
  * it has no caret blink or window focus, and the caret is not drawn (as in [[SkiaGlPresentBenchmarks]]). Not part of
  * the perf gate.
  *
  * Run it like [[SkiaGlPresentBenchmarks]], with the same `-D` window options, plus:
  *   - `serenity.sdl3.keys` (200), `serenity.sdl3.keys.warmup` (40) and `serenity.sdl3.keys.spacing` (5, milliseconds)
  *   - `serenity.sdl3.keys.driver`: `inprocess` (default; keys pushed into SDL's queue, no compositor hop) or
  *     `compositor` (the benchmark prints that it is waiting, and the keys are typed into the window from outside, for
  *     example with `wtype`)
  *   - `serenity.sdl3.swap`: the swap intervals to run, in order (default `1,0`; a compositor run types once per
  *     interval, so run it with one at a time)
  */
private[perf] object SdlKeyToSwapBenchmark:

  given Balance    = Balance.default
  given Logger[IO] = NoOpLogger[IO]

  private val ClockReadings = 50
  private val PaintGrace    = 10.seconds

  /** The trace's render splits come from the render-phase clock, which only the production overlay path feeds. This
    * benchmark draws through the frame planner, as [[SkiaGlPresentBenchmarks]] does so that its `draw` row stays
    * comparable with job 13, so these would read 0 and look like data.
    */
  private val UnmeasuredSplits = Set("render_layout", "render_draw", "render_flush")

  enum Injector(val label: String):
    case InProcess  extends Injector("inprocess")
    case Compositor extends Injector("compositor")

  final case class Options(
      keys: Int,
      warmupKeys: Int,
      spacing: FiniteDuration,
      injector: Injector,
      swapIntervals: List[Int]
  )

  object Options:

    val default: Options =
      Options(
        keys = 200,
        warmupKeys = 40,
        spacing = 5.millis,
        injector = Injector.InProcess,
        swapIntervals = List(1, 0)
      )

    def parse(properties: Map[String, String]): Either[String, Options] =
      for
        keys     <- number(properties, "serenity.sdl3.keys", default.keys, minimum = 1)
        warmup   <- number(properties, "serenity.sdl3.keys.warmup", default.warmupKeys, minimum = 0)
        spacing  <- number(properties, "serenity.sdl3.keys.spacing", default.spacing.toMillis.toInt, minimum = 1)
        injector <- injectorOf(properties.get("serenity.sdl3.keys.driver"))
        swaps    <- swapIntervalsOf(properties.get("serenity.sdl3.swap"))
      yield Options(keys, warmup, spacing.millis, injector, swaps)

    private def number(
      properties: Map[String, String],
      name: String,
      fallback: Int,
      minimum: Int
    ): Either[String, Int] =
      properties.get(name).fold(Right(fallback)) { raw =>
        raw.trim.toIntOption
          .filter(_ >= minimum)
          .toRight(s"$name must be a whole number of at least $minimum, not '$raw'")
      }

    private def injectorOf(raw: Option[String]): Either[String, Injector] =
      raw.map(_.trim.toLowerCase).fold[Either[String, Injector]](Right(default.injector)) {
        case "inprocess"  => Right(Injector.InProcess)
        case "compositor" => Right(Injector.Compositor)
        case other        => Left(s"serenity.sdl3.keys.driver must be inprocess or compositor, not '$other'")
      }

    private def swapIntervalsOf(raw: Option[String]): Either[String, List[Int]] =
      raw.fold[Either[String, List[Int]]](Right(default.swapIntervals)) {
        _.split(",", -1).toList.map(_.trim).traverse {
          case "0"   => Right(0)
          case "1"   => Right(1)
          case other => Left(s"serenity.sdl3.swap takes 0 or 1, each separated by a comma, not '$other'")
        }
      }

  /** The window's logical size and scale, as the renderer lays out for them. */
  final case class Geometry(logicalWidth: Int, logicalHeight: Int, scale: Double)

  /** What SDL stamped a key event with and when the pump read it, both on SDL's clock. */
  final private case class KeyArrival(event: Long, pickup: Long)

  final private[perf] case class Outcome(
      swapInterval: Int,
      renderFpsTarget: String,
      sent: Int,
      arrived: Int,
      keys: Vector[KeySwapStamps],
      frames: Vector[FrameStamps],
      stages: List[(String, PhaseStats)],
      unpainted: Int
  ):

    def lines: List[String] =
      val context = s"swap_interval=$swapInterval"
      val drawn   = PresentRows.of(frames)
      val summary =
        s"[sdl3-key] $context keys sent=$sent shown=${keys.size} unpainted=$unpainted frames=${frames.size} " +
          s"render_fps_target=$renderFpsTarget"
      summary :: KeySwapRows.of(keys).lines(context) ++
        List(
          KeySwapRows.line(context, "draw", drawn.draw.count, drawn.draw.p50Ms, drawn.draw.p95Ms, drawn.draw.maxMs)
        ) ++
        stages
          .filterNot((label, _) => UnmeasuredSplits.contains(label))
          .map((label, stats) => KeySwapRows.line(context, label, stats.count, stats.p50Ms, stats.p95Ms, stats.maxMs))

  /** One run at `interval`: `warmupKeys` keys that are not reported, then `keys` that are. */
  def trial(window: SdlGlWindow, geometry: Geometry, interval: Int, options: Options)(using
    IORuntime
  ): IO[Outcome] =
    val metrics = TestMeasure.cellMetrics(LaptopFrameBenchmarks.codeFont)
    val grid    = ViewportSize(geometry.logicalWidth / metrics.charWidth, geometry.logicalHeight / metrics.lineHeight)
    for
      stateManager <- IO(LaptopFrameBenchmarks.proseStateManager(grid = grid)._1)
      frameTimings = FrameTimings()
      _ <- IO(frameTimings.keyLatency.setEnabled(true))
      caches = stateManager.renderCaches
      settings = GlFrameSettings(
        metrics,
        FontSpec.fromAwt(LaptopFrameBenchmarks.codeFont),
        geometry.logicalWidth,
        geometry.logicalHeight,
        geometry.scale,
        caches.surfaceText,
        caches.backendCaches,
        ScreenIdentity(new Object)
      )
      outcome <- SdlPresentedFrames.open(window, settings, None, Some(frameTimings)).use {
        case Left(error) => IO.raiseError(new IllegalStateException(s"no presenter: ${error.describe}"))
        case Right(frames) =>
          run(window, frames, stateManager, metrics, grid, frameTimings, interval, options)
      }
    yield outcome

  /** The app's render function, drawing the model's frame on the presented window; the caret is left out, as in
    * [[SkiaGlPresentBenchmarks]].
    */
  private def renderFunction(renderer: SkiaGlFrameBenchmarks.Renderer): AppRuntime.RenderFn =
    (state, _, _, damage, _) => IO.blocking(renderer.render(state, damage))

  private def orRaise[A](result: Either[com.serenity.ui.sdl.SdlError, A]): IO[A] =
    result.fold(error => IO.raiseError(new IllegalStateException(error.describe)), IO.pure)

  private def run(
    window: SdlGlWindow,
    frames: SdlPresentedFrames,
    stateManager: StateManager,
    metrics: CellMetrics,
    grid: ViewportSize,
    frameTimings: FrameTimings,
    interval: Int,
    options: Options
  ): IO[Outcome] =
    val total = options.warmupKeys + options.keys
    for
      _             <- SdlPresentedFrames.setSwapInterval(window, interval).flatMap(orRaise)
      offset        <- clockOffset(window)
      arrivals      <- Ref.of[IO, Vector[KeyArrival]](Vector.empty)
      fastMode      <- SignallingRef.of[IO, Boolean](false)
      pending       <- Ref.of[IO, Damage](Damage.Nothing)
      pendingPaint  <- Ref.of[IO, Damage](Damage.Nothing)
      cursorVisible <- Ref.of[IO, Boolean](true)
      translators   <- Ref.of[IO, Option[AppRuntimeRenderLoops.FocusedTranslatorCacheEntry]](None)
      lastStart     <- Ref.of[IO, Option[FiniteDuration]](None)
      published     <- Ref.of[IO, Option[AppState]](None)
      router        <- InputRouter.create[IO, Event](new TextEntryTranslator(AppConfig.conventional))
      initial       <- stateManager.getCurrentState
      lastDrawn     <- Ref.of[IO, Option[AppState]](Some(initial))
      renderer = SkiaGlFrameBenchmarks.Renderer(
        metrics,
        grid,
        render => frames.frame(render),
        stateManager.renderCaches
      )
      emitDamage = (damage: Damage) =>
        pending.update(_ |+| damage) >> pendingPaint.update(_ |+| damage) >> fastMode.set(true)
      context = AppRuntimeRenderLoops.InputBatchContext(
        stateManager,
        router,
        SystemClipboard[IO](readText = IO.pure(None), writeText = _ => IO.unit),
        IO.unit,
        cursorVisible,
        emitDamage,
        translators,
        frameTimings,
        damagePublishedFor = state => published.set(Some(state))
      )
      fastPhase = AppRuntimeRenderLoops.fastRenderPhase(
        stateManager,
        fastMode,
        pending,
        pendingPaint,
        stateManager.getCurrentState.map(Some(_)),
        IO.unit,
        renderFunction(renderer),
        stateManager.renderCaches,
        lastFrameStart = lastStart,
        frameTimings = frameTimings,
        lastDrawnState = lastDrawn,
        damagePublishedFor = published,
        cursorVisible = cursorVisible.get
      )
      idlePhase = Stream.eval(fastMode.waitUntil(identity))
      _ <- IO.blocking(renderer.render(initial, Damage.Everything))
      _ <- IO(frames.takeStamps())
      inputLoop = keystrokeBatches(window, frameTimings, offset, arrivals).through(
        AppRuntimeRenderLoops.inputBatchPhase(context)
      )
      renderLoop = AppRuntimeRenderLoops.renderLoop(idlePhase, fastPhase)
      latency <- (
        Resource.make(inputLoop.compile.drain.start)(_.cancel),
        Resource.make(renderLoop.compile.drain.start)(_.cancel)
      ).tupled
        .use { _ =>
          inject(window, total, options) >> awaitArrivals(arrivals, total, options) >>
            collectPainted(IO(frameTimings.keyLatency.drain()), total + 1, PaintGrace, 10.millis, flush(window))
        }
      seen      <- arrivals.get
      allFrames <- IO(frames.takeStamps())
    yield
      val painted = latency.keys.sortBy(_.seq)
      val stamps = painted.zip(seen).map { (key, arrival) =>
        KeySwapStamps(arrival.event, arrival.pickup, arrival.pickup + key.totalNanos)
      }
      val lastWarmup = stamps.take(options.warmupKeys).lastOption.map(_.shown + offset)
      Outcome(
        swapInterval = interval,
        renderFpsTarget = initial.persisted.config.surfaceConfig.renderFpsTarget.framesPerSecond.toString,
        sent = measuredKeys(seen, options.warmupKeys, options.keys).size,
        arrived = seen.size,
        keys = measuredKeys(stamps, options.warmupKeys, options.keys),
        frames = allFrames.filter(frame => lastWarmup.forall(frame.requested > _)),
        stages = latency.copy(keys = measuredKeys(painted, options.warmupKeys, options.keys)).stats,
        unpainted = latency.unpainted
      )

  /** `System.nanoTime` minus SDL's clock, from the closest pair of reads: a delay between the two reads only widens the
    * gap, so the narrowest of several gives the offset with the least error.
    */
  private def clockOffset(window: SdlGlWindow): IO[Long] =
    window.onSdlThread { sdl =>
      val readings = Vector.fill(ClockReadings) {
        val before = System.nanoTime()
        val ticks  = sdl.ticksNanos
        val after  = System.nanoTime()
        (after - before, (before + after) / 2 - ticks)
      }
      readings.minBy(_._1)._2
    }

  /** SDL's key and text events as the input loop's batches: whatever is queued when the loop asks, translated, with
    * each key stamped into the app's latency trace and its two clock readings kept.
    */
  private def keystrokeBatches(
    window: SdlGlWindow,
    frameTimings: FrameTimings,
    offset: Long,
    arrivals: Ref[IO, Vector[KeyArrival]]
  ): Stream[IO, Chunk[PendingInput]] =
    val trace = frameTimings.keyLatency
    window.events.chunks
      .evalMap { events =>
        IO {
          val pickup = System.nanoTime() - offset
          val typed = events.toList.flatMap(stamped =>
            SdlKeyTranslation.keystrokes(stamped.event).map(info => (stamped.timestampNanos, info))
          )
          typed.foreach { (eventNanos, _) =>
            frameTimings.inputArrived()
            trace.keyReceived(System.currentTimeMillis() - (pickup - eventNanos) / 1_000_000L)
            trace.keyEnqueued()
          }
          if typed.nonEmpty then trace.keysDequeued(typed.size)
          (typed, pickup)
        }.flatMap { (typed, pickup) =>
          arrivals
            .update(_ ++ typed.map((eventNanos, _) => KeyArrival(eventNanos, pickup)))
            .as(Chunk.from(typed.map((_, info) => PendingInput.Keystroke(info): PendingInput)))
        }
      }
      .filter(_.nonEmpty)

  private def inject(window: SdlGlWindow, count: Int, options: Options): IO[Unit] =
    options.injector match
      case Injector.Compositor =>
        IO.println(s"[sdl3-key] waiting for $count typed characters in the window (warm-up included)")
      case Injector.InProcess =>
        val letters = TypingBurst.letters
        List.fill(count)(()).traverse_ { _ =>
          IO(letters.next()).flatMap(char => window.onSdlThread(_.pushText(char.toString))).flatMap(orRaise) >>
            IO.sleep(options.spacing)
        }

  /** The keys a run reports: after the warm-up, no more than `keys`, so the flush key is left out. */
  private[perf] def measuredKeys[A](all: Vector[A], warmup: Int, keys: Int): Vector[A] =
    all.slice(warmup, warmup + keys)

  /** One more key, pushed in process after the run. The trace credits a key whose damage lands while a frame is being
    * made to the next frame, and for the last key of a run no other key follows to wake one; without this it would be
    * counted as sent and never as shown.
    */
  private def flush(window: SdlGlWindow): IO[Unit] =
    window.onSdlThread(_.pushText("x")).flatMap(orRaise)

  /** Until `expected` keys have arrived, or `options` says to stop waiting. */
  private def awaitArrivals(arrivals: Ref[IO, Vector[KeyArrival]], expected: Int, options: Options): IO[Unit] =
    val within = options.injector match
      case Injector.InProcess  => 30.seconds + options.spacing * expected.toLong
      case Injector.Compositor => 5.minutes
    def poll: IO[Unit] = arrivals.get.map(_.size >= expected).ifM(IO.unit, IO.sleep(10.millis) >> poll)
    poll.timeoutTo(within, IO.unit)

  /** Runs `flush` once, then drains the key-latency trace until `expected` keys are accounted for, each painted or
    * counted unpainted, or `within` has passed, and reports what it has. A key a frame has taken is no longer pending
    * but is not painted until that frame's paint finishes, so "nothing pending" is not the end: only a drain shows what
    * has been painted.
    */
  private[perf] def collectPainted(
    drain: IO[LatencyWindow],
    expected: Int,
    within: FiniteDuration,
    pause: FiniteDuration,
    flush: IO[Unit]
  ): IO[LatencyWindow] =
    def gather(collected: LatencyWindow, deadline: FiniteDuration): IO[LatencyWindow] =
      drain.flatMap { window =>
        val total = collected.copy(
          keys = collected.keys ++ window.keys,
          unpainted = collected.unpainted + window.unpainted,
          backend = window.backend
        )
        if total.keys.size + total.unpainted >= expected then IO.pure(total)
        else
          IO.monotonic.flatMap { now =>
            if now >= deadline then IO.pure(total) else IO.sleep(pause) >> gather(total, deadline)
          }
      }
    flush >> IO.monotonic.flatMap(start => gather(LatencyWindow(0.0, Vector.empty, 0), start + within))

  def main(args: Array[String]): Unit =
    BenchmarkMain.run:
      import cats.effect.unsafe.implicits.global
      val properties = sys.props.toMap
      val options    = Options.parse(properties).fold(message => sys.error(message), identity)
      val sdl = Sdl.load(properties, java.nio.file.Files.exists(_)) match
        case Right(loaded) => loaded
        case Left(error)   => sys.error(s"libSDL3 did not load: ${error.describe}")
      val driver  = properties.getOrElse("serenity.sdl3.driver", "offscreen")
      val request = WindowRequest.parse(properties)
      val asked = request match
        case WindowRequest.Logical(width, height) => (width, height)
        case WindowRequest.Fullscreen             => (WindowRequest.default.width, WindowRequest.default.height)
      val lines = SdlGlWindow
        .open(sdl, SdlGlWindow.Settings(SdlPresentWindow.Title, asked._1, asked._2, Some(driver)))
        .use {
          case Left(error)   => IO.raiseError(new IllegalStateException(s"no GL window: ${error.describe}"))
          case Right(window) => report(window, driver, request, options)
        }
        .unsafeRunSync()
      lines.foreach(println)

  /** What a reader of the rows needs to know about how the keys were made and counted. */
  private[perf] def headerNotes(options: Options): List[String] =
    List(
      "[sdl3-key] note: render_layout, render_draw and render_flush are not reported; this path draws through the " +
        "frame planner, which does not feed the render-phase clock they come from"
    ) ++ Option
      .when(options.injector == Injector.InProcess)(
        "[sdl3-key] note: inprocess keys are stamped on the SDL thread, so event_pickup leaves out the compositor hop"
      )
      .toList ++ List(
      "[sdl3-key] note: one more key is pushed in process after the last, to flush it, and is left out of the rows; " +
        "the trace credits a key whose damage lands mid-frame to the next frame, so the last measured key always has " +
        "a follower and its key_to_swap can read one frame longer than it would without (only the max is likely to show it)"
    )

  private def report(window: SdlGlWindow, driver: String, request: WindowRequest, options: Options)(using
    IORuntime
  ): IO[List[String]] =
    for
      expected <- SkiaGlPresentBenchmarks.arrange(window, request)
      size     <- SkiaGlPresentBenchmarks.settledLogicalSize(window)
      renderer <- window.renderer
      pixels   <- window.pixelSize
      geometry = Geometry(size._1, size._2, size._3)
      header = List(
        s"[sdl3-key] header driver=$driver renderer=$renderer window=${pixels._1}x${pixels._2}px scale=${size._3} " +
          s"injector=${options.injector.label} keys=${options.keys} warmup=${options.warmupKeys} " +
          s"spacing_ms=${options.spacing.toMillis} vsync_at_open=${window.vsync}"
      ) ++ headerNotes(options) ++ expected.notes.map(note => s"[sdl3-key] $note") ++
        SdlPresentWindow
          .sizeNote(expected.logical, pixels, size._3, expected.fix)
          .toList
          .map(note => s"[sdl3-key] $note")
      rows <- options.swapIntervals.flatTraverse { interval =>
        trial(window, geometry, interval, options).attempt.map {
          case Right(outcome) => outcome.lines
          case Left(failure)  => List(s"[sdl3-key] swap_interval=$interval failed: ${failure.getMessage}")
        }
      }
    yield header ++ rows
