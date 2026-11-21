package com.serenity.diagnostics

import java.util.Locale
import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}

import cats.effect.IO

/** Where a frame's time goes, in pipeline order: an input waiting for the input loop, being applied to the state,
  * waiting for the render loop, the frontend syncing window state, drawing, waiting for the toolkit to paint, and the
  * paint itself. `InputToPaint` spans the whole journey, from an input's arrival to the end of the first paint showing
  * it.
  */
enum FramePhase(val label: String):
  case InputQueue extends FramePhase("input-queue")
  case InputApply extends FramePhase("input-apply")

  /** The stages an input's application splits into, one sample per input or event, not per frame: reading the input as
    * an event, stepping a typed key, applying any other event (its reducing and committing together), committing a run
    * of typed keys, and publishing the damage a dispatch leaves.
    */
  case ApplyDecode      extends FramePhase("apply-decode")
  case ApplyTypedStep   extends FramePhase("apply-typed-step")
  case ApplyEvent       extends FramePhase("apply-event")
  case ApplyTypedCommit extends FramePhase("apply-typed-commit")
  case ApplyDamage      extends FramePhase("apply-damage")
  case RenderWait       extends FramePhase("render-wait")
  case Sync             extends FramePhase("sync")

  /** The whole frame on the render thread, `Sync` included. */
  case Render extends FramePhase("render")

  /** What a frame's render splits into: building the scene (layout), drawing it without the flush, and flushing the
    * surface. The first two are the time inside each, so they do not add up to `Render`, which also holds the rest.
    */
  case RenderLayout extends FramePhase("render-layout")
  case RenderDraw   extends FramePhase("render-draw")
  case RenderFlush  extends FramePhase("render-flush")
  case PaintWait    extends FramePhase("paint-wait")
  case Paint        extends FramePhase("paint")
  case InputToPaint extends FramePhase("input-to-paint")

enum FrameKind(val label: String):
  case Full       extends FrameKind("full")
  case CursorOnly extends FrameKind("cursor")

final case class PhaseStats(count: Int, p50Ms: Double, p95Ms: Double, maxMs: Double)

object PhaseStats:
  private val NanosPerMs = 1_000_000.0

  def of(durationsNanos: Vector[Long]): PhaseStats =
    val sorted = durationsNanos.sorted
    def nearestRank(percentile: Double): Double =
      val rank = math.ceil(percentile * sorted.size).toInt.max(1) - 1
      sorted.lift(rank).fold(0.0)(_ / NanosPerMs)
    PhaseStats(sorted.size, nearestRank(0.50), nearestRank(0.95), sorted.lastOption.fold(0.0)(_ / NanosPerMs))

final case class FrameTimingSummary(
    elapsedMs: Double,
    frames: Map[FrameKind, Int],
    phases: Map[FramePhase, PhaseStats],
    counters: FrameCounterStats = FrameCounterStats.empty,
    backend: String = "",
    edtLag: Option[PhaseStats] = None,
    paints: Map[PaintSource, Int] = Map.empty
):

  def withCounters(counted: FrameCounterStats): FrameTimingSummary = copy(counters = counted)

  def withEdtLag(lag: PhaseStats): FrameTimingSummary = copy(edtLag = Option.when(lag.count > 0)(lag))

  def logLine: String =
    val seconds = elapsedMs / 1000.0
    val frameRates = FrameKind.values.toList.map { kind =>
      val count = frames.getOrElse(kind, 0)
      val rate  = if seconds > 0 then count / seconds else 0.0
      String.format(Locale.ROOT, "%s=%d (%.1f/s)", kind.label, count, rate)
    }
    val phaseParts = FramePhase.values.toList.flatMap { phase =>
      phases.get(phase).map { stats =>
        String.format(
          Locale.ROOT,
          "%s p50=%.2f p95=%.2f max=%.2f n=%d ms",
          phase.label,
          stats.p50Ms,
          stats.p95Ms,
          stats.maxMs,
          stats.count
        )
      }
    }
    val window = String.format(Locale.ROOT, "%.1f", seconds)
    val lagPart = edtLag.map { stats =>
      String.format(
        Locale.ROOT,
        "edt-lag p50=%.2f p95=%.2f max=%.2f n=%d ms",
        stats.p50Ms,
        stats.p95Ms,
        stats.maxMs,
        stats.count
      )
    }
    val paintsPart = Option(PaintSource.describe(paints)).filter(_.nonEmpty).map(described => s"paints $described")
    val parts      = phaseParts ++ lagPart.toList ++ counters.logParts ++ paintsPart.toList
    s"[FRAME]${PresentPoint.tag(backend)} window=${window}s ${frameRates.mkString(" ")} | ${if parts.isEmpty then "no samples" else parts.mkString(" | ")}"

/** The timings gathered since the last summary, plus the in-flight marks that tie one input to the frame showing it.
  *
  * Inputs are tracked by the earliest one not yet carried forward rather than one by one: when several land before a
  * frame, the oldest is the one whose latency the user feels.
  */
final case class FrameTimingState(
    windowStartNanos: Long,
    samples: Map[FramePhase, Vector[Long]],
    frames: Map[FrameKind, Int],
    arrivedAt: Option[Long],
    applying: Option[(Long, Long)],
    awaitingRender: Option[(Long, Long)],
    renderStartedAt: Option[Long],
    rendering: Option[Long],
    awaitingPaint: Option[Long],
    publishedAt: Option[Long],
    painting: Option[Long],
    paints: Map[PaintSource, Int] = Map.empty
):
  import FrameTimingState.MaxSamplesPerPhase

  def record(phase: FramePhase, durationNanos: Long): FrameTimingState =
    val kept = (samples.getOrElse(phase, Vector.empty) :+ durationNanos.max(0L)).takeRight(MaxSamplesPerPhase)
    copy(samples = samples.updated(phase, kept))

  def inputArrived(now: Long): FrameTimingState = copy(arrivedAt = arrivedAt.orElse(Some(now)))

  /** An input reaching the input loop with no recorded arrival (one a frontend didn't stamp) counts as arriving now. */
  def inputApplyStarted(now: Long): FrameTimingState =
    val arrival = arrivedAt.getOrElse(now)
    record(FramePhase.InputQueue, now - arrival).copy(arrivedAt = None, applying = Some((arrival, now)))

  def inputApplyFinished(now: Long): FrameTimingState =
    applying.fold(this) {
      case (arrival, startedAt) =>
        record(FramePhase.InputApply, now - startedAt)
          .copy(applying = None, awaitingRender = awaitingRender.orElse(Some((arrival, now))))
    }

  def renderStarted(now: Long): FrameTimingState =
    awaitingRender
      .fold(this) { case (_, appliedAt) => record(FramePhase.RenderWait, now - appliedAt) }
      .copy(
        renderStartedAt = Some(now),
        rendering = rendering.orElse(awaitingRender.map(_._1)),
        awaitingRender = None
      )

  def framePublished(now: Long): FrameTimingState =
    copy(
      publishedAt = publishedAt.orElse(Some(now)),
      awaitingPaint = awaitingPaint.orElse(rendering),
      rendering = None
    )

  def renderFinished(kind: FrameKind, now: Long): FrameTimingState =
    renderStartedAt
      .fold(this)(start => record(FramePhase.Render, now - start))
      .copy(renderStartedAt = None, frames = frames.updated(kind, frames.getOrElse(kind, 0) + 1))

  def paintStarted(now: Long, source: PaintSource): FrameTimingState =
    if !source.shows then this
    else
      publishedAt
        .fold(this)(published => record(FramePhase.PaintWait, now - published))
        .copy(publishedAt = None, painting = painting.orElse(awaitingPaint), awaitingPaint = None)

  def paintFinished(startedAt: Long, source: PaintSource, now: Long): FrameTimingState =
    val counted = copy(paints = paints.updated(source, paints.getOrElse(source, 0) + 1))
    if !source.shows then counted
    else
      val painted = counted.record(FramePhase.Paint, now - startedAt)
      painting
        .fold(painted)(arrival => painted.record(FramePhase.InputToPaint, now - arrival))
        .copy(painting = None)

  def summary(now: Long): FrameTimingSummary =
    FrameTimingSummary(
      (now - windowStartNanos) / 1_000_000.0,
      frames,
      samples.collect { case (phase, durations) if durations.nonEmpty => phase -> PhaseStats.of(durations) },
      paints = paints
    )

  def nextWindow(now: Long): FrameTimingState =
    copy(windowStartNanos = now, samples = Map.empty, frames = Map.empty, paints = Map.empty)

object FrameTimingState:
  /** Several minutes of samples at 60fps, so a summary interval never truncates in practice. */
  val MaxSamplesPerPhase = 4096

  def start(now: Long): FrameTimingState =
    FrameTimingState(now, Map.empty, Map.empty, None, None, None, None, None, None, None, None)

/** Records [[FrameTimingState]] transitions from whichever thread reaches each point: the toolkit's event thread for
  * input arrival and painting, the input loop and the paint executor for the rest. An `AtomicReference` rather than a
  * `Ref` because the event-thread callers are synchronous Swing callbacks with nothing to run an `IO` on.
  *
  * Records nothing until [[setEnabled]] switches it on (`ui.render.frame_timing`), so with timing off every hook is a
  * flag read.
  */
final class FrameTimings(clock: () => Long = () => System.nanoTime(), backend: String = ""):
  private val enabled = new AtomicBoolean(false)
  private val state   = new AtomicReference(FrameTimingState.start(clock()))

  /** Rides along with the frame phases so the frontends that already report them feed it without further wiring. */
  val keyLatency: KeyLatencyTrace = KeyLatencyTrace(clock, backend = backend)

  def isEnabled: Boolean = enabled.get()

  /** Starts from an empty window either way, so nothing half-recorded before a toggle is reported after it. */
  def setEnabled(on: Boolean): Unit =
    state.set(FrameTimingState.start(clock()))
    enabled.set(on)

  private def update(transition: (FrameTimingState, Long) => FrameTimingState): Unit =
    if enabled.get() then
      val now = clock()
      val _   = state.updateAndGet(transition(_, now))

  /** Adds one sample of `nanos` to a stage of the frame, for the stages measured by a clock of their own. */
  def recordPhase(phase: FramePhase, nanos: Long): Unit = update((current, _) => current.record(phase, nanos))

  def inputArrived(): Unit       = update(_.inputArrived(_))
  def inputApplyFinished(): Unit = update(_.inputApplyFinished(_))
  def renderStarted(): Unit      = update(_.renderStarted(_))

  def inputApplyStarted(): Unit =
    update(_.inputApplyStarted(_))
    keyLatency.dispatchStarted()

  def framePublished(): Unit =
    update(_.framePublished(_))
    keyLatency.framePublished()

  def renderFinished(kind: FrameKind): Unit = update(_.renderFinished(kind, _))

  /** Returns the paint's start, to hand back to [[paintFinished]]. */
  def paintStarted(source: PaintSource = PaintSource.Canvas): Long =
    val now = clock()
    if enabled.get() then
      val _ = state.updateAndGet(_.paintStarted(now, source))
    keyLatency.paintStarted(source)
    now

  /** A canvas paint counts as one under the layer while a layer is attached: it shows none of the layer's frames. */
  def canvasPaintStarted(layerAttached: Boolean): CanvasPaint =
    val source = if layerAttached then PaintSource.CanvasUnderLayer else PaintSource.Canvas
    CanvasPaint(paintStarted(source), source)

  final case class CanvasPaint(startedAt: Long, source: PaintSource):
    def finished(): Unit = paintFinished(startedAt, source)

  def paintFinished(startedAt: Long, source: PaintSource = PaintSource.Canvas): Unit =
    update(_.paintFinished(startedAt, source, _))
    keyLatency.paintFinished(source)

  def timed[A](phase: FramePhase)(work: => A): A =
    if measuring then
      val start = clock()
      try work
      finally recordMeasured(phase, clock() - start)
    else work

  def timedIO[A](phase: FramePhase)(work: IO[A]): IO[A] =
    IO(measuring).flatMap { on =>
      if on then IO(clock()).flatMap(start => work.guarantee(IO(recordMeasured(phase, clock() - start))))
      else work
    }

  /** Whether a timed stage has anywhere to be recorded: the frame timings, or the latency trace's stage split. */
  private def measuring: Boolean = enabled.get() || keyLatency.isEnabled

  private def recordMeasured(phase: FramePhase, nanos: Long): Unit =
    update((current, _) => current.record(phase, nanos))
    LatencySplit.ofPhase(phase).foreach(keyLatency.applyStageMeasured(_, nanos))

  /** Syncs window state and draws in a single hop onto the paint thread: each hop is a wait the frame pays for. */
  def timedFrame(kind: FrameKind, paintEc: scala.concurrent.ExecutionContext)(
    sync: IO[Unit],
    draw: IO[Unit]
  ): IO[Unit] =
    IO(renderStarted()) >>
      (timedIO(FramePhase.Sync)(sync) >> draw).evalOn(paintEc).guarantee(IO(renderFinished(kind)))

  /** The summary of everything since the previous drain, starting a new window. */
  def drain(): FrameTimingSummary =
    val now      = clock()
    val previous = state.getAndUpdate(_.nextWindow(now))
    previous.summary(now).copy(backend = backend)
