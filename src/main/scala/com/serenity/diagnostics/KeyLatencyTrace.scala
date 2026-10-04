package com.serenity.diagnostics

import java.util.Locale
import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}

/** One keystroke's journey to the screen, in pipeline order. Each stage ends at the point its label names the start of
  * the next: queued on the event thread, taken by the input loop, its dispatch started, applied to the state, its
  * damage emitted, the render loop waking for it, the frame deadline passing, the frame reading the model, the frame
  * published for painting, the paint starting and the paint ending.
  */
enum LatencyStage(val label: String):
  case EdtToEnqueue   extends LatencyStage("edt_to_enqueue")
  case Queue          extends LatencyStage("queue")
  case Dispatch       extends LatencyStage("dispatch")
  case Apply          extends LatencyStage("apply")
  case Damage         extends LatencyStage("damage")
  case FrameWait      extends LatencyStage("frame_wait")
  case PacingWait     extends LatencyStage("pacing_wait")
  case PreRender      extends LatencyStage("pre_render")
  case Render         extends LatencyStage("render")
  case PublishToPaint extends LatencyStage("publish_to_paint")
  case Paint          extends LatencyStage("paint")

/** A keystroke's monotonic stamps (nanoseconds) up to its damage reaching the render loop. `whenToEdtMs` is the gap
  * between the OS event time (`KeyEvent.getWhen`) and the event thread receiving it, on the wall clock that `getWhen`
  * uses, so it stays outside the monotonic total.
  */
final case class KeyStamps(
    seq: Long,
    whenToEdtMs: Long,
    receivedAt: Long,
    enqueuedAt: Option[Long] = None,
    dequeuedAt: Option[Long] = None,
    applyStartedAt: Option[Long] = None,
    appliedAt: Option[Long] = None,
    damagedAt: Option[Long] = None
)

/** One fast frame's stamps, and the keystrokes whose damage had landed when it read the model. */
final case class FrameStamps(
    wokeAt: Long,
    keys: Vector[KeyStamps] = Vector.empty,
    deadlineAt: Option[Long] = None,
    modelReadAt: Option[Long] = None,
    publishedAt: Option[Long] = None,
    paintStartedAt: Option[Long] = None
)

final case class KeyLatency(seq: Long, whenToEdtMs: Long, stageNanos: Vector[Long], totalNanos: Long):

  def stage(stage: LatencyStage): Long = stageNanos.lift(stage.ordinal).getOrElse(0L)

  def logLine: String =
    val stages = LatencyStage.values.toList.map(stage => s"${stage.label}=${KeyLatency.millis(this.stage(stage))}")
    s"[LATENCY] seq=$seq when_to_edt=$whenToEdtMs ${stages.mkString(" ")} total=${KeyLatency.millis(totalNanos)} ms"

object KeyLatency:
  private val NanosPerMs = 1_000_000.0

  private[diagnostics] def millis(nanos: Long): String = String.format(Locale.ROOT, "%.2f", nanos / NanosPerMs)

  /** Stamps can land out of order across threads (a frame may wake before a keystroke's damage lands, and still read
    * it), so each stage runs from the furthest point reached so far: a stage the keystroke overtook, or never passed
    * through, counts as zero, and the stages always add up to the total.
    */
  def of(key: KeyStamps, frame: FrameStamps, paintFinishedAt: Long): KeyLatency =
    val stageEnds = Vector(
      key.enqueuedAt,
      key.dequeuedAt,
      key.applyStartedAt,
      key.appliedAt,
      key.damagedAt,
      Some(frame.wokeAt),
      frame.deadlineAt,
      frame.modelReadAt,
      frame.publishedAt,
      frame.paintStartedAt,
      Some(paintFinishedAt)
    )
    val (_, stages) = stageEnds.foldLeft((key.receivedAt, Vector.empty[Long])) {
      case ((reached, durations), end) =>
        val next = end.fold(reached)(_.max(reached))
        (next, durations :+ (next - reached))
    }
    KeyLatency(key.seq, key.whenToEdtMs, stages, stages.sum)

/** The keystrokes painted during one report window, and how many were drawn by a frame that changed nothing visible. */
final case class LatencyWindow(elapsedMs: Double, keys: Vector[KeyLatency], unpainted: Int):
  import LatencyWindow.NanosPerMs

  def stats: List[(String, PhaseStats)] =
    if keys.isEmpty then Nil
    else
      ("when_to_edt" -> PhaseStats.of(keys.map(_.whenToEdtMs * NanosPerMs))) ::
        LatencyStage.values.toList.map(stage => stage.label -> PhaseStats.of(keys.map(_.stage(stage)))) :::
        List("total" -> PhaseStats.of(keys.map(_.totalNanos)))

  def summaryLine: String =
    val window = String.format(Locale.ROOT, "%.1f", elapsedMs / 1000.0)
    val parts = stats.map {
      case (label, stats) =>
        String.format(Locale.ROOT, "%s p50=%.2f p95=%.2f", label, stats.p50Ms, stats.p95Ms)
    }
    val body = if parts.isEmpty then "no keystrokes" else s"${parts.mkString(" | ")} ms"
    s"[LATENCY] summary window=${window}s keys=${keys.size} unpainted=$unpainted | $body"

  def logLines: Vector[String] = keys.map(_.logLine) :+ summaryLine

object LatencyWindow:
  private val NanosPerMs = 1_000_000L

/** Keystrokes and frames in flight, plus what completed since the last drain.
  *
  * Keystrokes travel the event thread and the input loop in arrival order, so each stage stamps the oldest keystrokes
  * not yet past it rather than tracking them by identity.
  */
final case class KeyLatencyState(
    windowStartNanos: Long,
    nextSeq: Long,
    keys: Vector[KeyStamps],
    dispatchStartedAt: Option[Long],
    frame: Option[FrameStamps],
    presented: Vector[FrameStamps],
    completed: Vector[KeyLatency],
    unpainted: Int
):
  import KeyLatencyState.*

  def keyReceived(whenToEdtMs: Long, now: Long): KeyLatencyState =
    copy(keys = (keys :+ KeyStamps(nextSeq, whenToEdtMs, now)).takeRight(MaxTrackedKeys), nextSeq = nextSeq + 1)

  def keyEnqueued(now: Long): KeyLatencyState =
    val index = keys.lastIndexWhere(_.enqueuedAt.isEmpty)
    keys.lift(index).fold(this)(key => copy(keys = keys.updated(index, key.copy(enqueuedAt = Some(now)))))

  def keysDequeued(count: Int, now: Long): KeyLatencyState =
    copy(keys = stampOldest(keys, count)(_.dequeuedAt.isEmpty)(_.copy(dequeuedAt = Some(now))))

  def dispatchStarted(now: Long): KeyLatencyState = copy(dispatchStartedAt = Some(now))

  def keysApplied(count: Int, now: Long): KeyLatencyState =
    val startedAt = dispatchStartedAt.getOrElse(now)
    copy(keys =
      stampOldest(keys, count)(_.appliedAt.isEmpty)(_.copy(applyStartedAt = Some(startedAt), appliedAt = Some(now)))
    )

  def damageEmitted(now: Long): KeyLatencyState =
    copy(keys =
      keys.map(key => if key.appliedAt.isDefined && key.damagedAt.isEmpty then key.copy(damagedAt = Some(now)) else key)
    )

  def frameWoke(now: Long): KeyLatencyState = copy(frame = Some(FrameStamps(now)))

  def frameDeadlineReached(now: Long): KeyLatencyState = copy(frame = frame.map(_.copy(deadlineAt = Some(now))))

  /** A frame that picks up no keystroke is dropped here: there is nothing of it to report. */
  def frameModelRead(now: Long): KeyLatencyState =
    val (shown, waiting) = keys.partition(_.damagedAt.isDefined)
    if frame.isEmpty then this
    else if shown.isEmpty then copy(frame = None)
    else copy(keys = waiting, frame = frame.map(_.copy(keys = shown, modelReadAt = Some(now))))

  def framePublished(now: Long): KeyLatencyState =
    copy(frame = frame.map(current => current.copy(publishedAt = current.publishedAt.orElse(Some(now)))))

  def frameEnded: KeyLatencyState =
    frame.fold(this) { current =>
      if current.keys.isEmpty then copy(frame = None)
      else if current.publishedAt.isDefined then
        val kept = (presented :+ current).takeRight(MaxPresentedFrames)
        copy(frame = None, presented = kept, unpainted = unpainted + droppedKeys(presented :+ current, kept))
      else copy(frame = None, unpainted = unpainted + current.keys.size)
    }

  /** A paint shows the latest published image, so it shows every frame published before it started. */
  def paintStarted(now: Long): KeyLatencyState =
    def mark(published: FrameStamps): FrameStamps =
      published.copy(paintStartedAt = published.paintStartedAt.orElse(Some(now)))
    copy(
      presented = presented.map(mark),
      frame = frame.map(current => if current.publishedAt.isDefined then mark(current) else current)
    )

  def paintFinished(now: Long): KeyLatencyState =
    val (painted, waiting) = presented.partition(_.paintStartedAt.isDefined)
    val paintedCurrent     = frame.filter(_.paintStartedAt.isDefined)
    val finished           = (painted ++ paintedCurrent).flatMap(shown => shown.keys.map(KeyLatency.of(_, shown, now)))
    copy(
      presented = waiting,
      frame =
        frame.map(current => if current.paintStartedAt.isDefined then current.copy(keys = Vector.empty) else current),
      completed = (completed ++ finished).takeRight(MaxCompletedKeys)
    )

  def window(now: Long): LatencyWindow = LatencyWindow((now - windowStartNanos) / 1_000_000.0, completed, unpainted)

  def nextWindow(now: Long): KeyLatencyState = copy(windowStartNanos = now, completed = Vector.empty, unpainted = 0)

object KeyLatencyState:
  /** Bounds the trace when a keystroke never reaches a frame, or a frontend never paints. */
  val MaxTrackedKeys     = 1024
  val MaxPresentedFrames = 16
  val MaxCompletedKeys   = 4096

  def start(now: Long): KeyLatencyState =
    KeyLatencyState(now, 0L, Vector.empty, None, None, Vector.empty, Vector.empty, 0)

  private def stampOldest(keys: Vector[KeyStamps], count: Int)(waiting: KeyStamps => Boolean)(
    stamp: KeyStamps => KeyStamps
  ): Vector[KeyStamps] =
    val (_, stamped) = keys.foldLeft((count, Vector.empty[KeyStamps])) {
      case ((left, done), key) =>
        if left > 0 && waiting(key) then (left - 1, done :+ stamp(key)) else (left, done :+ key)
    }
    stamped

  private def droppedKeys(all: Vector[FrameStamps], kept: Vector[FrameStamps]): Int =
    all.map(_.keys.size).sum - kept.map(_.keys.size).sum

/** Per-keystroke latency tracing (`ui.render.latency_trace`), recorded from whichever thread reaches each stage. Every
  * hook is a single flag read while tracing is off, so the pipeline behaves and allocates exactly as without it.
  */
final class KeyLatencyTrace(
    clock: () => Long = () => System.nanoTime(),
    wallClock: () => Long = () => System.currentTimeMillis()
):
  private val enabled = new AtomicBoolean(false)
  private val state   = new AtomicReference(KeyLatencyState.start(clock()))

  def isEnabled: Boolean = enabled.get()

  /** Starts from an empty trace either way, so nothing half-recorded before a toggle is reported after it. */
  def setEnabled(on: Boolean): Unit =
    state.set(KeyLatencyState.start(clock()))
    enabled.set(on)

  private def update(transition: (KeyLatencyState, Long) => KeyLatencyState): Unit =
    val now = clock()
    val _   = state.updateAndGet(transition(_, now))

  /** `whenMillis` is the event's `KeyEvent.getWhen`. */
  def keyReceived(whenMillis: Long): Unit =
    if enabled.get() then
      val whenToEdt = wallClock() - whenMillis
      update(_.keyReceived(whenToEdt, _))

  def keyEnqueued(): Unit            = if enabled.get() then update(_.keyEnqueued(_))
  def keysDequeued(count: Int): Unit = if enabled.get() then update(_.keysDequeued(count, _))
  def dispatchStarted(): Unit        = if enabled.get() then update(_.dispatchStarted(_))
  def keysApplied(count: Int): Unit  = if enabled.get() then update(_.keysApplied(count, _))
  def damageEmitted(): Unit          = if enabled.get() then update(_.damageEmitted(_))
  def frameWoke(): Unit              = if enabled.get() then update(_.frameWoke(_))
  def frameDeadlineReached(): Unit   = if enabled.get() then update(_.frameDeadlineReached(_))
  def frameModelRead(): Unit         = if enabled.get() then update(_.frameModelRead(_))
  def framePublished(): Unit         = if enabled.get() then update(_.framePublished(_))
  def frameEnded(): Unit             = if enabled.get() then update((current, _) => current.frameEnded)
  def paintStarted(): Unit           = if enabled.get() then update(_.paintStarted(_))
  def paintFinished(): Unit          = if enabled.get() then update(_.paintFinished(_))

  private[serenity] def pendingKeys: Vector[KeyStamps] = state.get().keys

  /** The keystrokes painted since the previous drain, starting a new window. */
  def drain(): LatencyWindow =
    val now      = clock()
    val previous = state.getAndUpdate(_.nextWindow(now))
    previous.window(now)
