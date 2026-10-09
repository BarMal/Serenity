package com.serenity.testkit

import java.util.concurrent.atomic.AtomicBoolean

import scala.annotation.tailrec
import scala.concurrent.duration.{DurationInt, DurationLong, FiniteDuration}

/** Names who stops the global IO runtime when it stops without a trace.
  *
  * Cats Effect's work-stealing pool shuts itself down, silently and without running the runtime's shutdown hook, when a
  * worker finds its own interrupt flag set after parking. Whoever set that flag (a library restoring it after an
  * `InterruptedException`, say) is gone by then, so this samples the compute workers every millisecond: a flag that is
  * set is reported with the stack the worker is running at that moment, and the workers all vanishing is reported
  * together with a dump of every thread.
  */
object RuntimeShutdownWatch:

  /** `interrupted` maps each worker whose interrupt flag is set to the frames it was running; `at` is a monotonic
    * reading of the clock.
    */
  final case class Sample(at: FiniteDuration, workers: Set[String], interrupted: Map[String, List[String]])

  enum Finding:
    case InterruptFlagSet(worker: String, frames: List[String])
    case PoolVanished(workers: Set[String])

  /** What the watcher remembers between samples. */
  final case class State(
      lastWorkers: Set[String],
      emptySince: Option[FiniteDuration],
      vanishReported: Boolean,
      interruptReported: Set[String]
  )

  object State:
    val initial: State = State(Set.empty, None, vanishReported = false, Set.empty)

  trait Watcher:
    def stop(): Unit

  /** Only the global runtime's pool has this prefix: runtimes a test builds use [[OwnedRuntime.ThreadPrefix]]. */
  private val WorkerPrefix  = "io-compute"
  private val BlockerMarker = "-blocker"
  private val Interval      = 1L

  /** A pool whose workers are all momentarily blockers looks empty for a sample or two before replacements start. */
  val VanishWindow: FiniteDuration = 500.millis

  def advance(state: State, current: Sample): (State, List[Finding]) =
    val newlyInterrupted = current.interrupted.toList.sortBy(_._1).collect {
      case (worker, frames) if !state.interruptReported.contains(worker) => Finding.InterruptFlagSet(worker, frames)
    }
    val populated  = current.workers.nonEmpty
    val emptySince = if populated then None else state.emptySince.orElse(Some(current.at))
    val vanished = Option.when(
      !populated && !state.vanishReported && state.lastWorkers.nonEmpty &&
        emptySince.exists(since => current.at - since >= VanishWindow)
    )(Finding.PoolVanished(state.lastWorkers))
    val next = State(
      lastWorkers = if populated then current.workers else state.lastWorkers,
      emptySince = emptySince,
      vanishReported = !populated && (state.vanishReported || vanished.isDefined),
      interruptReported = state.interruptReported ++ current.interrupted.keySet
    )
    (next, newlyInterrupted ::: vanished.toList)

  def render(finding: Finding): String = finding match
    case Finding.InterruptFlagSet(worker, frames) =>
      val stack = frames.map(frame => s"    at $frame")
      (s"[RUNTIME-SHUTDOWN] compute worker $worker has its interrupt flag set; the pool stops itself when the worker " +
        "next parks. It is running:" :: stack).mkString(System.lineSeparator)
    case Finding.PoolVanished(workers) =>
      s"[RUNTIME-SHUTDOWN] every compute worker is gone for over ${VanishWindow.toMillis} ms (${workers.toList.sorted.mkString(", ")}) while the global " +
        "runtime was not shut down through its hook"

  /** Compute workers of the pool named `prefix`, blockers excluded: a worker that becomes a blocker is replaced. */
  def liveSample(prefix: String = WorkerPrefix): Sample =
    val threads =
      liveThreads().filter(thread => thread.getName.startsWith(prefix) && !thread.getName.contains(BlockerMarker))
    val flagged = threads.filter(_.isInterrupted)
    Sample(
      System.nanoTime.nanos,
      threads.map(_.getName).toSet,
      flagged.map(thread => thread.getName -> thread.getStackTrace.toList.map(_.toString)).toMap
    )

  private def liveThreads(): List[Thread] =
    val root  = Iterator.iterate(Thread.currentThread.getThreadGroup)(_.getParent).takeWhile(_ ne null).toList.last
    val slots = new Array[Thread](root.activeCount * 2 + 16)
    slots.take(root.enumerate(slots, true)).toList

  /** Samples until stopped, emitting each finding; the dump of every thread follows a vanished pool. */
  def start(sample: () => Sample, emit: String => Unit): Watcher =
    val stopped = AtomicBoolean(false)
    val thread  = Thread(() => watch(stopped, sample, emit, State.initial), "runtime-shutdown-watch")
    thread.setDaemon(true)
    thread.start()
    () => stopped.set(true)

  @tailrec
  private def watch(stopped: AtomicBoolean, sample: () => Sample, emit: String => Unit, state: State): Unit =
    if !stopped.get then
      val (next, found) = advance(state, sample())
      found.foreach { finding =>
        emit(render(finding))
        finding match
          case Finding.PoolVanished(_)        => emit(HangReport.renderDump(HangReport.allThreadStacks()))
          case Finding.InterruptFlagSet(_, _) => ()
      }
      Thread.sleep(Interval)
      watch(stopped, sample, emit, next)

end RuntimeShutdownWatch
