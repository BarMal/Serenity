package com.serenity.testkit

import java.util.concurrent.atomic.AtomicBoolean

import scala.annotation.tailrec

/** Names who stops the global IO runtime when it stops without a trace.
  *
  * Cats Effect's work-stealing pool shuts itself down, silently and without running the runtime's shutdown hook, when a
  * worker finds its own interrupt flag set after parking. Whoever set that flag (a library restoring it after an
  * `InterruptedException`, say) is gone by then, so this samples the compute workers every millisecond: a flag that is
  * set is reported with the stack the worker is running at that moment, and the workers all vanishing is reported
  * together with a dump of every thread.
  */
object RuntimeShutdownWatch:

  /** `interrupted` maps each worker whose interrupt flag is set to the frames it was running. */
  final case class Sample(workers: Set[String], interrupted: Map[String, List[String]])

  enum Finding:
    case InterruptFlagSet(worker: String, frames: List[String])
    case PoolVanished(workers: Set[String])

  trait Watcher:
    def stop(): Unit

  private val WorkerPrefix  = "io-compute"
  private val BlockerMarker = "-blocker"
  private val Interval      = 1L

  def findings(previous: Sample, current: Sample, alreadyReported: Set[String]): List[Finding] =
    val newlyInterrupted = current.interrupted.toList.sortBy(_._1).collect {
      case (worker, frames) if !alreadyReported.contains(worker) => Finding.InterruptFlagSet(worker, frames)
    }
    val vanished =
      Option.when(previous.workers.nonEmpty && current.workers.isEmpty)(Finding.PoolVanished(previous.workers))
    newlyInterrupted ::: vanished.toList

  def render(finding: Finding): String = finding match
    case Finding.InterruptFlagSet(worker, frames) =>
      val stack = frames.map(frame => s"    at $frame")
      (s"[RUNTIME-SHUTDOWN] compute worker $worker has its interrupt flag set; the pool stops itself when the worker " +
        "next parks. It is running:" :: stack).mkString(System.lineSeparator)
    case Finding.PoolVanished(workers) =>
      s"[RUNTIME-SHUTDOWN] every compute worker is gone (${workers.toList.sorted.mkString(", ")}) while the global " +
        "runtime was not shut down through its hook"

  /** Compute workers of the pool named `prefix`, blockers excluded: a worker that becomes a blocker is replaced. */
  def liveSample(prefix: String = WorkerPrefix): Sample =
    val threads =
      liveThreads().filter(thread => thread.getName.startsWith(prefix) && !thread.getName.contains(BlockerMarker))
    val flagged = threads.filter(_.isInterrupted)
    Sample(
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
    val thread =
      Thread(() => watch(stopped, sample, emit, Sample(Set.empty, Map.empty), Set.empty), "runtime-shutdown-watch")
    thread.setDaemon(true)
    thread.start()
    () => stopped.set(true)

  @tailrec
  private def watch(
    stopped: AtomicBoolean,
    sample: () => Sample,
    emit: String => Unit,
    previous: Sample,
    reported: Set[String]
  ): Unit =
    if !stopped.get then
      val current = sample()
      val found   = findings(previous, current, reported)
      found.foreach { finding =>
        emit(render(finding))
        finding match
          case Finding.PoolVanished(_)        => emit(HangReport.renderDump(HangReport.allThreadStacks()))
          case Finding.InterruptFlagSet(_, _) => ()
      }
      Thread.sleep(Interval)
      watch(stopped, sample, emit, current, reported ++ current.interrupted.keySet)

end RuntimeShutdownWatch
