package com.serenity.testkit

import java.lang.management.{ManagementFactory, ThreadInfo}

import org.scalatest.Reporter
import org.scalatest.events.{AlertProvided, Event}

/** What a hung suite looks like from outside: the slowpoke alert naming the test, the stacks of the threads running
  * suites, and the state of the threads they wait on -- the IO runtime's compute and blocker threads, and any thread
  * blocked, holding a lock or stuck initialising a class or lazy val -- so the log says why the test is blocked and not
  * only where.
  */
object HangReport:

  final case class LockWait(lock: String, owner: Option[String])

  final case class ThreadStack(
      name: String,
      frames: List[String],
      state: String = "",
      waitingOn: Option[LockWait] = None,
      held: List[String] = Nil
  )

  private val SuiteThreadMarker   = "ScalaTest-running-"
  private val RuntimeThreadPrefix = "io-compute"
  private val MaxFrames           = 40

  // Frames below these are plumbing that is the same for every test or every fiber.
  private val PlumbingFrames = List("org.scalatest.", "cats.effect.IOFiber.run")

  // A thread waiting on class or lazy-val initialisation is invisible to findDeadlockedThreads.
  private val InitialisationFrames = List("<clinit>", "scala.runtime.LazyVals")

  def render(alert: String, stacks: List[ThreadStack], deadlocked: List[String]): String =
    val deadlock =
      if deadlocked.isEmpty then Nil
      else List(s"  JVM-detected deadlock between: ${deadlocked.mkString(", ")}")
    val (idle, active) = stacks.partition(isIdle)
    val threads        = active.flatMap(renderThread)
    val idleLine =
      if idle.isEmpty then Nil else List(s"  idle: ${idle.map(_.name).sorted.mkString(", ")}")
    ("[HANG-REPORT] " + alert :: deadlock ::: threads ::: idleLine).mkString("\n")

  private def renderThread(stack: ThreadStack): List[String] =
    val state = if stack.state.isEmpty then "" else s" [${stack.state}]"
    val waiting =
      stack.waitingOn.map(w => s" waiting on ${w.lock}" + w.owner.fold("")(o => s" held by $o")).getOrElse("")
    val holds = if stack.held.isEmpty then Nil else List(s"    holds ${stack.held.mkString(", ")}")
    s"  thread ${stack.name}$state$waiting" :: holds ::: ownFrames(stack).map(f => s"    at $f")

  private def ownFrames(stack: ThreadStack): List[String] =
    stack.frames.takeWhile(frame => !PlumbingFrames.exists(frame.contains)).take(MaxFrames)

  /** A runtime thread parked with nothing of ours on its stack and nothing held: waiting for work, not for a lock. */
  private def isIdle(stack: ThreadStack): Boolean =
    stack.name.startsWith(RuntimeThreadPrefix) && (stack.state == "WAITING" || stack.state == "TIMED_WAITING") &&
      stack.waitingOn.forall(_.owner.isEmpty) && stack.held.isEmpty &&
      !stack.frames.exists(frame => frame.contains("com.serenity") || InitialisationFrames.exists(frame.contains))

  def isRelevant(stack: ThreadStack): Boolean =
    stack.name.contains(SuiteThreadMarker) || stack.name.startsWith(RuntimeThreadPrefix) ||
      stack.state == "BLOCKED" || stack.held.nonEmpty || stack.waitingOn.exists(_.owner.isDefined) ||
      stack.frames.exists(frame => InitialisationFrames.exists(frame.contains))

  /** Suite threads first, since the alert names their tests, then everything they might be waiting on. */
  def threadStacks(): List[ThreadStack] =
    val (suites, others) = allThreadStacks().filter(isRelevant).partition(_.name.contains(SuiteThreadMarker))
    suites.sortBy(_.name) ::: others.sortBy(_.name)

  def suiteThreadStacks(): List[ThreadStack] =
    allThreadStacks().filter(_.name.contains(SuiteThreadMarker)).sortBy(_.name)

  private def allThreadStacks(): List[ThreadStack] =
    ManagementFactory.getThreadMXBean.dumpAllThreads(true, true).toList.map(fromThreadInfo)

  private def fromThreadInfo(info: ThreadInfo): ThreadStack =
    ThreadStack(
      info.getThreadName,
      info.getStackTrace.toList.map(_.toString),
      info.getThreadState.toString,
      Option(info.getLockName).map(LockWait(_, Option(info.getLockOwnerName))),
      info.getLockedMonitors.toList.map(_.toString) ::: info.getLockedSynchronizers.toList.map(_.toString)
    )

  def deadlockedThreadNames(): List[String] =
    Option(ManagementFactory.getThreadMXBean.findDeadlockedThreads())
      .map(ids => ManagementFactory.getThreadMXBean.getThreadInfo(ids).toList.map(_.getThreadName))
      .getOrElse(Nil)

end HangReport

/** Prints every alert (the slowpoke detector's "test running for N seconds" included) straight to stdout, with the
  * thread stacks [[HangReport.threadStacks]] chooses. CI's runner shows only the aggregate counts once a run ends, so
  * the console reporter's alerts never reach the log of a run that hangs; this one does.
  */
final class HangReporter(
    emit: String => Unit,
    stacks: () => List[HangReport.ThreadStack],
    deadlocked: () => List[String]
) extends Reporter:

  def this() = this(
    text =>
      System.out.print(text + System.lineSeparator)
      System.out.flush()
    ,
    () => HangReport.threadStacks(),
    () => HangReport.deadlockedThreadNames()
  )

  override def apply(event: Event): Unit = event match
    case alert: AlertProvided => emit(HangReport.render(alert.message, stacks(), deadlocked()))
    case _                    => ()
