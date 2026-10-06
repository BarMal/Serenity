package com.serenity.testkit

import java.lang.management.ManagementFactory

import org.scalatest.Reporter
import org.scalatest.events.{AlertProvided, Event}

/** What a hung suite looks like from outside: the slowpoke alert naming the test, and the stacks of the threads running
  * suites, so the log says where the test is blocked and not only which one it is.
  */
object HangReport:

  final case class ThreadStack(name: String, frames: List[String])

  private val SuiteThreadMarker = "ScalaTest-running-"
  private val MaxFrames         = 40

  def render(alert: String, stacks: List[ThreadStack], deadlocked: List[String]): String =
    val deadlock =
      if deadlocked.isEmpty then Nil
      else List(s"  JVM-detected deadlock between: ${deadlocked.mkString(", ")}")
    val threads = stacks.flatMap(stack => s"  thread ${stack.name}" :: ownFrames(stack).map(f => s"    at $f"))
    ("[HANG-REPORT] " + alert :: deadlock ::: threads).mkString("\n")

  /** The frames down to the point ScalaTest took over: below it is the same runner plumbing for every test. */
  private def ownFrames(stack: ThreadStack): List[String] =
    stack.frames.takeWhile(!_.contains("org.scalatest.")).take(MaxFrames)

  def suiteThreadStacks(): List[ThreadStack] =
    import scala.jdk.CollectionConverters.*
    Thread.getAllStackTraces.asScala.toList
      .collect {
        case (thread, frames) if thread.getName.contains(SuiteThreadMarker) =>
          ThreadStack(thread.getName, frames.toList.map(_.toString))
      }
      .sortBy(_.name)

  def deadlockedThreadNames(): List[String] =
    Option(ManagementFactory.getThreadMXBean.findDeadlockedThreads())
      .map(ids => ManagementFactory.getThreadMXBean.getThreadInfo(ids).toList.map(_.getThreadName))
      .getOrElse(Nil)

end HangReport

/** Prints every alert (the slowpoke detector's "test running for N seconds" included) straight to stdout, with the
  * stacks of the running suites. CI's runner shows only the aggregate counts once a run ends, so the console reporter's
  * alerts never reach the log of a run that hangs; this one does.
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
    () => HangReport.suiteThreadStacks(),
    () => HangReport.deadlockedThreadNames()
  )

  override def apply(event: Event): Unit = event match
    case alert: AlertProvided => emit(HangReport.render(alert.message, stacks(), deadlocked()))
    case _                    => ()
