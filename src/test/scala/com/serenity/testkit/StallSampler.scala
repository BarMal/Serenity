package com.serenity.testkit

import java.lang.management.ManagementFactory
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.StandardOpenOption.{APPEND, CREATE}
import java.nio.file.{Files, Path, Paths}
import java.time.Instant
import java.util.concurrent.atomic.AtomicBoolean

import scala.annotation.tailrec
import scala.concurrent.duration.{Duration, DurationInt, DurationLong, FiniteDuration}
import scala.util.Try

/** Records when the whole test JVM stops running for a while, so a test that overshot its budget can be matched to a
  * stall rather than blamed on its own work.
  *
  * A daemon thread sleeps [[Interval]] and measures how long the sleep really took. A GC pause, a safepoint, CPU
  * starvation or a slow disk all delay it together with every test thread; a lateness above [[Threshold]] is appended
  * to the file named by the [[PathProperty]] system property, stamped with the wall-clock time the thread woke, which
  * is the same clock a JUnit report's `timestamp` and the `-Xlog` decorations use. The property is unset locally, so
  * nothing runs there.
  */
object StallSampler:

  val PathProperty = "serenity.test.stallLog"

  val Interval: FiniteDuration  = 100.millis
  val Threshold: FiniteDuration = 250.millis

  final case class Stall(woke: Instant, jvmUptime: FiniteDuration, lateness: FiniteDuration)

  trait Sampler:
    def stop(): Unit

  /** How far past `interval` a sleep that began at `before` and ended at `after` (monotonic nanoseconds) ran. */
  def lateness(interval: FiniteDuration, before: Long, after: Long): FiniteDuration =
    ((after - before).nanos - interval).max(Duration.Zero)

  def render(stall: Stall): String =
    s"${stall.woke} uptime=${stall.jvmUptime.toMillis}ms late=${stall.lateness.toMillis}ms"

  def header(path: Path, started: Instant): String =
    s"# stall sampler started $started; each line is a wake more than ${Threshold.toMillis} ms late " +
      s"(interval ${Interval.toMillis} ms), stamped with the wake time -> $path"

  def pathFrom(property: Option[String]): Option[Path] =
    property.map(_.trim).filter(_.nonEmpty).map(Paths.get(_))

  /** Starts the sampler once per test JVM when [[PathProperty]] is set. */
  def startFromProperties(): Unit = fromProperties

  private lazy val fromProperties: Option[Sampler] =
    pathFrom(Option(System.getProperty(PathProperty))).map { path =>
      appendLines(path, List(header(path, Instant.now)))
      start(
        Interval,
        Threshold,
        () => System.nanoTime,
        millis => Thread.sleep(millis),
        stall => appendLines(path, List(render(stall)))
      )
    }

  def start(
    interval: FiniteDuration,
    threshold: FiniteDuration,
    nanoTime: () => Long,
    sleep: Long => Unit,
    record: Stall => Unit
  ): Sampler =
    val stopped = AtomicBoolean(false)
    val thread  = Thread(() => watch(stopped, interval, threshold, nanoTime, sleep, record), "stall-sampler")
    thread.setDaemon(true)
    thread.start()
    () => stopped.set(true)

  @tailrec
  private def watch(
    stopped: AtomicBoolean,
    interval: FiniteDuration,
    threshold: FiniteDuration,
    nanoTime: () => Long,
    sleep: Long => Unit,
    record: Stall => Unit
  ): Unit =
    if !stopped.get then
      val before = nanoTime()
      sleep(interval.toMillis)
      val late = lateness(interval, before, nanoTime())
      if late > threshold && !stopped.get then
        record(Stall(Instant.now, ManagementFactory.getRuntimeMXBean.getUptime.millis, late))
      watch(stopped, interval, threshold, nanoTime, sleep, record)

  /** A diagnostic must never fail a test run, so an unwritable file only loses the line. */
  private def appendLines(path: Path, lines: List[String]): Unit =
    val _ = Try {
      Option(path.toAbsolutePath.getParent).foreach(Files.createDirectories(_))
      Files.write(path, lines.map(_ + System.lineSeparator).mkString.getBytes(UTF_8), CREATE, APPEND)
    }

end StallSampler
