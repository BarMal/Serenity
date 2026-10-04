package com.serenity.spike

import java.lang.management.ManagementFactory
import java.nio.file.{Files, Path}

/** p50/p95/mean of a sample, printed in `TypingProfile`'s `RESULT` shape. */
final case class Summary(count: Int, p50: Double, p95: Double, mean: Double, max: Double)

object Summary:
  def of(samplesMs: Seq[Double]): Option[Summary] =
    Option.when(samplesMs.nonEmpty) {
      val sorted = samplesMs.sorted.toVector
      def pct(p: Double): Double = sorted(math.min(sorted.length - 1, math.ceil(p * sorted.length).toInt - 1).max(0))
      Summary(sorted.length, pct(0.50), pct(0.95), sorted.sum / sorted.length, sorted.last)
    }

object Report:
  /** `RESULT <name> n=<n> ms p50=.. p95=.. mean=.. max=.. [extra]`. */
  def result(name: String, samplesMs: Seq[Double], extra: String = ""): Unit =
    Summary.of(samplesMs) match
      case Some(s) =>
        println(
          f"RESULT $name n=${s.count} ms p50=${s.p50}%.3f p95=${s.p95}%.3f mean=${s.mean}%.3f max=${s.max}%.3f" +
            (if extra.isEmpty then "" else s" $extra")
        )
      case None => println(s"RESULT $name n=0 no samples $extra")

  def note(name: String, text: String): Unit = println(s"RESULT $name $text")

object Timing:
  def ms(action: => Unit): Double =
    val started = System.nanoTime
    action
    (System.nanoTime - started) / 1e6

  private val os = ManagementFactory.getOperatingSystemMXBean

  /** This process's CPU time in nanoseconds, all threads. */
  def processCpuNanos: Long =
    os match
      case bean: com.sun.management.OperatingSystemMXBean => bean.getProcessCpuTime
      case _                                              => -1L

  def parkMs(milliseconds: Long): Unit =
    val deadline = System.nanoTime + milliseconds * 1_000_000L
    while System.nanoTime < deadline do
      java.util.concurrent.locks.LockSupport.parkNanos(deadline - System.nanoTime)

/** The 300-paragraph lorem benchmark document from `bench/gen-lorem.py`, generated if no file is given. */
object LoremDocument:
  def load(path: Option[Path]): Vector[String] =
    val text = path match
      case Some(file) => Files.readString(file)
      case None =>
        val process = ProcessBuilder("python3", "bench/gen-lorem.py").redirectErrorStream(true).start()
        val output  = String(process.getInputStream.readAllBytes(), java.nio.charset.StandardCharsets.UTF_8)
        if process.waitFor() != 0 then sys.error(s"bench/gen-lorem.py failed: $output")
        output
    text.stripTrailing().split("\n", -1).toVector
