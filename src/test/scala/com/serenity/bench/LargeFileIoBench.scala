package com.serenity.bench

import java.lang.management.{ManagementFactory, MemoryType}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import cats.effect.unsafe.implicits.global
import com.serenity.io.FileManager
import com.serenity.rope.Balance
import com.serenity.state.models.BufferId

/** Opens and saves a generated file of the given size in megabytes and prints wall time, bytes allocated by the calling
  * thread and the peak heap. Run with `-Xmx2g`: `LargeFileIoBench <megabytes> [crlf]`.
  */
object LargeFileIoBench:

  private given Balance = Balance.default

  final private case class Measurement(millis: Long, allocatedBytes: Long, peakBytes: Long):
    def describe: String = s"$millis ms, allocated ${allocatedBytes >> 20} MB, peak heap ${peakBytes >> 20} MB"

  def main(args: Array[String]): Unit =
    val megabytes  = args.headOption.flatMap(_.toIntOption).getOrElse(50)
    val terminator = if args.contains("crlf") then "\r\n" else "\n"
    val path       = Files.createTempFile("serenity-large", ".txt")
    try
      writeSample(path, megabytes.toLong * 1024 * 1024, terminator)
      val manager        = new FileManager()
      val (buffer, load) = measured(manager.loadFile(path, BufferId(1)).unsafeRunSync())
      val (_, save)      = measured(manager.saveBuffer(buffer, path).unsafeRunSync())
      println(s"size=${megabytes}MB terminator=${terminator.length}")
      println(s"open: ${load.describe}")
      println(s"save: ${save.describe}")
    finally Files.deleteIfExists(path)

  private def measured[A](run: => A): (A, Measurement) =
    val threads = ManagementFactory.getThreadMXBean.asInstanceOf[com.sun.management.ThreadMXBean]
    System.gc()
    val pools = ManagementFactory.getMemoryPoolMXBeans.asScala.filter(_.getType == MemoryType.HEAP).toList
    pools.foreach(_.resetPeakUsage())
    val before    = allocatedByThread(threads)
    val started   = System.nanoTime()
    val result    = run
    val millis    = (System.nanoTime() - started) / 1_000_000
    val allocated = allocatedByThread(threads).map((id, bytes) => bytes - before.getOrElse(id, 0L)).sum
    (result, Measurement(millis, allocated, pools.map(_.getPeakUsage.getUsed).sum))

  private def allocatedByThread(threads: com.sun.management.ThreadMXBean): Map[Long, Long] =
    val ids = threads.getAllThreadIds
    ids.zip(threads.getThreadAllocatedBytes(ids)).toMap

  private def writeSample(path: Path, bytes: Long, terminator: String): Unit =
    val line  = ("The quick brown fox jumps over the lazy dog, café naïve. " * 2).trim + terminator
    val chunk = (line * 1000).getBytes(StandardCharsets.UTF_8)
    val out   = Files.newOutputStream(path)
    try (0L until bytes / chunk.length).foreach(_ => out.write(chunk))
    finally out.close()
