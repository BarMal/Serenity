package com.serenity.perf

import java.awt.GraphicsEnvironment
import java.nio.file.Files
import java.time.Duration

import scala.jdk.CollectionConverters.*

import jdk.jfr.Recording
import jdk.jfr.consumer.{RecordedEvent, RecordedFrame, RecordingFile}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Prints where the plain cell-grid frame spends time and allocates, by the project method nearest the sample. Temporary:
  * it profiles the frame with Flight Recorder so the extra cost of the frame can be named.
  */
class ChromeFrameCostDiagnosisSpec extends AnyFlatSpec with Matchers:

  private def projectFrame(event: RecordedEvent): String =
    Option(event.getStackTrace).map(_.getFrames.asScala.toList).getOrElse(Nil)
      .find(frame => frame.getMethod.getType.getName.startsWith("com.serenity") && !frame.getMethod.getType.getName.contains("perf."))
      .fold("(no project frame)")(describe)

  private def describe(frame: RecordedFrame): String =
    s"${frame.getMethod.getType.getName.stripPrefix("com.serenity.")}.${frame.getMethod.getName}"

  private def topFrame(event: RecordedEvent): String =
    Option(event.getStackTrace).map(_.getFrames.asScala.toList).getOrElse(Nil).headOption.fold("(none)")(describe)

  private def profile(name: String, frameRun: () => Any): Unit =
    (1 to 3000).foreach(_ => frameRun())
    val file      = Files.createTempFile("frame", ".jfr")
    val recording = new Recording()
    recording.enable("jdk.ExecutionSample").withPeriod(Duration.ofMillis(1))
    recording.enable("jdk.ObjectAllocationSample").`with`("throttle", "20000/s")
    recording.start()
    (1 to 40000).foreach(_ => frameRun())
    recording.stop()
    recording.dump(file)
    recording.close()
    val events = RecordingFile.readAllEvents(file).asScala.toList
    val cpu    = events.filter(_.getEventType.getName == "jdk.ExecutionSample")
    val alloc  = events.filter(_.getEventType.getName == "jdk.ObjectAllocationSample")
    def table(title: String, keyed: List[(String, Long)], total: Long): Unit =
      println(s"DIAG [$name] $title total=$total")
      keyed.groupMapReduce(_._1)(_._2)(_ + _).toList.sortBy(-_._2).take(18).foreach {
        case (key, weight) => println(f"DIAG [$name]   ${100.0 * weight / math.max(1L, total)}%5.1f%%  $key")
      }
    table("cpu by project method", cpu.map(e => projectFrame(e) -> 1L), cpu.size.toLong)
    table("cpu by top frame", cpu.map(e => topFrame(e) -> 1L), cpu.size.toLong)
    val allocWeights = alloc.map(e => projectFrame(e) -> e.getLong("weight"))
    table("allocation by project method", allocWeights, allocWeights.map(_._2).sum)
    Files.deleteIfExists(file): Unit

  "The chrome frame cost" should "be profiled" in {
    assume(!GraphicsEnvironment.isHeadless, "needs a display")
    val frames = (CellGridBenchmarks.benchmarks() ++ CellGridBenchmarks.chromeVariantBenchmarks())
      .filter(b => b.name == "render.cell_grid.frame_ascii" || b.name.endsWith("flush_square_corners"))
    frames.foreach(frame => profile(frame.name, frame.run))
  }
