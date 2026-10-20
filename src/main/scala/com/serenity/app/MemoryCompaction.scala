package com.serenity.app

import java.lang.management.ManagementFactory

import scala.util.control.NonFatal

import cats.effect.IO
import cats.syntax.all.*
import com.sun.management.HotSpotDiagnosticMXBean
import org.typelevel.log4cats.Logger

/** `performance.memory.compact`: makes the JVM hand unused heap back to the operating system sooner, by setting
  * manageable HotSpot flags at startup. Measured at about 317 MiB idle against 520 MiB under the shipped launch flags
  * (`docs/memory-footprint.md`, PR #2266).
  */
object MemoryCompaction:

  enum Collector:
    case G1, Serial, Other

  final case class VmOption(name: String, value: String)

  /** The JVM runtime this acts on, so the decision and the order of calls can be tested without touching a real one. */
  trait Jvm:
    def collector: IO[Either[String, Collector]]
    def set(option: VmOption): IO[Either[String, Unit]]

  private val MinHeapFreeRatio     = VmOption("MinHeapFreeRatio", "10")
  private val MaxHeapFreeRatio     = VmOption("MaxHeapFreeRatio", "30")
  private val G1PeriodicGCInterval = VmOption("G1PeriodicGCInterval", "30000")

  /** The options to set for the collector in use, in the order to set them.
    *
    * The JVM refuses a minimum above the current maximum, so the minimum goes first: from the defaults of 40 and 70, 10
    * and then 30 are each accepted, whereas 30 first would fall below the minimum of 40. The periodic interval is a G1
    * flag and means nothing to another collector, and a collector that does not size its heap by free ratios (ZGC,
    * Shenandoah, Epsilon, Parallel) gets nothing.
    */
  def plan(enabled: Boolean, collector: Collector): List[VmOption] =
    if !enabled then Nil
    else
      collector match
        case Collector.G1     => List(MinHeapFreeRatio, MaxHeapFreeRatio, G1PeriodicGCInterval)
        case Collector.Serial => List(MinHeapFreeRatio, MaxHeapFreeRatio)
        case Collector.Other  => Nil

  def apply(enabled: Boolean, jvm: Jvm = Jvm.hotSpot)(using logger: Logger[IO]): IO[Unit] =
    if !enabled then logger.info("[MEMORY] performance.memory.compact is off; the JVM keeps its launch heap options")
    else
      jvm.collector.flatMap {
        case Left(reason) => logger.warn(s"[MEMORY] Heap options not applied: $reason")
        case Right(collector) =>
          plan(enabled, collector) match
            case Nil => logger.info(s"[MEMORY] No heap options apply to the $collector collector")
            case options =>
              options.traverse(option => jvm.set(option).map(option -> _)).flatMap { results =>
                val applied = results.collect { case (option, Right(_)) => s"${option.name}=${option.value}" }
                val refused = results.collect { case (option, Left(reason)) => s"${option.name}: $reason" }
                logger.info(s"[MEMORY] Applied for the $collector collector: ${applied.mkString(", ")}") >>
                  refused.traverse_(reason => logger.warn(s"[MEMORY] Not applied, $reason"))
              }
      }

  object Jvm:

    /** HotSpot's diagnostic bean. It lives in `jdk.management`, which a trimmed runtime image may lack, so a missing
      * class is reported as unavailable rather than left to end the launch.
      */
    val hotSpot: Jvm = new Jvm:

      def collector: IO[Either[String, Collector]] =
        IO.blocking(guarded {
          val bean = diagnostics
          if isOn(bean, "UseG1GC") then Collector.G1
          else if isOn(bean, "UseSerialGC") then Collector.Serial
          else Collector.Other
        })

      def set(option: VmOption): IO[Either[String, Unit]] =
        IO.blocking(guarded(diagnostics.setVMOption(option.name, option.value)))

      private def diagnostics: HotSpotDiagnosticMXBean =
        ManagementFactory.getPlatformMXBean(classOf[HotSpotDiagnosticMXBean])

      private def isOn(bean: HotSpotDiagnosticMXBean, flag: String): Boolean =
        bean.getVMOption(flag).getValue == "true"

      // The boundary with the JDK management API, which signals refusal and absence by throwing; a LinkageError is not
      // NonFatal, so it is named.
      private def guarded[A](action: => A): Either[String, A] =
        try Right(action)
        catch
          case NonFatal(error)     => Left(error.toString)
          case error: LinkageError => Left(error.toString)
