package com.serenity.app

import java.lang.management.ManagementFactory

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import cats.syntax.all.*
import com.serenity.app.MemoryCompaction.{Collector, Jvm, VmOption}
import com.sun.management.HotSpotDiagnosticMXBean
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{Logger, LoggerFactory, LoggerName}

/** Returning unused heap to the OS: which JVM options to set for the collector in use, in an order the JVM accepts. */
class MemoryCompactionSpec extends AnyFlatSpec with Matchers:

  private given LoggerFactory[IO] = Slf4jFactory.create[IO]
  private given Logger[IO]        = LoggerFactory[IO].getLogger(using LoggerName("MemoryCompactionSpec"))

  private def named(options: List[VmOption]): List[(String, String)] =
    options.map(option => option.name -> option.value)

  /** A JVM that enforces the one rule that makes ordering matter: the minimum free ratio may not exceed the maximum. */
  final private class FakeJvm(
      collectorOf: Either[String, Collector],
      state: Ref[IO, Map[String, Int]],
      applied: Ref[IO, List[VmOption]],
      rejecting: Set[String] = Set.empty
  ) extends Jvm:
    val collector: IO[Either[String, Collector]] = IO.pure(collectorOf)

    def set(option: VmOption): IO[Either[String, Unit]] =
      state.get.flatMap { current =>
        val proposed = current.updated(option.name, option.value.toInt)
        val minimum  = proposed("MinHeapFreeRatio")
        val maximum  = proposed("MaxHeapFreeRatio")
        if rejecting(option.name) then IO.pure(Left(s"${option.name} is read-only"))
        else if minimum > maximum then IO.pure(Left(s"MinHeapFreeRatio ($minimum) exceeds MaxHeapFreeRatio ($maximum)"))
        else state.set(proposed) >> applied.update(_ :+ option).as(Right(()))
      }

  private val launchDefaults = Map("MinHeapFreeRatio" -> 40, "MaxHeapFreeRatio" -> 70)

  private def runAgainst(
    enabled: Boolean,
    collector: Either[String, Collector],
    rejecting: Set[String] = Set.empty
  ): (List[VmOption], Map[String, Int]) =
    val program = for
      state   <- Ref.of[IO, Map[String, Int]](launchDefaults)
      applied <- Ref.of[IO, List[VmOption]](Nil)
      _       <- MemoryCompaction.apply(enabled, FakeJvm(collector, state, applied, rejecting))
      options <- applied.get
      values  <- state.get
    yield (options, values)
    program.unsafeRunSync()

  "MemoryCompaction.plan" should "set both free ratios and the periodic interval under G1" in {
    named(MemoryCompaction.plan(enabled = true, Collector.G1)) shouldBe List(
      "MinHeapFreeRatio"     -> "10",
      "MaxHeapFreeRatio"     -> "30",
      "G1PeriodicGCInterval" -> "30000"
    )
  }

  it should "leave the periodic interval out for a collector that is not G1" in {
    named(MemoryCompaction.plan(enabled = true, Collector.Serial)) shouldBe List(
      "MinHeapFreeRatio" -> "10",
      "MaxHeapFreeRatio" -> "30"
    )
  }

  it should "set nothing for a collector whose heap sizing the ratios do not govern" in {
    MemoryCompaction.plan(enabled = true, Collector.Other) shouldBe Nil
  }

  it should "set nothing at all when the setting is off, whatever the collector" in {
    Collector.values.toList.map(MemoryCompaction.plan(enabled = false, _)) shouldBe List.fill(Collector.values.length)(
      Nil
    )
  }

  it should "lower the minimum before the maximum, so each call is accepted from the JVM defaults of 40 and 70" in {
    val names = MemoryCompaction.plan(enabled = true, Collector.G1).map(_.name)
    names.indexOf("MinHeapFreeRatio") should be < names.indexOf("MaxHeapFreeRatio")
  }

  "MemoryCompaction.apply" should "apply every option from the JVM defaults without the JVM rejecting one" in {
    val (options, values) = runAgainst(enabled = true, Right(Collector.G1))

    named(options).map(_._1) shouldBe List("MinHeapFreeRatio", "MaxHeapFreeRatio", "G1PeriodicGCInterval")
    values("MinHeapFreeRatio") shouldBe 10
    values("MaxHeapFreeRatio") shouldBe 30
  }

  it should "touch nothing when the setting is off" in {
    val (options, values) = runAgainst(enabled = false, Right(Collector.G1))

    options shouldBe Nil
    values shouldBe launchDefaults
  }

  it should "apply only the ratios under a collector that is not G1" in {
    val (options, _) = runAgainst(enabled = true, Right(Collector.Serial))

    named(options).map(_._1) shouldBe List("MinHeapFreeRatio", "MaxHeapFreeRatio")
  }

  it should "carry on past an option the JVM refuses, applying the rest" in {
    val (options, _) = runAgainst(enabled = true, Right(Collector.G1), rejecting = Set("G1PeriodicGCInterval"))

    named(options).map(_._1) shouldBe List("MinHeapFreeRatio", "MaxHeapFreeRatio")
  }

  it should "apply nothing, and not fail, when the collector cannot be determined" in {
    val (options, values) = runAgainst(enabled = true, Left("not a HotSpot JVM"))

    options shouldBe Nil
    values shouldBe launchDefaults
  }

  "the flags" should "all be writeable at runtime on this JVM" in {
    val diagnostics = ManagementFactory.getPlatformMXBean(classOf[HotSpotDiagnosticMXBean])
    val writeable = MemoryCompaction
      .plan(enabled = true, Collector.G1)
      .map(o => o.name -> diagnostics.getVMOption(o.name).isWriteable)

    writeable shouldBe MemoryCompaction.plan(enabled = true, Collector.G1).map(_.name -> true)
  }

  "the JVM" should "accept the options and read them back, then get its launch values back" in {
    val diagnostics                   = ManagementFactory.getPlatformMXBean(classOf[HotSpotDiagnosticMXBean])
    def current(name: String): String = diagnostics.getVMOption(name).getValue

    val collector = Jvm.hotSpot.collector.unsafeRunSync()
    assume(collector.isRight, s"collector not determinable: $collector")
    val plan     = MemoryCompaction.plan(enabled = true, collector.toOption.get)
    val launched = plan.map(option => VmOption(option.name, current(option.name)))

    try
      MemoryCompaction.apply(enabled = true, Jvm.hotSpot).unsafeRunSync()
      plan.map(option => option.name -> current(option.name)) shouldBe named(plan)
    finally
      // Restoring the maximum first would be rejected whenever it lies below the minimum just set, so the ratios go
      // back in the opposite order to the way they went out.
      launched.reverse.traverse_(option => Jvm.hotSpot.set(option)).unsafeRunSync()

    plan.map(option => option.name -> current(option.name)) shouldBe named(launched)
  }
