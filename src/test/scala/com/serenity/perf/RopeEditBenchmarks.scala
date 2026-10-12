package com.serenity.perf

import java.lang.management.ManagementFactory

import scala.annotation.tailrec
import scala.jdk.CollectionConverters.*
import scala.util.Random

import com.serenity.perf.RopeEditStatistics.Sample
import com.serenity.rope.{Balance, Rope, RopeRebuilds}

/** Per-edit latency of the rope on a 10 MB document (#1900), through its public API only so that a replacement tree can
  * be measured by the same code. Each scenario makes [[Edits]] timed edits on a fresh rope and attributes every edit
  * over a millisecond to a subtree rebuild or a garbage collection that happened during it.
  *
  * Range delete and large insert would otherwise consume or balloon the document, so each timed edit is followed by an
  * untimed edit that restores the size. Run with `-Xmx2g`:
  * `sbt "Test/runMain com.serenity.perf.RopeEditBenchmarks [passes] [scenario...]"`. The first pass includes JIT
  * warm-up; later passes show the same scenarios warm.
  */
private[perf] object RopeEditBenchmarks:

  given Balance = Balance.default

  val DocumentChars = 10_000_000
  val Edits         = 20_000

  private val MaxSpan        = 10_000
  private val LargeInsertMin = 10_000
  private val LargeInsertMax = 100_000
  private val Middle         = DocumentChars / 2

  /** The timed edit, and the untimed edit that undoes its effect on the document's size. */
  final private case class Step(timed: () => Rope, restore: Rope => Rope)

  final private case class Scenario(name: String, step: (Rope, Int, Random) => Step)

  private def must(edit: Option[Rope]): Rope = edit.getOrElse(sys.error("rope edit rejected an in-range position"))

  private def typed(random: Random): String = random.alphanumeric.head.toString

  private def text(random: Random, length: Int): String =
    random.alphanumeric.take(length).mkString.grouped(80).mkString("\n")

  private val scenarios: Vector[Scenario] = Vector(
    Scenario(
      "random insert",
      (rope, _, random) => Step(() => must(rope.insert(random.nextInt(rope.weight), typed(random))), identity)
    ),
    Scenario(
      "random delete",
      (rope, _, random) =>
        val at = random.nextInt(rope.weight - 1)
        Step(() => must(rope.delete(at, at + 1)), identity)
    ),
    Scenario(
      "typing burst",
      (rope, index, random) => Step(() => must(rope.insert(Middle + index, typed(random))), identity)
    ),
    Scenario(
      "append at end",
      (rope, _, random) => Step(() => must(rope.insert(rope.weight, typed(random))), identity)
    ),
    Scenario("prepend", (rope, _, random) => Step(() => must(rope.insert(0, typed(random))), identity)),
    Scenario(
      "backspace burst",
      (rope, index, _) => Step(() => must(rope.delete(Middle - index - 1, Middle - index)), identity)
    ),
    Scenario(
      "range delete 1-10k",
      (rope, _, random) =>
        val length  = 1 + random.nextInt(MaxSpan)
        val start   = random.nextInt(rope.weight - length)
        val removed = rope.sliceString(start, start + length)
        Step(() => must(rope.delete(start, start + length)), after => must(after.insert(start, removed)))
    ),
    Scenario(
      "large insert 10-100k",
      (rope, _, random) =>
        val length   = LargeInsertMin + random.nextInt(LargeInsertMax - LargeInsertMin + 1)
        val at       = random.nextInt(rope.weight)
        val inserted = text(random, length)
        Step(() => must(rope.insert(at, inserted)), after => must(after.delete(at, at + inserted.length)))
    )
  )

  private val collectors = ManagementFactory.getGarbageCollectorMXBeans.asScala.toVector

  private def collections: Long = collectors.map(_.getCollectionCount).filter(_ >= 0).sum

  private def document(random: Random): Rope =
    val lines = Iterator.continually(random.alphanumeric.take(20 + random.nextInt(80)).mkString)
    Rope(lines.take(DocumentChars / 50).mkString("\n").take(DocumentChars))

  private def timed(step: Step): (Rope, Sample) =
    val rebuildsBefore    = RopeRebuilds.totals.rebuilds
    val collectionsBefore = collections
    val started           = System.nanoTime()
    val edited            = step.timed()
    val elapsed           = System.nanoTime() - started
    (
      step.restore(edited),
      Sample(elapsed, RopeRebuilds.totals.rebuilds - rebuildsBefore, collections - collectionsBefore)
    )

  @tailrec
  private def run(
    scenario: Scenario,
    rope: Rope,
    random: Random,
    index: Int,
    samples: Vector[Sample]
  ): (Rope, Vector[Sample]) =
    if index == Edits then (rope, samples)
    else
      val (next, sample) = timed(scenario.step(rope, index, random))
      run(scenario, next, random, index + 1, samples :+ sample)

  private def micros(nanos: Long): String = f"${nanos / 1000.0}%.0f"

  private def report(scenario: Scenario, end: Rope, samples: Vector[Sample], leaves: Long): String =
    val percentiles = RopeEditStatistics.percentiles(samples)
    val spikes      = RopeEditStatistics.spikes(samples)
    val latency     = s"${micros(percentiles.p50)}/${micros(percentiles.p99)}/${micros(percentiles.p999)} us"
    val rebuilds    = s"rebuild-edits ${samples.count(_.rebuilds > 0)} (leaves $leaves)"
    val slow = s">1ms ${spikes.total} (rebuild ${spikes.withRebuild}, gc ${spikes.withCollection}, " +
      s"neither ${spikes.unexplained}, first-100 ${spikes.firstHundredEdits})"
    f"${scenario.name}%-22s $latency%-20s max ${percentiles.max / 1e6}%.1f ms  height ${end.height}%d  $rebuilds  $slow"

  private def runPass(pass: Int, selected: Set[String]): Unit =
    println(s"--- pass $pass (p50/p99/p99.9)")
    scenarios.filter(scenario => selected.isEmpty || selected.contains(scenario.name)).foreach { scenario =>
      val random         = Random(1900)
      val start          = document(random)
      val leavesBefore   = RopeRebuilds.totals.leaves
      val (end, samples) = run(scenario, start, random, 0, Vector.empty)
      println(report(scenario, end, samples, RopeRebuilds.totals.leaves - leavesBefore))
    }

  def main(args: Array[String]): Unit =
    val passes = args.headOption.flatMap(_.toIntOption).getOrElse(2)
    val names  = args.toList.drop(1).toSet
    println(
      s"rope $DocumentChars chars, $Edits edits per scenario, max heap ${Runtime.getRuntime.maxMemory() >> 20} MB"
    )
    (1 to passes).foreach(runPass(_, names))

end RopeEditBenchmarks
