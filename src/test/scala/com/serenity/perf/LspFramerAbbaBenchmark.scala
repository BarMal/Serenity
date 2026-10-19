package com.serenity.lsp

import java.lang.management.ManagementFactory

import scala.annotation.tailrec

import com.serenity.lsp.client.LspFramer
import com.serenity.lsp.client.LspFramer.FrameDecoder
import fs2.Chunk
import io.circe.Json
import io.circe.syntax.*

/** Compares the pure `LspFramer.FrameDecoder` (B) with the mutable one it replaced (A), decoder against decoder, with
  * the same pre-built chunks and the same driver shape, so only the decoder differs.
  *
  * ABBA: each cycle runs the slots A B B A A B B A, every slot a batch of whole decodes. Drift in the machine (thermal,
  * GC heap shape, JIT recompiles) then lands on both sides in turn instead of favouring whichever ran second.
  *
  * Run with `sbt "Test/runMain com.serenity.lsp.LspFramerAbbaBenchmark [cycles]"`.
  */
object LspFramerAbbaBenchmark:

  final private case class Scenario(name: String, chunks: Vector[Chunk[Byte]], frames: Int, batch: Int)

  final private case class Slot(side: Char, nanosPerDecode: Double, bytesPerDecode: Double)

  private val Order = "ABBAABBA"

  private val allocation = ManagementFactory.getThreadMXBean match
    case bean: com.sun.management.ThreadMXBean =>
      bean.setThreadAllocatedMemoryEnabled(true)
      Some(bean)
    case _ => None

  private def allocated(): Long =
    allocation.fold(0L)(_.getThreadAllocatedBytes(Thread.currentThread.threadId))

  private def message(id: Int, payload: String): Json =
    Json.obj("jsonrpc" -> "2.0".asJson, "id" -> id.asJson, "method" -> "bench".asJson, "params" -> payload.asJson)

  private def framed(messages: Seq[Json]): Array[Byte] = messages.toArray.flatMap(LspFramer.encode)

  private def chunked(bytes: Array[Byte], size: Int): Vector[Chunk[Byte]] =
    bytes.grouped(size).map(Chunk.array(_)).toVector

  private val scenarios: List[Scenario] =
    List(
      Scenario(
        "large frames: 8 x 1 MiB bodies, 8 KiB reads",
        chunked(framed((1 to 8).map(id => message(id, "y" * (1024 * 1024)))), 8 * 1024),
        8,
        3
      ),
      Scenario(
        "many small frames: 20,000 x ~80 B, 8 KiB reads",
        chunked(framed((1 to 20000).map(id => message(id, s"payload-$id"))), 8 * 1024),
        20000,
        5
      ),
      Scenario(
        "byte at a time: 1,500 x ~80 B, 1-byte reads",
        chunked(framed((1 to 1500).map(id => message(id, s"payload-$id"))), 1),
        1500,
        5
      )
    )

  /** The mutable decoder, driven chunk by chunk. */
  private def legacyDecode(chunks: Vector[Chunk[Byte]]): Int =
    val decoder = LegacyFraming.LegacyFrameDecoder()
    val counted = chunks.foldLeft(0)((count, chunk) => count + decoder.feed(chunk).size)
    decoder.finish()
    counted

  /** The pure decoder, its state threaded from chunk to chunk. */
  private def pureDecode(chunks: Vector[Chunk[Byte]]): Int =
    @tailrec
    def go(index: Int, decoder: FrameDecoder, count: Int): Int =
      if index == chunks.size then decoder.finish.fold(error => throw error, _ => count)
      else
        decoder.feed(chunks(index)) match
          case Right((frames, next)) => go(index + 1, next, count + frames.size)
          case Left(error)           => throw error
    go(0, FrameDecoder.empty, 0)

  private def decode(side: Char, chunks: Vector[Chunk[Byte]]): Int =
    if side == 'A' then legacyDecode(chunks) else pureDecode(chunks)

  private def slot(side: Char, scenario: Scenario): Slot =
    System.gc()
    val bytesBefore = allocated()
    val start       = System.nanoTime()
    val decoded     = (1 to scenario.batch).map(_ => decode(side, scenario.chunks)).sum
    val nanos       = System.nanoTime() - start
    val bytes       = allocated() - bytesBefore
    require(decoded == scenario.frames * scenario.batch, s"$side decoded $decoded frames in ${scenario.name}")
    Slot(side, nanos.toDouble / scenario.batch, bytes.toDouble / scenario.batch)

  private def mean(values: Seq[Double]): Double = values.sum / values.size

  private def sd(values: Seq[Double]): Double =
    val m = mean(values)
    math.sqrt(values.map(v => (v - m) * (v - m)).sum / math.max(1, values.size - 1))

  private def report(scenario: Scenario, slots: Seq[Slot]): String =
    def row(side: Char) =
      val own = slots.filter(_.side == side)
      (
        mean(own.map(_.nanosPerDecode / 1e6)),
        sd(own.map(_.nanosPerDecode / 1e6)),
        mean(own.map(_.bytesPerDecode / 1024))
      )
    val (aMs, aSd, aKb) = row('A')
    val (bMs, bSd, bKb) = row('B')
    f"| ${scenario.name} | $aMs%.2f +- $aSd%.2f | $bMs%.2f +- $bSd%.2f | ${bMs / aMs}%.2fx | $aKb%.0f | $bKb%.0f | ${bKb / aKb}%.2fx |"

  def main(args: Array[String]): Unit =
    val cycles = args.headOption.flatMap(_.toIntOption).getOrElse(4)
    val rows = scenarios.map { scenario =>
      (1 to 6).foreach(_ => Order.foreach(side => slot(side, scenario)))
      val slots = (1 to cycles).flatMap(_ => Order.map(slot(_, scenario)))
      report(scenario, slots)
    }
    println(s"ABBA, $cycles cycles x 8 slots per scenario (${cycles * 4} slots per side), 6 warm-up cycles discarded")
    println("A = mutable decoder, B = pure decoder; time and allocation are per whole-stream decode")
    println()
    println(
      "| Scenario | A ms (mean +- sd) | B ms (mean +- sd) | B / A time | A KiB alloc | B KiB alloc | B / A alloc |"
    )
    println("| --- | --- | --- | --- | --- | --- | --- |")
    rows.foreach(println)
