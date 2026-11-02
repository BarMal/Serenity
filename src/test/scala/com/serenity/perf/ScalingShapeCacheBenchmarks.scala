package com.serenity.perf

import com.serenity.SkiaFrames
import com.serenity.rope.Balance
import com.serenity.state.manager.RenderCaches
import com.serenity.state.models.BufferId
import com.serenity.ui.skia.{SkiaShapedTextCache, SkiaTextCaches}

/** How the bounded Skia shape cache (two generations of 2048 stretches) serves many open buffers (#2478): the buffers
  * are drawn one after another into one set of render caches, pass after pass, and for every frame the cache's lookups,
  * misses (stretches shaped), older-generation hits and evictions are read as the difference of its counters.
  *
  * Not part of the perf gate, like [[SkiaFrameBenchmarks]]: it prints a table to read. Run it with
  * `sbt "Test / runMain com.serenity.perf.ScalingShapeCacheBenchmarks"`; an optional argument is the number of buffers
  * (default 20). The first pass pays for every stretch once; from the second, a miss is a stretch the cache had
  * dropped.
  */
private[perf] object ScalingShapeCacheBenchmarks:

  given Balance = Balance.default

  /** What one frame did to the cache: the differences of its counters across the frame, and the frame's time. */
  final case class FrameTraffic(lookups: Long, shapings: Long, promotions: Long, evictions: Long, nanos: Long)

  final case class PassSummary(
      pass: Int,
      frames: Int,
      p50Ms: Double,
      lookupsPerFrame: Double,
      missesPerFrame: Double,
      olderHitsPerFrame: Double,
      evictions: Long,
      hitRate: Double
  )

  private val Passes = 4

  private def counters(cache: SkiaShapedTextCache): FrameTraffic =
    FrameTraffic(cache.lookups, cache.shapings, cache.promotions, cache.evictions, 0L)

  def summarise(pass: Int, frames: Vector[FrameTraffic]): PassSummary =
    val count   = frames.size.toDouble
    val lookups = frames.map(_.lookups).sum
    val misses  = frames.map(_.shapings).sum
    PassSummary(
      pass,
      frames.size,
      p50Ms = frames.map(_.nanos).sorted.apply(frames.size / 2) / 1_000_000.0,
      lookupsPerFrame = lookups / count,
      missesPerFrame = misses / count,
      olderHitsPerFrame = frames.map(_.promotions).sum / count,
      evictions = frames.map(_.evictions).sum,
      hitRate = if lookups == 0L then 1.0 else (lookups - misses).toDouble / lookups
    )

  /** The traffic of each frame of each pass, and the counters of the cache at the end. */
  def trace(
    kind: MultiBufferFixture.Kind,
    buffers: Int,
    passes: Int,
    size: MultiBufferFixture.Size
  ): (Vector[Vector[FrameTraffic]], FrameTraffic) =
    val state  = MultiBufferFixture.state(kind, buffers, size)
    val caches = RenderCaches.create()
    val cache  = SkiaTextCaches.of(caches.backendCaches).shapedText
    val traffic = Vector.tabulate(passes) { _ =>
      Vector.tabulate(buffers) { index =>
        val before  = counters(cache)
        val started = System.nanoTime()
        val _       = LaptopColdFrames.of(SkiaFrames, MultiBufferFixture.switchTo(state, BufferId(index + 1)), caches)
        val nanos   = System.nanoTime() - started
        val after   = counters(cache)
        FrameTraffic(
          after.lookups - before.lookups,
          after.shapings - before.shapings,
          after.promotions - before.promotions,
          after.evictions - before.evictions,
          nanos
        )
      }
    }
    (traffic, counters(cache))

  private def report(kind: MultiBufferFixture.Kind, buffers: Int): Unit =
    val (traffic, totals) = trace(kind, buffers, Passes, MultiBufferFixture.FullSize)
    println(
      s"shape cache, $buffers ${kind.label} buffers, one frame each per pass (container figures unless on a laptop)"
    )
    println(
      f"${"pass"}%5s ${"frames"}%7s ${"p50 ms"}%9s ${"lookups"}%9s ${"misses"}%8s ${"older hits"}%11s ${"evicted"}%9s ${"hit rate"}%9s"
    )
    traffic.zipWithIndex.foreach { (frames, index) =>
      val s = summarise(index + 1, frames)
      println(
        f"${s.pass}%5d ${s.frames}%7d ${s.p50Ms}%9.2f ${s.lookupsPerFrame}%9.1f ${s.missesPerFrame}%8.1f " +
          f"${s.olderHitsPerFrame}%11.1f ${s.evictions}%9d ${s.hitRate}%9.3f"
      )
    }
    println(
      s"totals: ${totals.lookups} lookups, ${totals.shapings} shapings, ${totals.promotions} promotions, ${totals.evictions} evictions"
    )

  def main(args: Array[String]): Unit =
    val buffers = args.headOption.flatMap(_.toIntOption).getOrElse(20)
    MultiBufferFixture.Kind.values.foreach(report(_, buffers))
