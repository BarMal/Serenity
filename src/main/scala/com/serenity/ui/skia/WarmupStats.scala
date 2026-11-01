package com.serenity.ui.skia

import java.util.concurrent.atomic.AtomicLong

/** What the first use of Skia text costs one owner's caches: the faces built, the shaper made and the text shaped, each
  * as a count and the nanoseconds it took. The render benchmark reads it to show where a cold frame's time goes.
  */
final class WarmupStats:

  private val faces        = new AtomicLong(0L)
  private val faceNanos    = new AtomicLong(0L)
  private val shapers      = new AtomicLong(0L)
  private val shaperNanos  = new AtomicLong(0L)
  private val shapings     = new AtomicLong(0L)
  private val shapingNanos = new AtomicLong(0L)

  def faceBuilt(nanos: Long): Unit =
    val _ = faces.incrementAndGet()
    val _ = faceNanos.addAndGet(nanos)

  def shaperMade(nanos: Long): Unit =
    val _ = shapers.incrementAndGet()
    val _ = shaperNanos.addAndGet(nanos)

  def shaped(nanos: Long): Unit =
    val _ = shapings.incrementAndGet()
    val _ = shapingNanos.addAndGet(nanos)

  def snapshot: WarmupStats.Snapshot =
    WarmupStats.Snapshot(
      faces.get.toInt,
      faceNanos.get,
      shapers.get.toInt,
      shaperNanos.get,
      shapings.get,
      shapingNanos.get
    )

object WarmupStats:

  final case class Snapshot(
      faces: Int,
      faceNanos: Long,
      shapers: Int,
      shaperNanos: Long,
      shapings: Long,
      shapingNanos: Long
  ):

    def +(other: Snapshot): Snapshot =
      Snapshot(
        faces + other.faces,
        faceNanos + other.faceNanos,
        shapers + other.shapers,
        shaperNanos + other.shaperNanos,
        shapings + other.shapings,
        shapingNanos + other.shapingNanos
      )

  object Snapshot:
    def empty: Snapshot = Snapshot(0, 0L, 0, 0L, 0L, 0L)
