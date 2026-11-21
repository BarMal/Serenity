package com.serenity.perf

import java.util.Locale

import com.serenity.perf.TypingWhileIndexingProfile.Summary

/** One typed key's clock readings, in nanoseconds on SDL's clock (`SDL_GetTicksNS`): the event's own timestamp, the
  * pump reading it, and the swap that showed it returning.
  */
final private[perf] case class KeySwapStamps(event: Long, pickup: Long, shown: Long):

  def isOrdered: Boolean = event <= pickup && pickup <= shown

  def keyToSwapMs: Double   = FrameTimes.ms(event, shown)
  def eventPickupMs: Double = FrameTimes.ms(event, pickup)

/** The rows the SDL3 key-to-swap benchmark prints for a run of keys (#2380). */
final private[perf] case class KeySwapRows(keyToSwap: Summary, eventPickup: Summary, outOfOrder: Int):

  /** One `[sdl3-key]` line per row; `context` says what the run was, such as `swap_interval=1`. */
  def lines(context: String): List[String] =
    val note = if outOfOrder > 0 then s" out_of_order=$outOfOrder" else ""
    List("key_to_swap" -> keyToSwap, "event_pickup" -> eventPickup).map { (name, summary) =>
      KeySwapRows.line(context, name, summary.count, summary.p50Ms, summary.p95Ms, summary.maxMs) + note
    }

private[perf] object KeySwapRows:

  def of(keys: Vector[KeySwapStamps]): KeySwapRows =
    KeySwapRows(
      keyToSwap = Summary.of(keys.map(_.keyToSwapMs)),
      eventPickup = Summary.of(keys.map(_.eventPickupMs)),
      outOfOrder = keys.count(!_.isOrdered)
    )

  /** The line for any row measured over `count` samples, in the style of the job-13 lines. */
  def line(context: String, name: String, count: Int, p50Ms: Double, p95Ms: Double, maxMs: Double): String =
    String.format(
      Locale.ROOT,
      "[sdl3-key] %s %s p50=%.2f p95=%.2f max=%.2f n=%d",
      context,
      name,
      p50Ms,
      p95Ms,
      maxMs,
      count
    )
