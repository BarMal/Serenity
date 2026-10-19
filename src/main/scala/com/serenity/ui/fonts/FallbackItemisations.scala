package com.serenity.ui.fonts

import java.awt.Font
import java.util.LinkedHashMap

import com.serenity.text.fallback.{FallbackItemiser, FallbackRun}

/** The fallback runs of each line of text recently laid out, so a line is itemised once however often a layout asks.
  * Layout derives a line's fonts before it looks the line up in the wrap cache, so without this every frame would
  * itemise every line it draws again. Bounded, and owned by whoever creates it (one per render cache owner).
  */
final class FallbackItemisations(coverages: FallbackCoverages, maxEntries: Int):

  final private case class Key(primary: Font, fallback: FontFallback, stickyNeutrals: Boolean, text: String)

  private val entries = new LinkedHashMap[Key, Vector[FallbackRun]](64, 0.75f, true):
    override def removeEldestEntry(eldest: java.util.Map.Entry[Key, Vector[FallbackRun]]): Boolean =
      size() > maxEntries

  def runs(primary: Font, fallback: FontFallback, text: String, stickyNeutrals: Boolean): Vector[FallbackRun] =
    if text.length > FallbackItemisations.MaxMemoisedChars then itemised(primary, fallback, text, stickyNeutrals)
    else
      val key = Key(primary, fallback, stickyNeutrals, text)
      val hit = synchronized(entries.get(key))
      if hit != null then hit
      else
        val found = itemised(primary, fallback, text, stickyNeutrals)
        val _     = synchronized(entries.put(key, found))
        found

  private def itemised(primary: Font, fallback: FontFallback, text: String, stickyNeutrals: Boolean) =
    FallbackItemiser.itemise(text, 0, text.length, coverages.coverage(primary, fallback), stickyNeutrals)

object FallbackItemisations:

  val DefaultEntries = 512

  /** A line this long is itemised each time rather than kept: its text would be the memo's whole budget. */
  val MaxMemoisedChars = 8192
