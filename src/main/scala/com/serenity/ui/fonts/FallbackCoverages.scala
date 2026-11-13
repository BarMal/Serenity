package com.serenity.ui.fonts

import java.awt.Font
import java.util.LinkedHashMap

/** The [[AwtGlyphCoverage]] of each primary font and fallback chain recently laid out, so a chain's coverage memo
  * outlives the line that asked about it. Bounded and owned by whoever creates it (one per render cache owner); there
  * is no shared instance.
  */
final class FallbackCoverages(maxEntries: Int):

  private val asked = java.util.concurrent.atomic.AtomicLong(0L)

  /** How many times a coverage was asked for: what a hot layout path must not do for a line the primary font covers. */
  def lookups: Long = asked.get()

  final private case class Key(primary: Font, fallback: FontFallback)

  private val entries = new LinkedHashMap[Key, AwtGlyphCoverage](16, 0.75f, true):
    override def removeEldestEntry(eldest: java.util.Map.Entry[Key, AwtGlyphCoverage]): Boolean =
      size() > maxEntries

  def coverage(primary: Font, fallback: FontFallback): AwtGlyphCoverage =
    val _   = asked.incrementAndGet()
    val key = Key(primary, fallback)
    synchronized {
      Option(entries.get(key)).getOrElse {
        val built = AwtGlyphCoverage.of(primary, fallback)
        val _     = entries.put(key, built)
        built
      }
    }

object FallbackCoverages:

  val DefaultCapacity = 32
