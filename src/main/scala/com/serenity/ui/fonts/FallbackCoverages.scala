package com.serenity.ui.fonts

import java.awt.Font
import java.util.LinkedHashMap

/** The [[Java2DGlyphCoverage]] of each primary font and fallback chain recently laid out, so a chain's coverage memo
  * outlives the line that asked about it. Bounded and owned by whoever creates it (one per render cache owner); there is
  * no shared instance.
  */
final class FallbackCoverages(maxEntries: Int):

  final private case class Key(primary: Font, fallback: FontFallback)

  private val entries = new LinkedHashMap[Key, Java2DGlyphCoverage](16, 0.75f, true):
    override def removeEldestEntry(eldest: java.util.Map.Entry[Key, Java2DGlyphCoverage]): Boolean =
      size() > maxEntries

  def coverage(primary: Font, fallback: FontFallback): Java2DGlyphCoverage =
    val key = Key(primary, fallback)
    synchronized {
      Option(entries.get(key)).getOrElse {
        val built = Java2DGlyphCoverage.of(primary, fallback)
        val _     = entries.put(key, built)
        built
      }
    }

object FallbackCoverages:

  val DefaultCapacity = 32
