package com.serenity.ui.fonts

import java.util.LinkedHashMap

/** What a chain of fonts is known to cover, kept as 256-codepoint pages of two bitsets (asked, covered) in an
  * access-ordered LRU of at most `maxPages` pages. Text asks about a handful of scripts, so a few pages hold nearly
  * every question, and the memory stays bounded however much text passes through. One instance belongs to the coverage
  * that owns it; nothing here is shared between owners.
  */
final private[fonts] class GlyphCoveragePages(maxPages: Int):
  import GlyphCoveragePages.*

  private val PageBits = 8
  private val PageSize = 1 << PageBits
  private val Words    = PageSize / java.lang.Long.SIZE

  final private class Page:
    val asked: Array[Long]   = new Array[Long](Words)
    val covered: Array[Long] = new Array[Long](Words)

  private val pages = new LinkedHashMap[Int, Page](16, 0.75f, true):
    override def removeEldestEntry(eldest: java.util.Map.Entry[Int, Page]): Boolean = size() > maxPages

  /** Whether `font` covers `codePoint` in `slot`; the font is asked only when the page has not been asked. The font is
    * asked outside the lock, since a composite font loads its components on first use: two threads may ask the same
    * question, and both get the same answer. A page already asked answers without allocating.
    */
  def covers(slot: Int, codePoint: Int, font: java.awt.Font): Boolean =
    val key    = (slot << 13) | (codePoint >> PageBits)
    val offset = codePoint & (PageSize - 1)
    val word   = offset >> 6
    val bit    = 1L << (offset & 63)
    val known = synchronized {
      val page = pages.get(key)
      if page == null || (page.asked(word) & bit) == 0L then Unknown
      else if (page.covered(word) & bit) != 0L then Yes
      else No
    }
    if known != Unknown then known == Yes
    else
      val answer = font.canDisplay(codePoint)
      synchronized {
        val page = Option(pages.get(key)).getOrElse {
          val fresh = new Page
          val _     = pages.put(key, fresh)
          fresh
        }
        page.asked(word) |= bit
        if answer then page.covered(word) |= bit
      }
      answer

  def pageCount: Int = synchronized(pages.size)

object GlyphCoveragePages:
  private val Unknown = 0
  private val No      = 1
  private val Yes     = 2
