package com.serenity.ui.renderer

import java.awt.Font
import java.util.concurrent.atomic.{AtomicInteger, AtomicLong}

import com.serenity.ui.layout.{BoundedRefCache, LayoutMemos, TextLayoutSnapshot}

/** How far a string advances in a font, measured once and remembered.
  *
  * Overlay rows size their background and place their caret from the advance of their text, which the measurer of the
  * owner of `memos` measures (the cell grid without one). A modal repaints the same rows every frame, so the
  * measurement is keyed by exactly what determines it -- text and font -- and a repeat is a lookup. The cache holds at
  * most `capacity` advances, because the texts typed into a search or prompt box are unbounded.
  */
final class MeasuredTextAdvances private[serenity] (capacity: Int, memos: LayoutMemos):
  import MeasuredTextAdvances.*

  private val cache     = new BoundedRefCache[Key, Float](new AtomicInteger(capacity))
  private val measuredN = AtomicLong(0)

  /** How many advances were measured, as opposed to served from the cache. */
  def measurements: Long = measuredN.get()

  def advanceOf(text: String, font: Font): Float =
    val key = Key(text, font)
    cache.get(key).getOrElse {
      val _       = measuredN.incrementAndGet()
      val advance = TextLayoutSnapshot.caretXsForText(text, font, memos).lastOption.getOrElse(0.0f)
      cache.put(key, advance)
      advance
    }

object MeasuredTextAdvances:

  final private case class Key(text: String, font: Font)

  val DefaultCapacity: Int = 512
