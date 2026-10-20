package com.serenity.ui.renderer

import java.awt.Font
import java.awt.font.FontRenderContext
import java.util.concurrent.atomic.{AtomicInteger, AtomicLong}

import com.serenity.ui.layout.TextLayoutSnapshot

/** How far a string advances in a font, measured once and remembered.
  *
  * Overlay rows size their background and place their caret from the advance of their text, which a `TextLayout`
  * measures. A modal repaints the same rows every frame, so the measurement is keyed by exactly what determines it --
  * text, font and render context -- and a repeat is a lookup. The cache holds at most `capacity` advances, because the
  * texts typed into a search or prompt box are unbounded. Measuring shapes through `shapedText`, so the painter that
  * shares it finds the text already shaped.
  */
final class MeasuredTextAdvances(capacity: Int, shapedText: ShapedTextCache):
  import MeasuredTextAdvances.*

  private val cache     = new BoundedRefCache[Key, Float](new AtomicInteger(capacity))
  private val measuredN = AtomicLong(0)

  /** How many advances were measured, as opposed to served from the cache. */
  def measurements: Long = measuredN.get()

  def advanceOf(text: String, font: Font, fontRenderContext: FontRenderContext): Float =
    val key = Key(text, font, fontRenderContext)
    cache.get(key).getOrElse {
      val _ = measuredN.incrementAndGet()
      val advance =
        TextLayoutSnapshot.caretXsForText(text, font, shapedText, fontRenderContext).lastOption.getOrElse(0.0f)
      cache.put(key, advance)
      advance
    }

object MeasuredTextAdvances:

  final private case class Key(text: String, font: Font, fontRenderContext: FontRenderContext)

  val DefaultCapacity: Int = 512
